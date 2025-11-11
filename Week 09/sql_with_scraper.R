library(DBI)
library(httr2)
library(rvest)

# functions ----

# function that takes a formatted response from the processResult function and
# uses it to update the SQL database:
pageUpdater<-function(con, result){
  statement<-glue::glue_sql(.con = con, 
                            "UPDATE pages set 
                             headline = {result$headline},
                             article = {result$article},
                             pubdate = {result$pubdate},
                             metadata = {result$metadata},
                             scraped = TRUE,
                             scraped_time  = {paste(Sys.time())}
                             WHERE links = {result$link}
                            "
  )
  
  
  updates<-dbExecute(con, statement)
  return(updates)
}

# a result processor with some built-in code for handling response errors:
processResult<-function(result){
  if(result$status!=200){
    article_data<-data.frame(link = result$request$url, headline=NA, article=NA, pubdate=NA,metadata=NA)
  }else{
    page_data<-result|>
      resp_body_html()
    headline<-page_data|>
      html_element('.Page-headline')|>
      html_text()
    article<-page_data|>
      html_elements('.RichTextStoryBody p')|>
      html_text()|>
      stringr::str_squish()|>
      stringr::str_c(collapse=' ')
    pubdate<-page_data|>
      html_element('bsp-timestamp')|>
      html_attr('data-timestamp')|>
      as.numeric()
    pubdate<-as.POSIXct(pubdate/1000,
                        origin = "1970-01-01", tz = "UTC"
    )|>paste0()
    metadata<-page_data|>
      html_element("[name = 'gtm-dataLayer']")|>
      html_attr('content')
    article_data<-data.frame(link = result$request$url, headline, article, pubdate, metadata)
  }
  return(article_data)
}

# unused example of a regular expression filter function that would retrieve
# links related to protests activities. (Mostly borrowed from the regular
# expression used by the countlove crawler:
# https://github.com/count-love/crawler)
linkFilter<-function(links){
  exclude_title_regex <- '-((basket|base|foot|soft|volley)ball|overtime|second\\-half|((first|third|fourth)\\-quarter)|gallery|letter|photos?|videos?|slideshow|pep\\-rall(y|ies)|stock\\-market|march\\-madness|dakar)'
  words_regex <-'(/|\\-)(protest(|ed|ers)|march(|ed)|demonstr(ated|ation|ators)|rall(y|ies|ied))(\\-|$)'
  links<-links[which(stringr::str_detect(links, "/article/"))]
  links<-links[which(stringr::str_detect(links, words_regex))]
  links<-links[which(!stringr::str_detect(links, exclude_title_regex))]
  return(links)
}

# Database setup-----
# code creates a new database if one doesn't already exist in the current
# directory

con <- dbConnect(RSQLite::SQLite(), "data/ap_data.db")
if(!dbExistsTable(con, "monthly_sitemaps")){
  dbExecute(con,
            "CREATE TABLE monthly_sitemaps(
            links TEXT PRIMARY KEY,
            linkdates TEXT,
            scraped BOOLEAN DEFAULT 0
            )"
  )
}
if(!dbExistsTable(con, "pages")){
  
  dbExecute(con,
            "CREATE TABLE pages(
            links TEXT PRIMARY KEY,
            headline TEXT,
            pubdate TEXT,
            article TEXT,
            metadata BLOB,
            scraped BOOLEAN DEFAULT 0,
            scraped_time TEXT
            )"
  )
}

# create/update links -----
# code to retrieve links from the AP sitemap. On the first run it includes
# all URLs since 2020. Subsequent runs should only add links that don't already
# exist in the table.


# get links to monthly sitemaps:
sitemap<-read_html('https://apnews.com/ap-sitemap.xml')

links<-sitemap|>
  html_elements(xpath='//loc[following-sibling::lastmod]')|>
  html_text()


linkdates<-stringr::str_extract(links, "([0-9]+)", group=1)|>
  stringr::str_c("01")|>
  as.Date(format="%Y%m%d")|>
  paste()

linktable<-data.frame(links, linkdates)
values<-
  linktable|>
  glue::glue_data_sql(.con = con, 
               "({links}, {linkdates})")|>
  glue::glue_sql_collapse(sep=', ')


statement<-glue::glue_sql(.con=con, 
               "INSERT INTO monthly_sitemaps (links, linkdates) 
               VALUES {values}
               ON CONFLICT (monthly_sitemaps.links) DO NOTHING")



dbExecute(con , statement)

# identify new sitemaps
unscraped<-dbGetQuery(con, "SELECT * from monthly_sitemaps 
                            WHERE 
                              (scraped =FALSE 
                              OR date(linkdates) > (date('now', '-35 days'))) 
                              AND date(linkdates)>='2020-01-01'")
for(i in 1:nrow(unscraped)){
  
  print(i)
  statement<-glue::glue_sql(.con=con, "UPDATE monthly_sitemaps 
                  SET scraped = TRUE 
                  WHERE links = {unscraped$links[i]}")
  dbExecute(con,statement)
  links_list<-read_html(unscraped$links[i])|>
    html_elements('url loc')|>
    html_text()
  
  values<-
    glue::glue_sql(.con = con, 
                   "({links_list})")|>
    glue::glue_sql_collapse(sep=', ')
  
  statement<-glue::glue_sql(.con=con, 
                            "INSERT INTO pages (links) 
               VALUES {values}
               ON CONFLICT (pages.links) DO NOTHING")
  dbExecute(con, statement)
  Sys.sleep(.1)
}

count <-dbGetQuery(con, "SELECT count(*) as pages_scraped from pages where scraped = 1")
count




# scraping the pages ----
## getting a list of unscraped links -----

to_scrape<-dbGetQuery(con, "SELECT links from pages where
                      links IN (SELECT links FROM pages  
                      where scraped =0 AND links LIKE '%/article/%' AND links NOT LIKE '%lotteries%' AND links NOT LIKE '%march-madness%'
                      AND links NOT LIKE '%ap-verifica%' AND links NOT LIKE '%earnings%' AND links not like '%noticias-%'
                      ORDER BY RANDOM() 
                      LIMIT 500
                      
                      )
                      ")


# get number of available cores for parallel scraping
n_workers<-parallel::detectCores() -1
n_groups<-n_workers * 5


## Running the scraper ----
if(nrow(to_scrape)>0){
  # base request with throttle to avoid sending more than 10 requests per second
  base_req<-request('https://apnews.com')|>
    req_throttle(capacity = 10, 
                 fill_time_s = 1)
  
  # scraping in batches of n_workers * 5
  links_to_scrape<-split(to_scrape$links, ceiling(seq_along(to_scrape$links)/n_groups))
  i<-1
  for(i in i:length(links_to_scrape)){
    print(i)
    links_i<-links_to_scrape[[i]]
    
    # create a list of requests:
    requests<-purrr::map(.x=links_i, .f=~base_req|>
                           req_url_path(
                             stringr::str_extract(.x, 'https://apnews\\.com(.+)', group=1))
                         
    )
  
    # perform requests in (in parallel)
    results <- req_perform_parallel(
      requests,
      on_error = 'continue',
      max_active = n_workers,
      progress = TRUE
    )
    # processing the results to a list
    processed_result <- purrr::map(.x = results, .f =  ~ processResult(.x))
    
    # updating the database with new results
    updated_rows <- purrr::map(.x = processed_result, .f =  ~ pageUpdater(con, .x))
    
    # check for "too many requests" errors and pause for a long time if
    # one is encountered
    ratelimited <- purrr::map_dbl(.x = results, .f =  ~ .x$status == 429) |>
      sum()
    if (ratelimited > 0) {
      print(sprintf(
        "%s errors pausing for %s seconds",
        failure_count,
        failure_count * 30
      ))
      Sys.sleep(failure_count * 30)
    }
  }
}

# uncomment to retrieve all scraped articles
#scraped_articles<-dbGetQuery(con, "SELECT * from pages where scraped = 1")

# disconnect from the database when you're finished working
dbDisconnect(con)



