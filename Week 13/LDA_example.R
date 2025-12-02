# Utility functions ############################################################

# ______________________________________________________________________________





# 1. Setup #####################################################################

## Checking for installed packages
packages<-c('quanteda','tidyverse','text2vec', 'keyATM', "LDAvis", "plotly", "umap")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}

library(tidyverse)     # data manipulation
library(quanteda)      # used for text processing
library(text2vec)      # general package for text analysis, used here just for LDA
 


# read the bills data
bills<-read_csv("Data/bills_sample.csv")|>
  mutate(became_law  =factor(became_law, 
                             levels = c("Did not become law", "Became law")),
         party = factor(party,  
                        levels=c("Democratic", "Bipartisan", "Republican")
         ))





# ______________________________________________________________________________





# 2. Pre-processing steps ######################################################


# convert to a corpus making sure to specify the document name field and the
# field containing text. Remaining variables will be passed as docvars, and can
# be accessed with docvars(bill_corpus)

bill_corpus<-corpus(bills, 
                    docid_field = 'Name',
                    text_field = 'summary'
)

# tokenize texts - split words, filter punctuation and lowercast

bill_tokens<-tokens(bill_corpus, 
                    remove_punct = TRUE,
                    remove_url =TRUE,
                    remove_numbers =TRUE,
                    remove_symbols=TRUE
)|>
  tokens_tolower()






# stopword removal (both from a list of standard stopwords and a custom list
# specific to this data

custom_stopwords <- c(
  'subsection',
  "section",
  'subchapter',
  "u.s.c.",
  "strike",
  "paragraph",
  "amend*",
  "act",
  "code",
  "subparagraph",
  "-",
  'title',
  "insert",
  "chapter"
)

bill_tokens<-bill_tokens|>
  tokens_remove(pattern = c(stopwords("en"),custom_stopwords)
  )


### dfm conversion ----
# this takes a list of tokens and creates a document-term-matrix
bill_dfm<-dfm(bill_tokens)

# note the dimensions:
dim(bill_dfm)


### Stemming and trimming----
# we'll do some additional pruning to our term-document-matrix. This is mostly
# for speed, and you might want to adjust these values depending on your data
# and your model.


# uncomment the code below to get a sense of how the stemming works.
# bill_tokens|>tokens_wordstem()|>pluck(1)

# the stem_unstem function will replace stemmed terms with their most common
# variant

bill_dfm<-dfm(bill_tokens)|>
  stem_unstem()|>
  # dfm_trim will remove very rare or very common words. 
  dfm_trim(min_docfreq = 2,
           max_docfreq = 1000,
           docfreq_type = 'count'
           )


# note the difference in the matrix size - this will make a big difference in how
# quickly our models run
dim(bill_dfm)

# ______________________________________________________________________________





# 3. LDA #######################################################################

# We'll use the text2vec package, which has an especially fast implementation of
# the basic LDA model

# note the programming paradigm here may feel a little unusual:
## 1. we initialize a model with LDA$new() and assign it to a variable "bill_lda"
## 2. we fit the model and return the output with bill_lda$fit_transform()
##    - fit_transform produces output but it also modifies bill_lda
## 3. we can access information about the fitted model by examining the 
##    bill_lda object

library(text2vec)
set.seed(100)

k<-25 # number of topics
alpha <- .01 # alpha controls the concentration for the document-topic distributions
beta <- .001 # beta controls the concentration for the word-topic distributions
bill_lda <- LDA$new(n_topics = k, 
                    doc_topic_prior = alpha, 
                    topic_word_prior = beta)

set.seed(365)
# convergence_tol allows early stopping if the model has converged
doc_topic_distr <- bill_lda$fit_transform(bill_dfm, 
                                     n_iter = 1000, 
                                     convergence_tol = 1e-5, 
                                     n_check_convergence = 10, 
                                     progressbar = interactive())

# examining the likelihoods from each iteration
ll <- attr(doc_topic_distr, 'likelihood')
plot(ll[,1],ll[,2], xlab='iteration', ylab='log-likelihood')

# get_top_words to get terms associated with each topic (phi) lambda controls a
# special term that increases/decreases the weights of common terms

## Examining topic distribtuions ----

bill_lda$get_top_words(n=10, lambda=1)

# a lambda value below 1 should show terms that are more distinctive to a given
# topic

bill_lda$get_top_words(n=10, lambda=.5)

# doc_topic_distr contains the document-topic distribution
dim(doc_topic_distr) # 1 row for each document, 1 column for each topic

# what are the documents most strongly associated with topic 1?

topdocs<-sort(doc_topic_distr[,1], decreasing=T)|>head(n=10)
topdocs

# using match to find the documents most associated with topic 1
match_index<-match(names(topdocs), docnames(bill_dfm))
docvars(bill_dfm)[match_index, ]


# using the custom function defined at the top of this script
result<-get_documents(doc_topic_distr, 
                      k = 1:3,
                      n = 10,
                      docvars=docvars(bill_dfm))






# Create an interactive visualization for a topic model (slightly modified from the original
# so that it can work in a markdown file)
source('https://raw.githubusercontent.com/Neilblund/APAN/refs/heads/main/ldavis_shareable.R')
LDAvis_standalone(bill_lda, outputfile='lda_topics.html')

# look for the lda_topics.html file in your current directory.

# If you want to display these results in a markdown file, you can use:
# htmltools::includeHTML('lda_topics.html')


## Visualizing documents ----


library(umap)
library(plotly)
wrap_labels <- function(x, width=50) {
  sapply(x, function(s) {
    paste(strwrap(s, width = width), collapse = "<br>")
  }, USE.NAMES = FALSE)
}




# putting the distributions in a data frame and renaming columns
theta<-doc_topic_distr|>data.frame()
numbers<-stringr::str_pad(string = seq(ncol(theta)), 
                          width=nchar(ncol(theta)), 
                          side='left', pad=0 )

theta_comp <- umap(theta)

theta_layout<-theta_comp$layout

colnames(theta_layout)<-c("Dim.1", "Dim.2")

# get the highest proportion topic for each document and labeling with the top 5 words
topic_labels<-apply(bill_lda$get_top_words(n=5, lambda=.5), 2, paste0, collapse=', ')
maxtopic<-apply(theta, 1, function(x) topic_labels[which.max(x)])



df<-data.frame('title' =paste0("<b>", wrap_labels(bills$title),"</b>"), theta_layout, topic = maxtopic)
docplot<-ggplot(df, aes(x=`Dim.1`, y=`Dim.2`, color=topic, labels=title)) + 
  geom_point() +
  theme_bw()

ggplotly(docplot)


## Topics by group -----


# combine theta with existing data
colnames(theta)<-topic_labels
bills_combined<-bind_cols(docvars(bill_dfm), theta)

# get long format for version of the distributions
bill_topics<-bills_combined|>
  pivot_longer(cols =(ncol(docvars(bill_dfm))+1):ncol(bills_combined), 
               names_to="topic", 
               values_to="proportion")|>
  select(title, topic, became_law,party, proportion)


  
# compare topic proportions in bills that passed against those that didn't
bill_topics|>
  ggplot(aes(x=became_law,y=proportion)) + 
  stat_summary(fun = mean, geom = "bar", fill = "lightblue") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)  +
  facet_wrap(~topic, scales='free_y') +
  theme_bw()



# compare bills based on the sponsorships
bill_topics|>
  ggplot(aes(x=party,y=proportion, fill=party)) + 
  stat_summary(fun = mean, geom = "bar") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)  +
  facet_wrap(~topic, scales='free_y') +
  theme_bw() +
  scale_fill_manual(values = c("Blue", "Grey", "Red"))





# ______________________________________________________________________________





# 4. Keyword topic model ########################################################

# note that this takes 5-10 minutes to run on the full data. 

library(keyATM)

# a dictionary with some key words for specific topics
bills_dict<-list('agriculture'  =c('farm*','food','agriculture','usda'),
                'defense' = c('dod','defense','military'),
                'schools' = c('student*','school*','education*'),
                'entitlements' = c("medicare",'medicaid','social',  'ssa', 'va', 'snap')
)

# converted to a quanteda dictionary
dict_topic<-dictionary(bills_dict)
# convert the pol_dfm to a structure preferred by this package
keyATM_docs<-keyATM_read(bill_dfm)
# convert the quanteda dictionary to a format used by keyATM 
keys<-read_keywords(docs = keyATM_docs, dictionary = dict_topic)

key_viz <- visualize_keywords(docs = keyATM_docs, keywords = keys)
key_viz



out <- keyATM(
  docs              = keyATM_docs,    # text input
  no_keyword_topics = 10,              # number of topics without keywords
  keywords          = keys,       # keywords
  model             = "covariates",       
  options           = list(seed = 250),
  model_settings = list(covariates_data = docvars(bill_dfm),
                        covariates_formula = ~ became_law
  )
)
# document-topics;
# out$theta
# topic-word distribution
# out$phi


# get the top
top_words(out)

# get the row index of the top documents 10 for each topic:
top_docs(out)

# plot the topic proportions
plot_topicprop(out)



topic1<-top_docs(out)[,1]

docvars(bill_dfm)[topic1,]


# viewing with topic covariates

strata_topic <- by_strata_DocTopic(
  out, by_var = "became_lawBecame law",
  labels =c('Did not become law', 'Became law')
)

fig_doctopic <- plot(strata_topic, var_name = "became_law")
fig_doctopic



plot_topicprop(out)

  



