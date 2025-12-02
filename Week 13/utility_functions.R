
## a function to stem and then unstem a dfm ----
stem_unstem<-function(dfm){
  features<-featnames(dfm)
  feature_table<-data.frame(
    features = features,
    stemmed_features = char_wordstem(features, check_whitespace = FALSE),
    feature_counts = colSums(dfm)
  )
  tops<-tapply(feature_table, 
               INDEX = feature_table$stemmed_features, 
               FUN = function(x) {
                 x$features[which.max(x$feature_counts)]
               },
               simplify = TRUE
  )
  feature_table$replacement<-unname(tops)[match(feature_table$stemmed_features, names(tops))]
  dfm<-dfm_replace(dfm, pattern = feature_table$features,
                   replacement = feature_table$replacement
  )
  
  return(dfm)
}

# function to get documents associated with one or more topics
get_documents<-function(theta, k=1:ncol(theta), n=10, docvars=NULL){
  topdocs<-apply(cbind(theta[,k]), 2, FUN=function(x) head(order(x, decreasing=T), n=n))
  
  result<-data.frame(topic=rep(k, each=n), documents=c(topdocs))
  if(!is.null(docvars)){
    result<-docvars[result$documents,]
    result$topic<-rep(k, each=n)
  }
  return(result)
  
}
