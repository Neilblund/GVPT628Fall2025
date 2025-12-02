# quick demonstration of using a pre-trained sentence embedding model
# note that the reticulate package is just a way to call python from within R
# If you're already familiar with Python, you might be better off just using 
# it directly. When translating, just note that the "$" notation is taking the
# place of the "." notation in many of the functions here.
library(reticulate)

# you can check for available Python installations like this:
#py_config()

# if you don't already have version 3.9 or greater, you can run this to install
# the latest version:
# install_python()


# create a virtual env (generally a good idea for python projects, because it helps
# avoid compatability issues)

if(!virtualenv_exists('embedding_and_bertopic')){
  virtualenv_create("embedding_and_bertopic", version = '>=3.9')
  
  # then install the sentence transformers package
  py_install('sentence_transformers', envname = 'embedding_and_bertopic')
  # and install bertopic if needed
  py_install('bertopic', envname = 'embedding_and_bertopic')
}




# use the virtual environment you just created 
use_virtualenv('embedding_and_bertopic')

# import the sentence transformers package
sentence_transformer<-import('sentence_transformers')

# initialize an embedding model
embedding_model <- sentence_transformer$SentenceTransformer("all-MiniLM-L6-v2")



sentences<-c("America is a nation of immigrants.",
             "A pear is a type of fruit.",
             "The blue whale is the largest animal in the world.",
             "The Leviathan is a sea monster referenced in the book of Job.",
             "Foreigners come to the U.S. from around the world."
             )

# embed these sentences
embeddings<-embedding_model$encode(sentences)

# convert the array to a regular matrix
embedding_mat<-do.call("rbind", embeddings$tolist())

#  Creating a matrix of cosine similarity scores - higher numbers = more similar sentences

cos_sim <- function(vec1, vec2) {
  sum(vec1 * vec2) / sqrt(sum(vec1^2) * sum(vec2^2))
}

n <- nrow(embedding_mat) 
cmb <- expand.grid(i=1:n, j=1:n) 

similarity_matrix<-matrix(apply(cmb, 1, FUN=function(x) cos_sim(embedding_mat[x[1],], 
                                                                embedding_mat[x[2], ])), n, n)

rownames(similarity_matrix) <- sentences
colnames(similarity_matrix) <-sentences


similarity_matrix


# note that sentences that share exact terms still aren't particularly similar
# to one another:
library(ggcorrplot)
ggcorrplot(similarity_matrix, hc.order = FALSE, 
           lab = TRUE, lab_size = 3, tl.cex = 12, legend.title = 'Cosine Similarity')

# Because the sentence embeddings have a far lower dimensionality than their
# equivalent bag-of-words representations, they can be used as inputs to
# supervised or unsupervised models without lower complexity, and since
# they can be pre-trained on a huge amount of text, they make it feasible to train
# models on a smaller amount of text.



# BERtopic----------------------------------------------------
library(tidyverse)
bills<-read_csv("Data/bills_sample.csv")


texts<-purrr::map_chr(.x=bills$summary, .f=~stringr::str_split(.x, "\\s+")|>unlist()|>head(n=50)|>paste(collapse=' '))

bertopic<-import("bertopic")

topicmodel <- bertopic$BERTopic()


topics<-topicmodel$fit_transform(texts)

# get some representative documents for topic 5
# make a data frame with topic information
topics<-topicmodel$get_topic_info()
docs<-topics$Representative_Docs$tolist()

docs<-purrr::map_chr(.x= topics$Representative_Docs$tolist(), .f=~paste(.x, collapse='\n'))


topic_data<-tibble::tibble(
  topic_number = topics$Topic$tolist(),
  counts = topics$Count$tolist(),
  terms = topics$Name$tolist(),
  docs = docs
)




# we can use the trained model to predict new texts. Here's an example 

td<-topicmodel$transform(documents = c("Shiny medallions for valiant dudes act."))

topic<-py_to_r(td[[1]][[1]])$tolist()
topic

topic_data[which(topic_data$topic_number==topic),]

# note that the sentence is correctly assigned to a topic with bills celebrating
# veterans, even though the words are not really in those documents


# Get an approximation of the topic-document distribution from a fitted model:
topic_distr <- topicmodel$approximate_distribution(texts) 
topic_list<-topic_distr[[1]]$tolist()

theta<-do.call('rbind', topic_list)|>
  tibble()|>
  distinct()



