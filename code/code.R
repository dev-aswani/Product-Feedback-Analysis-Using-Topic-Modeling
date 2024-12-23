install.packages("reticulate")
install.packages("httr") #to read from web
install.packages("jsonlite") #to get json file format
install.packages("tm") #TextMining Library
install.packages("quanteda") #alternative of some functions in tm library
install.packages("pdftools") #To Read PDF files
install.packages("reshape2") #to work with dataframe
install.packages("ggrepel")
install.packages("tidytext")
install.packages("dplyr") 
install.packages("stringr")
install.packages("udpipe")
install.packages("flextable")
# install klippy for copy-to-clipboard button in code chunks
install.packages("remotes")
remotes::install_github("rlesur/klippy")
install.packages("tidyr")
install.packages("SentimentAnalysis")
install.packages("textclean")


library("httr") 
library("jsonlite") 
library("tm")
library("pdftools")
library("quanteda")
library("dplyr")
library("reshape2")
library("tidyr")
library("ggplot2")
library("ggrepel")
library("tidytext")
library(reticulate)
library(SentimentAnalysis)

library(stringr) 
library(udpipe) 
library(flextable)
# activate klippy for copy-to-clipboard button
klippy::klippy()


#Getting the youtube comments ----

# Create the virtual environment
virtualenv_create(envname = "youtube-comments", packages = c("google-api-python-client", "pandas"))

# Use the virtual environment
use_virtualenv("youtube-comments")

# Check Python configuration
py_config()

# List and filter packages
py_list_packages(envname = "youtube-comments")
grep(pattern = "google-api-python-clien|pandas",
     x       = as.character(py_list_packages(envname = "youtube-comments")$package))




#basic python api comment retriever
py_run_string("
import googleapiclient.discovery
import pandas as pd

api_service_name = 'youtube'
api_version = 'v3'
DEVELOPER_KEY = 'AIzaSyDWjcFsQE4qsjUHjUHOOrzZQWQQWgcvTII'  # Replace with your actual API key

youtube = googleapiclient.discovery.build(
    api_service_name, api_version, developerKey=DEVELOPER_KEY)

def getcomments(video):
    request = youtube.commentThreads().list(
        part='snippet',
        videoId=video,
        maxResults=100
    )

    comments = []

    # Execute the request.
    response = request.execute()

    # Get the comments from the response.
    for item in response['items']:
        comment = item['snippet']['topLevelComment']['snippet']
        public = item['snippet']['isPublic']
        comments.append([
            comment['authorDisplayName'],
            comment['publishedAt'],
            comment['likeCount'],
            comment['textOriginal'],
            comment['videoId'],
            public
        ])

    while 'nextPageToken' in response:
        nextPageToken = response['nextPageToken']
        # Create a new request object with the next page token.
        nextRequest = youtube.commentThreads().list(
            part='snippet', videoId=video, maxResults=100, pageToken=nextPageToken)
        # Execute the next request.
        response = nextRequest.execute()
        # Get the comments from the next response.
        for item in response['items']:
            comment = item['snippet']['topLevelComment']['snippet']
            public = item['snippet']['isPublic']
            comments.append([
                comment['authorDisplayName'],
                comment['publishedAt'],
                comment['likeCount'],
                comment['textOriginal'],
                comment['videoId'],
                public
            ])

    df2 = pd.DataFrame(comments, columns=['author', 'published_at', 'like_count', 'text', 'video_id', 'public'])
    return df2

# Retrieve comments for multiple videos
video_ids = ['eVXfsNS-V_I']
df_list = [getcomments(video_id) for video_id in video_ids]
# Concatenate the DataFrames
df = pd.concat(df_list)
df = df.sort_values(by='like_count', ascending=False)
")

df <- py$df
View(df)
df<- na.omit(df)

#initially sampled 1500 rows then later stored them for replicability
#samples <- sample(1:nrow(df),1500)
#write.csv(samples_df,"samples.csv")

samples_df <- read.csv("samples.csv")
View(samples_df)

samples <- samples_df$values
View(samples)

df <- df[samples, ] 

nrow(df)
#Sentiment tagging ----
set.seed(323)
sentiment <- analyzeSentiment(df$text)
df$sentiment <- convertToDirection(sentiment$SentimentQDAP)

positive_df <- df %>%
  filter(sentiment=='positive')

neutral_df <- df %>%
  filter(sentiment=='neutral')

negative_df <- df %>%
  filter(sentiment=='negative')

#Pre-processing ----

#creating a corpus
overall_corpus<-Corpus(VectorSource(df$text))

positive_corpus<-Corpus(VectorSource(positive_df$text))

neutral_corpus<-Corpus(VectorSource(neutral_df$text))

negative_corpus<-Corpus(VectorSource(negative_df$text))

inspect(neutral_corpus)

replace_emojis <- function(column) {
  # Define a dictionary of emojis and their corresponding words
  emoji_dict <- list("😃" = " happy ", "😢" = " sad ", "❤️" =  "lov e", "😍" =  "ador e", "😡" ="angry"  )
  
  # Loop through each emoji in the dictionary and replace it with its corresponding word
  for (emoji in names(emoji_dict)) {
    column <- gsub(emoji, emoji_dict[[emoji]], column, fixed = TRUE)
  }
  
  return(column)
}

remove_ <- function(column) {
  # Pattern to match newlines, tabs, angle brackets, and anything within angle brackets
  pattern <- "[\r\n\t<>]+|<.*?>"
  
  # Pattern to match emojis (generalized)
  #emoji_pattern <- "[\\p{Emoji_Presentation}\\p{Extended_Pictographic}]"
  
  clean_text <- "[^\\w[:punct:][:space:]]"
  
  # Combine both patterns
  combined_pattern <- paste(pattern, clean_text, sep = "|")
  
  # Replace matched patterns with a space
  gsub(combined_pattern, " ", column, perl = TRUE)
}



overall_corpus<-tm_map(overall_corpus, content_transformer(replace_emojis))
positive_corpus<-tm_map(positive_corpus, content_transformer(replace_emojis))
neutral_corpus<-tm_map(neutral_corpus, content_transformer(replace_emojis))
negative_corpus<-tm_map(negative_corpus, content_transformer(replace_emojis))

overall_corpus<-tm_map(overall_corpus, content_transformer(remove_))
positive_corpus<-tm_map(positive_corpus, content_transformer(remove_))
neutral_corpus<-tm_map(neutral_corpus, content_transformer(remove_))
negative_corpus<-tm_map(negative_corpus, content_transformer(remove_))


#POS Tagging 
#english-ewt, english-gum, english-lines, english-partut: the different english models, ewt fits our situation best
m_eng   <- udpipe::udpipe_download_model(language = "english-ewt")

ud_model <- udpipe_load_model(file = m_eng)


pos_tag_text <- function(text) {
  # Annotate the text
  text_anndf <- udpipe::udpipe_annotate(ud_model, x = text) %>%
    as.data.frame() %>%
    mutate(token = ifelse((upos == "ADJ" & token == toupper(token)) | (upos == "PROPN" & token == toupper(token)), token, tolower(token))) %>%
    dplyr::select(token_id, token, lemma, upos, xpos)  # Select relevant columns
  return(text_anndf)
}



overall_corpus_pos<- lapply(overall_corpus, function(doc) {
  pos_tag_text(as.character(doc))
})

positive_corpus_pos<- lapply(positive_corpus, function(doc) {
  pos_tag_text(as.character(doc))
})

neutral_corpus_pos<- lapply(neutral_corpus, function(doc) {
  pos_tag_text(as.character(doc))
})

negative_corpus_pos <- lapply(negative_corpus, function(doc) {
  pos_tag_text(as.character(doc))
})

head(negative_corpus_pos, 5)

filter_nouns_verbs <- function(corpus) {
  corpus %>% dplyr::filter(upos %in% c("NOUN", "VERB", "ADJ", "PROPN"))
}

inspect(negative_corpus)

overall_corpus_filtered <- lapply(overall_corpus_pos, filter_nouns_verbs)

positive_corpus_filtered <- lapply(positive_corpus_pos, filter_nouns_verbs)

neutral_corpus_filtered <- lapply(neutral_corpus_pos, filter_nouns_verbs)

negative_corpus_filtered <- lapply(negative_corpus_pos, filter_nouns_verbs)

#convert to lower case 

overall_corpus<-tm_map(overall_corpus, content_transformer(tolower))
positive_corpus <- tm_map(positive_corpus,content_transformer(tolower))
neutral_corpus <- tm_map(neutral_corpus,content_transformer(tolower))
negative_corpus <- tm_map(negative_corpus,content_transformer(tolower))


#lemmatization
replace_tokens_with_lemmas <- function(corpus_pos, original_text) {
  # Join the lemmas to form the lemmatized document
  lemmatized_text <- paste(corpus_pos$lemma, collapse = " ")
  return(lemmatized_text)
}


overall_df_lemmatized <- sapply(seq_along(overall_corpus_filtered), function(i) {
  replace_tokens_with_lemmas(overall_corpus_filtered[[i]], corpus[i])
})

positive_df_lemmatized <- sapply(seq_along(positive_corpus_filtered), function(i) {
  replace_tokens_with_lemmas(positive_corpus_filtered[[i]], corpus[i])
})

neutral_df_lemmatized <- sapply(seq_along(neutral_corpus_filtered), function(i) {
  replace_tokens_with_lemmas(neutral_corpus_filtered[[i]], corpus[i])
})

negative_df_lemmatized <- sapply(seq_along(negative_corpus_filtered), function(i) {
  replace_tokens_with_lemmas(negative_corpus_filtered[[i]], corpus[i])
})


# Print or store the results
#for (i in 1:length(overall_corpus_pos)) {
#  cat("Document", i, "POS Tagged Text:\n")
#  cat(overall_corpus_pos[[i]], "\n\n")
#}


# Inspect the lemmatized corpus
head(negative_df_lemmatized)


overall_corpus_lemmatized<-Corpus(VectorSource(overall_df_lemmatized))

positive_corpus_lemmatized<-Corpus(VectorSource(positive_df_lemmatized))

neutral_corpus_lemmatized<-Corpus(VectorSource(neutral_df_lemmatized))

negative_corpus_lemmatized<-Corpus(VectorSource(negative_df_lemmatized))

#removing stop words
overall_corpus<-tm_map(overall_corpus_lemmatized, removeWords, 
                       stopwords("english"))
positive_corpus<-tm_map(positive_corpus_lemmatized, removeWords,
                        stopwords("english"))
neutral_corpus<-tm_map(neutral_corpus_lemmatized, removeWords,
                       stopwords("english"))
negative_corpus<-tm_map(negative_corpus_lemmatized, removeWords, 
                        stopwords("english"))

#removing punctuation
overall_corpus<-tm_map(overall_corpus, removePunctuation, 
                       preserve_intra_word_dashes=TRUE)
positive_corpus<-tm_map(positive_corpus, removePunctuation,
                        preserve_intra_word_dashes=TRUE)
neutral_corpus<-tm_map(neutral_corpus, removePunctuation,
                       preserve_intra_word_dashes=TRUE)
negative_corpus<-tm_map(negative_corpus, removePunctuation, 
                        preserve_intra_word_dashes=TRUE)

#remove extra white space
overall_corpus<-tm_map(overall_corpus, stripWhitespace)
positive_corpus<-tm_map(positive_corpus, stripWhitespace)
neutral_corpus<-tm_map(neutral_corpus, stripWhitespace)
negative_corpus<-tm_map(negative_corpus, stripWhitespace)

#remove non-zero documents for corpus

overall_corpus_processed <- overall_corpus[!sapply(overall_corpus, function(doc) nchar(as.character(doc)) == 0)]
positive_corpus_processed <- positive_corpus[!sapply(positive_corpus, function(doc) nchar(as.character(doc)) == 0)]
neutral_corpus_processed <- negative_corpus[!sapply(negative_corpus, function(doc) nchar(as.character(doc)) == 0)]
negative_corpus_processed <- neutral_corpus[!sapply(neutral_corpus, function(doc) nchar(as.character(doc)) == 0)]

length(negative_corpus_processed)

inspect(negative_corpus_processed)



#customized stopwords -- iteration 1
custom_stopwords <- c("watch", "video", "series", "editor", "1st", "get", "arun", "rickroll", 
                  "mrwhosetheboss", "comment", "content", "cat", "know", "dude", "sick", 
                  "month", "bro", "scream", "roll", "rick", "milos", "forget", "cellphone", "say", 
                  "warudo", "sup", "malaysia", 
                  "haaaaaaa", "ayo", "-mazeball","get","phone")

overall_corpus_processed<-tm_map(overall_corpus_processed, removeWords, custom_stopwords)
positive_corpus_processed<-tm_map(positive_corpus_processed, removeWords, custom_stopwords)
neutral_corpus_processed<-tm_map(neutral_corpus_processed, removeWords, custom_stopwords)
negative_corpus_processed<-tm_map(negative_corpus_processed, removeWords, custom_stopwords)

#customized stopwords - iteration 2
custom_stopwords_2 <- c(
  "make", "sell", "content", "know", "arun", "lmao", "year", "say", "mention", "give", "way",
  "bro", "smh", "warudo", "take", "sup", "sheaomie",
  "oooooooooooooooooooahhhhhhh","minority", "malaysia", "haaaaaaa", "gfffff","suresh",
  "arun", "-mazeball","see","people","smart","mother","stall","sorry","unwatchable","arun","idk"
  
)

overall_corpus_processed<-tm_map(overall_corpus_processed, removeWords, custom_stopwords_2)
positive_corpus_processed<-tm_map(positive_corpus_processed, removeWords, custom_stopwords_2)
neutral_corpus_processed<-tm_map(neutral_corpus_processed, removeWords, custom_stopwords_2)
negative_corpus_processed<-tm_map(negative_corpus_processed, removeWords, custom_stopwords_2)

#customized stopwords - iteration 3

custom_stopwords_3 <- c(
  "bathroom","arun","review","leave","hold","mom","early","samsing","samsng","early","tell",
  "one","tech","day","4th","arun","informative","early","man","choose",
  "use","keep","gang","guy","early","late","think","vid","lot"
  
)

overall_corpus_processed<-tm_map(overall_corpus_processed, removeWords, custom_stopwords_3)
positive_corpus_processed<-tm_map(positive_corpus_processed, removeWords, custom_stopwords_3)
neutral_corpus_processed<-tm_map(neutral_corpus_processed, removeWords, custom_stopwords_3)
negative_corpus_processed<-tm_map(negative_corpus_processed, removeWords, custom_stopwords_3)

#customized stopwords - iteration 4

custom_stopwords_4 <- c(
  "arun","post","bangladesh","etiopea","warch","move","friday","mais",
  "sortie","2023","heeelllll","canada","nubia","friend","sort","yawn",
  "witchcraft","daughter","brit","canada","balls",
  "mil","daysolve","mais","sortie","post","sandstorm",
  "monobrow","reply","whick","channels","desktop","gay","americans","hungry",
  "canada","milos","bruh","ice","cream","chin","ching","sandstorm","helio","god","samson",
  "blessssssssssssssssssssssssssss","brim","section","whatch","noooooo","youtube",
  "channel","bore","helio","sandstorm","aint",
  "creak","views","arun","chance","businesscorporate","korea","sis","quote",
  "rickroll","waterdrop","rick","roll","rick-roll","rickroll","hello","miss","let","nerd",
  "sadsad","namibia","thats","test","creak","overrate","aurn"
  )

overall_corpus_processed<-tm_map(overall_corpus_processed, removeWords, custom_stopwords_4)
positive_corpus_processed<-tm_map(positive_corpus_processed, removeWords, custom_stopwords_4)
neutral_corpus_processed<-tm_map(neutral_corpus_processed, removeWords, custom_stopwords_4)
negative_corpus_processed<-tm_map(negative_corpus_processed, removeWords, custom_stopwords_4)

#customized stopwords - iteration 4

custom_stopwords_4 <- c(
  "racist","guy","gang","early","rick","mazeball","watch","editor","suresh","keerthy","sup",
)

overall_corpus_processed<-tm_map(overall_corpus_processed, removeWords, custom_stopwords_4)
positive_corpus_processed<-tm_map(positive_corpus_processed, removeWords, custom_stopwords_4)
neutral_corpus_processed<-tm_map(neutral_corpus_processed, removeWords, custom_stopwords_4)
negative_corpus_processed<-tm_map(negative_corpus_processed, removeWords, custom_stopwords_4)


#creating document term matrix

overall_dtm<-DocumentTermMatrix(overall_corpus_processed, 
                                control = list(weighting = function(x)
                                  weightTfIdf(x, normalize =TRUE)))

positive_dtm<-DocumentTermMatrix(positive_corpus_processed, 
                                 control = list(weighting = function(x)
                                   weightTfIdf(x, normalize =TRUE)))

neutral_dtm<-DocumentTermMatrix(neutral_corpus_processed, 
                                control = list(weighting = function(x)
                                  weightTfIdf(x, normalize =TRUE)))

negative_dtm<-DocumentTermMatrix(negative_corpus_processed, 
                                 control = list(weighting = function(x)
                                   weightTfIdf(x, normalize =TRUE)))

clean_dtm <- function(dtm) {
  empty_docs <- rowSums(as.matrix(dtm)) == 0
  if (any(empty_docs)) {
    cat("Number of empty documents:", sum(empty_docs), "\n")
    dtm <- dtm[!empty_docs, ]
  } else {
    cat("No empty documents found.\n")
  }
  
  if (nrow(dtm) == 0) {
    stop("The document-term matrix is empty after removing empty documents.")
  }
  
  if (any(rowSums(as.matrix(dtm)) == 0)) {
    stop("There are still empty documents in the DTM.")
  }
  
  return(dtm)
}

# Apply the function to each DTM
overall_dtm <- clean_dtm(overall_dtm)
positive_dtm <- clean_dtm(positive_dtm)
neutral_dtm <- clean_dtm(neutral_dtm)
negative_dtm <- clean_dtm(negative_dtm)


nrow(overall_dtm)
nrow(positive_dtm)
nrow(negative_dtm)
nrow(neutral_dtm)
#Term distribution analysis

#Convert Matrix to DataFrame 
#converting matrix to dataframe for analysis

overall_dtf <- as.data.frame(as.matrix(overall_dtm))
positive_dtf <- as.data.frame(as.matrix(positive_dtm))
neutral_dtf <- as.data.frame(as.matrix(neutral_dtm))
negative_dtf <- as.data.frame(as.matrix(negative_dtm))


#get list of all terms in overall corpus
colnames(overall_dtf)
#get list of all terms in positive corpus
colnames(positive_dtf)
#get list of all terms in neutral corpus
colnames(neutral_dtf)
#get list of all terms in negative corpus
colnames(negative_dtf)

#Term Distribution Analysis ----

set.seed(092093)
#Overall corpus
#Get Most Frequent Words 

#get top words 
overall_word_freq <- colSums(overall_dtf)
View(overall_word_freq)

#convert it to dataframe
overall_word_freq_df<- data.frame(Term = names(overall_word_freq), Frequency = overall_word_freq, 
                                  row.names = NULL)
View(overall_word_freq_df)

overall_top_words_all <-  overall_word_freq_df %>%
  top_n(20, Frequency) %>%
  arrange(desc(Frequency)) 

#how can you use this to improve your preprocessing?
View(overall_top_words_all)

#Plot it
ggplot(overall_top_words_all, aes(x = reorder(Term, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(x = "Word", y = "Frequency", title = "Top Words All") +
  coord_flip()


#Get top word per document 
#add document id as a column 
overall_dtf$document <- rownames(overall_dtf)

#Get Document Term Frequecy Matrix
overall_document_term_freq<- melt(overall_dtf, id.vars = "document", 
                        variable.name = "Term", value.name = "Frequency")
View(overall_document_term_freq)


#Positive corpus

#Get Most Frequent Words

#get top words 
positive_word_freq <- colSums(positive_dtf)
View(positive_word_freq)

#convert it to dataframe
positive_word_freq_df<- data.frame(Term = names(positive_word_freq), Frequency = positive_word_freq, 
                                  row.names = NULL)
View(positive_word_freq_df)

positive_top_words_all <-  positive_word_freq_df %>%
  top_n(20, Frequency) %>%
  arrange(desc(Frequency))

View(positive_top_words_all)

#Plot it
ggplot(positive_top_words_all, aes(x = reorder(Term, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(x = "Word", y = "Frequency", title = "Positive Top Words All") +
  coord_flip()


#Get top word per document 
#add document id as a column 
positive_dtf$document <- rownames(positive_dtf)

#Get Document Term Frequency Matrix
positive_document_term_freq<- melt(positive_dtf, id.vars = "document", 
                                  variable.name = "Term", value.name = "Frequency")
View(overall_document_term_freq)

#Neutral corpus

#get top words 
neutral_word_freq <- colSums(neutral_dtf)
View(neutral_word_freq)

#convert it to dataframe
neutral_word_freq_df<- data.frame(Term = names(neutral_word_freq), Frequency = neutral_word_freq, 
                                   row.names = NULL)
View(neutral_word_freq_df)

neutral_top_words_all <-  neutral_word_freq_df %>%
  top_n(20, Frequency) %>%
  arrange(desc(Frequency))

#how can you use this to improve your preprocessing?
View(neutral_top_words_all)

#Plot it
ggplot(neutral_top_words_all, aes(x = reorder(Term, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(x = "Word", y = "Frequency", title = "Neutral Top Words All") +
  coord_flip()


#Get top word per document
#add document id as a column 
neutral_dtf$document <- rownames(neutral_dtf)

#Get Document Term Frequecy Matrix
neutral_document_term_freq<- melt(neutral_dtf, id.vars = "document", 
                                   variable.name = "Term", value.name = "Frequency")
View(neutral_document_term_freq)


#Negative corpus

#get top words 
negative_word_freq <- colSums(negative_dtf)
View(negative_word_freq)

#convert it to dataframe
negative_word_freq_df<- data.frame(Term = names(negative_word_freq), Frequency = negative_word_freq, 
                                  row.names = NULL)
View(negative_word_freq_df)

negative_top_words_all <-  negative_word_freq_df %>%
  top_n(20, Frequency) %>%
  arrange(desc(Frequency))

View(negative_top_words_all)

#Plot it
ggplot(negative_top_words_all, aes(x = reorder(Term, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(x = "Word", y = "Frequency", title = "Negative Top Words All") +
  coord_flip() 


#Get top word per document 
#add document id as a column 
negative_dtf$document <- rownames(negative_dtf)

#Get Document Term Frequecy Matrix
negative_document_term_freq<- melt(negative_dtf, id.vars = "document", 
                                  variable.name = "Term", value.name = "Frequency")
View(negative_document_term_freq)


#LSA ----
set.seed(239393)
install.packages("lsa")
library("lsa")

#function for cleaning tdm

clean_tdm <- function(tdm) {
  empty_docs <- colSums(as.matrix(tdm)) == 0
  if (any(empty_docs)) {
    cat("Number of empty documents:", sum(empty_docs), "\n")
    tdm <- tdm[, !empty_docs]
  } else {
    cat("No empty documents found.\n")
  }
  
  if (ncol(tdm) == 0) {
    stop("The term-document matrix is empty after removing empty documents.")
  }
  
  if (any(colSums(as.matrix(tdm)) == 0)) {
    stop("There are still empty documents in the TDM.")
  }
  
  return(tdm)
}


#Overall

overall_tdm<-TermDocumentMatrix(overall_corpus_processed, 
                                control = list(weighting = function(x)
                                  weightTfIdf(x, normalize =TRUE)))

overall_tdm <- clean_tdm(overall_tdm)
inspect(overall_tdm)

overall_lsa_space<-lsa(overall_tdm,dims=dimcalc_share())

#convert to matrix
overall_tddm <- as.textmatrix(overall_lsa_space)
overall_tddm

#get term/feature 
overall_term_space<-overall_lsa_space$tk
overall_labels=rownames(overall_term_space)

#get document/feature 
overall_doc_space<-overall_lsa_space$dk
overall_labels_doc=rownames(overall_doc_space) #what is your observation?


#get feature space
overall_context_space<-overall_lsa_space$sk

#plotting the terms over 2 dimention

ggplot(as.data.frame(overall_term_space), aes(x = overall_term_space[,1], y=overall_term_space[,2], 
                                     label=overall_labels)) +
  geom_point() +
  geom_jitter()+
  geom_text_repel(max.overlaps = 15) +
  labs(x = "Context1", y = "Context2", title = "Terms in Space")

#using this matrix you can calculate document similarity using cosin
#this you should do for your task e.g show 10 most similar 1 rating comments
ggplot(as.data.frame(overall_doc_space), aes(x = overall_doc_space[,1], y=overall_doc_space[,2], 
                                     label=overall_labels_doc)) +
  geom_point() +
  geom_jitter()+
  geom_text_repel(max.overlaps = 15) +
  labs(x = "Context1", y = "Context2", title = "Docs in Space")



#LSA Visualization

install.packages("LSAfun")
library("LSAfun")

overall_sla_matrix<-overall_term_space %*% diag(overall_context_space) %*% t(overall_doc_space)


#extract top 5 terms of top 5 features
top_overall_terms <- c()
overall_term_space_df <- as.data.frame(overall_term_space)
overall_doc_space_df <- as.data.frame(overall_doc_space)

for(i in 1:5){
  top_overall_terms <- c(top_overall_terms,
                         rownames(overall_term_space_df)[which.max(overall_term_space_df[,i])])
}
top_overall_terms 


#find top 5 neighbors for all these top terms
neighbors(top_overall_terms[1],n=6,tvectors=overall_sla_matrix)
plot_neighbors(top_overall_terms[1],n=6,tvectors=overall_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3, col=c("black"))

neighbors(top_overall_terms[2],n=6,tvectors=overall_sla_matrix)
plot_neighbors(top_overall_terms[2],n=5,tvectors=overall_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3, col=c("black"))

neighbors(top_overall_terms[3],n=6,tvectors=overall_sla_matrix)
plot_neighbors(top_overall_terms[3],n=6,tvectors=overall_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3, col=c("black"))

neighbors(top_overall_terms[4],n=6,tvectors=overall_sla_matrix)
plot_neighbors(top_overall_terms[4],n=6,tvectors=overall_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3, col=c("black"))

neighbors(top_overall_terms[5],n=6,tvectors=overall_sla_matrix)
plot_neighbors(top_overall_terms[5],n=6,tvectors=overall_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3, col=c("black"))




#Positive

#Create Semantic Space from your corpus
positive_tdm<-TermDocumentMatrix(positive_corpus_processed, 
                                 control = list(weighting = function(x)
                                   weightTfIdf(x, normalize =TRUE)))
positive_tdm <- clean_tdm(positive_tdm)

View((as.data.frame(as.matrix(positive_tdm)))["sandstorm",])

#for specific number of dimensions, just use dim=dim_number e.g dim=10
positive_lsa_space<-lsa(overall_tdm,dims=dimcalc_share())
positive_lsa_space 

#convert to matrix
positive_tddm <- as.textmatrix(positive_lsa_space)
positive_tddm

#get term/feature 
positive_term_space<-positive_lsa_space$tk
positive_term_space
positive_labels=rownames(positive_term_space)

#get document/feature 
positive_doc_space<-positive_lsa_space$dk
positive_labels_doc=rownames(positive_doc_space) #what is your observation?

#get feature space
positive_context_space<-positive_lsa_space$sk

#plotting the terms over 2 dimensions
ggplot(as.data.frame(positive_term_space), aes(x = positive_term_space[,1], y=positive_term_space[,2], 
                                     label=positive_labels)) +
  geom_point() +
  geom_jitter()+
  geom_text_repel(max.overlaps = 25) +
  labs(x = "Context1", y = "Context2", title = "Terms in Space")

#using this matrix you can calculate document similarity using cosin
#this you should do for your task e.g show 10 most similar 1 rating comments
ggplot(as.data.frame(positive_doc_space), aes(x = positive_doc_space[,1], y=positive_doc_space[,2], 
                                    label=positive_labels_doc)) +
  geom_point() +
  geom_jitter()+
  geom_text_repel(max.overlaps = 15) +
  labs(x = "Context1", y = "Context2", title = "Docs in Space")

#LSA Visualization
install.packages("LSAfun")
library("LSAfun")

positive_sla_matrix<-positive_term_space %*% diag(positive_context_space) %*% t(positive_doc_space)
rownames(positive_sla_matrix)

#extract top 5 terms for the first 5 features and plot 5 closest neighbors

class(positive_term_space)
top_positive_terms <- c()
positive_term_space_df <- as.data.frame(positive_term_space)
rownames(positive_term_space_df)[which.max(positive_term_space_df[,1])]

top_positive_terms

for(i in 1:5){
  top_positive_terms <- c(top_positive_terms,
                          rownames(positive_term_space_df)[which.max(positive_term_space_df[,i])])
}

top_positive_terms
#find top 5 neighbours for all these top terms

neighbors(top_positive_terms[1],n=6,tvectors=positive_sla_matrix)
plot_neighbors(top_positive_terms[1],n=6,tvectors=positive_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3, col=heat.colors()) 

?plot_neighbors
top_positive_terms
neighbors(top_positive_terms[2],n=6,tvectors=positive_sla_matrix)
plot_neighbors(top_positive_terms[2],n=5,tvectors=positive_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3)

neighbors(top_positive_terms[3],n=6,tvectors=positive_sla_matrix)
plot_neighbors(top_positive_terms[3],n=5,tvectors=positive_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3)

neighbors(top_positive_terms[4],n=6,tvectors=positive_sla_matrix)
plot_neighbors(top_positive_terms[4],n=5,tvectors=positive_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3)

neighbors(top_positive_terms[5],n=6,tvectors=positive_sla_matrix)
plot_neighbors(top_positive_terms[5],n=6,tvectors=positive_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3)

#Neutral

# Create Semantic Space from your corpus 
neutral_tdm <- TermDocumentMatrix(neutral_corpus_processed, 
                                  control = list(weighting = function(x)
                                    weightTfIdf(x, normalize =TRUE)))
neutral_tdm <- clean_tdm(neutral_tdm)

neutral_lsa_space <- lsa(neutral_tdm, dims=dimcalc_share())

# Convert to matrix
neutral_tddm <- as.textmatrix(neutral_lsa_space)

# Get term/feature space
neutral_term_space <- neutral_lsa_space$tk
neutral_labels = rownames(neutral_term_space)

# Get document/feature space
neutral_doc_space <- neutral_lsa_space$dk
neutral_labels_doc = rownames(neutral_doc_space)

# Get feature space
neutral_context_space <- neutral_lsa_space$sk

# Plotting the terms over 2 dimensions
ggplot(as.data.frame(neutral_term_space), aes(x = neutral_term_space[,1], y = neutral_term_space[,2], 
                                              label = neutral_labels)) +
  geom_point() +
  geom_jitter() +
  geom_text_repel(max.overlaps = 15) +
  labs(x = "Context1", y = "Context2", title = "Terms in Neutral Space")

# Plotting the documents over 2 dimensions
ggplot(as.data.frame(neutral_doc_space), aes(x = neutral_doc_space[,1], y = neutral_doc_space[,2], 
                                             label = neutral_labels_doc)) +
  geom_point() +
  geom_jitter() +
  geom_text_repel(max.overlaps = 15) +
  labs(x = "Context1", y = "Context2", title = "Docs in Neutral Space")

# LSA Visualization for Neutral Corpus 
neutral_sla_matrix <- neutral_term_space %*% diag(neutral_context_space) %*% t(neutral_doc_space)

#extract top 5 terms for the first 5 features and plot 5 closest neighbors

top_neutral_terms <- c()
neutral_term_space_df <- as.data.frame(neutral_term_space)
rownames(neutral_term_space_df)[which.max(neutral_term_space_df[,5])]

for(i in 1:5){
  top_neutral_terms <- c(top_neutral_terms,
                         rownames(neutral_term_space_df)[which.max(neutral_term_space_df[,i])])
}
top_neutral_terms

View(overall_dtf[,'phone',drop=FALSE] %>%
       arrange(-phone))

#find top 5 neighbours for all these top terms
neighbors(top_neutral_terms[1],n=6,tvectors=neutral_sla_matrix)

plot_neighbors(top_neutral_terms[1],n=6,tvectors=neutral_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3, col=c("black"))

neighbors(top_neutral_terms[2],n=6,tvectors=neutral_sla_matrix)
plot_neighbors(top_neutral_terms[2],n=5,tvectors=neutral_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3, col=c("black"))

neighbors(top_neutral_terms[3],n=6,tvectors=neutral_sla_matrix)
plot_neighbors(top_neutral_terms[3],n=5,tvectors=neutral_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3, col=c("black"))
?plot_neighbors
neighbors(top_neutral_terms[4],n=6,tvectors=neutral_sla_matrix)

plot_neighbors(top_neutral_terms[4],n=5,tvectors=neutral_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3, col=c("black"))

neighbors(top_neutral_terms[5],n=6,tvectors=neutral_sla_matrix)
plot_neighbors(top_neutral_terms[5],n=6,tvectors=neutral_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3, col=c("black"))

#Negative 

negative_tdm <- TermDocumentMatrix(negative_corpus_processed, 
                                   control = list(weighting = function(x)
                                     weightTfIdf(x, normalize =TRUE)))

negative_tdm <- clean_tdm(negative_tdm)

negative_lsa_space <- lsa(negative_tdm, dims=dimcalc_share())


# Convert to matrix
negative_tddm <- as.textmatrix(negative_lsa_space)

# Get term/feature space
negative_term_space <- negative_lsa_space$tk
negative_labels = rownames(negative_term_space)

# Get document/feature space
negative_doc_space <- negative_lsa_space$dk
negative_labels_doc = rownames(negative_doc_space)

# Get feature space
negative_context_space <- negative_lsa_space$sk

# Plotting the terms over 2 dimensions
ggplot(as.data.frame(negative_term_space), aes(x = negative_term_space[,1], y = negative_term_space[,2], 
                                               label = negative_labels)) +
  geom_point() +
  geom_jitter() +
  geom_text_repel(max.overlaps = 15) +
  labs(x = "Context1", y = "Context2", title = "Terms in Negative Space")

# Plotting the documents over 2 dimensions
ggplot(as.data.frame(negative_doc_space), aes(x = negative_doc_space[,1], y = negative_doc_space[,2], 
                                              label = negative_labels_doc)) +
  geom_point() +
  geom_jitter() +
  geom_text_repel(max.overlaps = 15) +
  labs(x = "Context1", y = "Context2", title = "Docs in Negative Space")

# LSA Visualization for Negative Corpus 
negative_sla_matrix <- negative_term_space %*% diag(negative_context_space) %*% t(negative_doc_space)
rownames(negative_sla_matrix)
plot_neighbors("big", n = 20, tvectors = negative_sla_matrix)
neighbors("low", n=20, tvectors=negative_sla_matrix)
# Plot neighbors using PCA
plot_neighbors("big", n = 20, tvectors = negative_tddm, connect.lines = 0, 
               start.lines = T, method = "PCA", dims = 3)


#extract top 5 terms of top 5 features
top_negative_terms <- c()
negative_term_space_df <- as.data.frame(negative_term_space)
rownames(negative_term_space_df)[which.max(negative_term_space_df[,5])]

for(i in 1:5){
  top_negative_terms <- c(top_negative_terms,
                          rownames(negative_term_space_df)[which.max(negative_term_space_df[,i])])
}

top_negative_terms
#find top 5 neighbours for all these top terms
neighbors(top_negative_terms[1],n=6,tvectors=negative_sla_matrix)
plot_neighbors(top_negative_terms[1],n=6,tvectors=negative_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3, col=c("black"))

neighbors(top_negative_terms[2],n=6,tvectors=negative_sla_matrix)
plot_neighbors(top_negative_terms[2],n=5,tvectors=negative_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3, col=c("black"))

neighbors(top_negative_terms[3],n=6,tvectors=negative_sla_matrix)
plot_neighbors(top_negative_terms[3],n=5,tvectors=negative_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3, col=c("black"))

neighbors(top_negative_terms[4],n=6,tvectors=negative_sla_matrix)
plot_neighbors(top_negative_terms[4],n=5,tvectors=negative_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3, col=c("black"))

neighbors(top_negative_terms[5],n=6,tvectors=negative_sla_matrix)
plot_neighbors(top_negative_terms[5],n=6,tvectors=negative_sla_matrix, connect.lines = 0, 
               start.lines = T, method="PCA",dims = 3, col=c("black"))

#7: Latent Dirichlet Allocation (LDA) ----

#creating document term matrix for LDA
set.seed(7676794)
overall_dtm<-DocumentTermMatrix(overall_corpus_processed)

positive_dtm<-DocumentTermMatrix(positive_corpus_processed)

neutral_dtm<-DocumentTermMatrix(neutral_corpus_processed)

negative_dtm<-DocumentTermMatrix(negative_corpus_processed)

clean_dtm <- function(dtm) {
  empty_docs <- rowSums(as.matrix(dtm)) == 0
  if (any(empty_docs)) {
    cat("Number of empty documents:", sum(empty_docs), "\n")
    dtm <- dtm[!empty_docs, ]
  } else {
    cat("No empty documents found.\n")
  }
  
  if (nrow(dtm) == 0) {
    stop("The document-term matrix is empty after removing empty documents.")
  }
  
  if (any(rowSums(as.matrix(dtm)) == 0)) {
    stop("There are still empty documents in the DTM.")
  }
  
  return(dtm)
}

# Apply the function to each DTM
overall_dtm <- clean_dtm(overall_dtm)
positive_dtm <- clean_dtm(positive_dtm)
neutral_dtm <- clean_dtm(neutral_dtm)
negative_dtm <- clean_dtm(negative_dtm)


nrow(overall_dtm)
nrow(positive_dtm)
nrow(negative_dtm)
nrow(neutral_dtm)

install.packages("topicmodels")
library("topicmodels")

install.packages("ldatuning")
library("ldatuning")

#find ideal number of topics (leave this as a task)
result <- FindTopicsNumber(
  overall_dtm,
  topics = seq(from = 2, to = 50, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result) 

#13 or 7 for overall
#12 or 10 for positive
#11 or 15 for neutral
#19 or 16 for negative

#7.2 Create LDA Model
set.seed(123)

?LDAcontrol
overall_ldaResult <-LDA(overall_dtm, 13, method="VEM", control=list(nstart=4, seed = list(1,2,3,4), best=TRUE, alpha = 0.1))
positive_ldaResult <-LDA(positive_dtm, 12, method="VEM", control=list(nstart=4, seed = list(1,2,3,4), best=TRUE, alpha = 0.1))
neutral_ldaResult <-LDA(neutral_dtm, 15, method="VEM", control=list(nstart=4, seed = list(1,2,3,4), best=TRUE, alpha = 0.1))
negative_ldaResult <-LDA(negative_dtm, 16, method="VEM", control=list(nstart=4, seed = list(1,2,3,4), best=TRUE, alpha = 0.1))

terms(positive_ldaResult, 5)


#making a wordcloud for each topic 
install.packages("wordcloud")
library(wordcloud)
topic = 1

words = posterior(negative_ldaResult)$terms[topic, ]
topwords = head(sort(words, decreasing = T), n=50)
head(topwords)

wordcloud(names(topwords), topwords)


#7.3 Plotting Topics/Terms
# Visualizing top 10 words with highest beta from each topic uinsg tidytext
lda.topics <- tidy(negative_ldaResult,matrix = "beta")
top_terms <- lda.topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>% 
  ungroup() %>%
  arrange(topic,-beta)

plot_topic <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
plot_topic


#7.3 Plotting Topics/Document
lda.document <- tidy(positive_ldaResult, matrix = "gamma")
lda.document

top_terms <- lda.document %>%
  group_by(topic) %>%
  top_n(10,gamma) %>% 
  ungroup() %>%
  arrange(topic,-gamma)

plot_topic <- top_terms %>%
  mutate(term = reorder_within(document, gamma, topic)) %>%
  ggplot(aes(document, gamma, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
plot_topic

#7.4 LDA Interactive Visualization 
install.packages("LDAvis")
library("LDAvis")


topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

serVis(topicmodels2LDAvis(negative_ldaResult))

