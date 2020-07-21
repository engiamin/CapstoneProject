library(tm)
library(wordcloud)
library(igraph)
library(dplyr)
library(ngram) 
library(stringi)
library(ggplot2)
library(ggpubr)
library(tokenizers)
library(RWeka)
library(stringr)


#Profanity list file
ProfanityList <- read.delim("Profanity.txt", header=FALSE)
ProfanityList <- as.data.frame(ProfanityList)
profanity <- ProfanityList$V1

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))



# Define function to make N grams
tdm_Ngram <- function (textcp, n) {
  NgramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n))}
  tdm_ngram <- TermDocumentMatrix(textcp, control = list(tokenizer = NgramTokenizer))
  tdm_ngram
}


# Define function to extract the N grams and sort
ngram_sorted_df <- function (tdm_ngram) {
  tdm_ngram_m <- as.matrix(tdm_ngram)
  tdm_ngram_sums <- rowSums(tdm_ngram_m)
  tdm_ngram_df <- data.frame(words = names (tdm_ngram_sums), Count=tdm_ngram_sums)
  tdm_ngram_df <- tdm_ngram_df[order(-tdm_ngram_df$Count), , drop = FALSE]
}


ngram_save_df <- function(tdm_ngram_df, n) {
  tdm_ngram_df$words <- as.character(tdm_ngram_df$words)
  ngram_split <- str_split_fixed(tdm_ngram_df$words, " ", n)
  tdm_ngram_df <- cbind(tdm_ngram_df,ngram_split)
}



### Data files were partitioned into smaller files of size 0.5 MB (total of 369 files)

do_processing <- function(partNo){
  
  #Open and read File
  file <-  readLines(paste0("./Splits/part (",partNo,").txt"), encoding="UTF-8",  skipNul = TRUE)
  
  
  #Clean File
  file <- VCorpus(VectorSource(file))
  file <- tm_map(file, toSpace, "[^a-zA-Z|']")
  file <- tm_map(file, toSpace, "http\\S*") #remove URLS http first, then www, until the next space
  file <- tm_map(file, toSpace, "www\\S*") #remove URLS http first, then www, until the next space
  file <- tm_map(file, removeNumbers) #remove numbers
  file <- tm_map(file, content_transformer(tolower)) #convert all to lowercase
  file <- tm_map(file, removePunctuation) #remove punctuation
  file <- tm_map(file, removeWords, stopwords("english")) #remove stopwords
  file <- tm_map(file, stripWhitespace) #remove extra white spaces
  file <- tm_map(file, removeWords, profanity) #remove extra white spaces
  
  
  #Make N grams
  tdm_1gram <- tdm_Ngram(file, 1)
  tdm_2gram <- tdm_Ngram(file, 2)
  tdm_3gram <- tdm_Ngram(file, 3)
  tdm_4gram <- tdm_Ngram(file, 4)
  
  
  # Extract term-count tables from N-Grams and sort 
  tdm_1gram_df <- ngram_sorted_df(tdm_1gram)
  tdm_2gram_df <- ngram_sorted_df(tdm_2gram)
  tdm_3gram_df <- ngram_sorted_df(tdm_3gram)
  tdm_4gram_df <- ngram_sorted_df(tdm_4gram)
  
  # Save files
  unigram_save_df <- ngram_save_df(tdm_1gram_df,1)
  write.csv(unigram_save_df, paste0("Part_",partNo,"_Unigram.csv"))
  
  bigram_save_df <- ngram_save_df(tdm_2gram_df,2)
  write.csv(bigram_save_df, paste0("Part_",partNo,"_Bigram.csv"))
  
  trigram_save_df <- ngram_save_df(tdm_3gram_df,3)
  write.csv(trigram_save_df, paste0("Part_",partNo,"_Trigram.csv"))
  
  quadgram_save_df <- ngram_save_df(tdm_4gram_df,4)
  write.csv(quadgram_save_df, paste0("Part_",partNo,"_Quadgram.csv"))
  
  rm(file)
  
}



for (i in seq(from = 1, to=369)){
  do_processing(i) 
}

