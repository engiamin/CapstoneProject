library(tm)
library(dplyr)
library(stringi)
library(stringr)



#bigram_save_df <- read.csv("./Bigrams/Bigrams_Combined.csv")
#save(bigram_save_df, file="Bigrams.RData")


#trigram_save_df <- read.csv("./Trigrams/Trigrams_Combined.csv")
#save(trigram_save_df, file="Trigrams.RData")


#quadgram_save_df <- read.csv("./Quadgrams/Quadgrams_Combined.csv")
#save(quadgram_save_df, file="Quadgrams.RData")



load(file = "Bigrams.RData")
load(file = "Trigrams.RData")
load(file = "Quadgrams.RData")


input_text ="Hi what's your"

algorithm <- function(input_text){
  
  a <- input_text
  a <- removeNumbers(removePunctuation(tolower(a)))
  a <- removeWords(a, stopwords("english"))
  a <- stripWhitespace(a)
  a <- unlist(strsplit(a, " "))
 
  
  if (length(a) >= 3) {
    
    #1# Take last three words and match them with 3 words in quadgram
    Match4gram <- quadgram_save_df %>%
      filter(as.character(quadgram_save_df$'X1')==a[length(a)-2] & 
               as.character(quadgram_save_df$'X2')==a[length(a)-1] & 
               as.character(quadgram_save_df$'X3')==a[length (a)])
    
    #Calculate sum of occurrences of three words  
    SumCount <- sum(Match4gram[, 'Count'])
    
    #Calculate scores
    Match4gram_scores <- Match4gram %>%
      mutate(Score  = Count/SumCount)
    
    #if there is an actual match .. return the match with highest score
    if (nrow(Match4gram_scores) > 0){
      WordMatch <- Match4gram_scores[Match4gram_scores$Score==max(Match4gram_scores$Score),
                                     as.character("X4")]
    } #otherwise look in lower ngram
    else
    {
      #Take last two words and match them with 2 words in trigram
      Match3gram <- trigram_save_df %>%
        filter(  as.character(trigram_save_df$'X1')==a[length(a)-1] & 
                   as.character(trigram_save_df$'X2')==a[length (a)])
      
      #Calculate sum of occurrences of three words  
      SumCount <- sum(Match3gram[, 'Count'])
      
      #Calculate scores
      Match3gram_scores <- Match3gram %>%
        mutate(Score  = 0.4 * Count/SumCount)
      
      
      #if there is an actual match .. return the match with highest score
      if (nrow(Match3gram_scores) > 0){
        WordMatch <- Match3gram_scores[Match3gram_scores$Score==max(Match3gram_scores$Score),
                                       as.character("X3")]
      } else {
        
        #Take last word and match them with 1 words in bigram
        Match2gram <- bigram_save_df %>%
          filter(as.character(bigram_save_df$'X1')==a[length(a)])
        
        #Calculate sum of occurrences of word  
        SumCount <- sum(Match2gram[, 'Count'])
        
        #Calculate scores
        Match2gram_scores <- Match2gram %>%
          mutate(Score  = 0.4 * 0.4 * Count/SumCount)
        
        
        #if there is an actual match .. return the match with highest score
        if (nrow(Match2gram_scores) > 0){
          WordMatch <- Match2gram_scores[Match2gram_scores$Score==max(Match2gram_scores$Score),
                                         as.character("X2")]
          
        } else {
          
          WordMatch <- "Sorry, No match!"
        }
        
        
        
      }
      
      
    }
    
  }
  
  
  
  
  if (length(a) == 2) {
      #Take last two words and match them with 2 words in trigram
      Match3gram <- trigram_save_df %>%
        filter(  as.character(trigram_save_df$'X1')==a[length(a)-1] & 
                   as.character(trigram_save_df$'X2')==a[length (a)])
      
      #Calculate sum of occurrences of three words  
      SumCount <- sum(Match3gram[, 'Count'])
      
      #Calculate scores
      Match3gram_scores <- Match3gram %>%
        mutate(Score  =  Count/SumCount)
      
      #if there is an actual match .. return the match with highest score
      if (nrow(Match3gram_scores) > 0){
        WordMatch <- Match3gram_scores[Match3gram_scores$Score==max(Match3gram_scores$Score),
                                       as.character("X3")]
      } else {
        
        #Take last word and match them with 1 words in bigram
        Match2gram <- bigram_save_df %>%
          filter(as.character(bigram_save_df$'X1')==a[length(a)])
        
        #Calculate sum of occurrences of word  
        SumCount <- sum(Match2gram[, 'Count'])
        
        #Calculate scores
        Match2gram_scores <- Match2gram %>%
          mutate(Score  = 0.4 * Count/SumCount)
        
        
        #if there is an actual match .. return the match with highest score
        if (nrow(Match2gram_scores) > 0){
          WordMatch <- Match2gram_scores[Match2gram_scores$Score==max(Match2gram_scores$Score),
                                         as.character("X2")]
          
        } else {
          
          WordMatch <- "Sorry, No match!"
        }

      }
  }
  
  
  
  
  if (length(a) == 1) {

      #Take last word and match them with 1 words in bigram
      Match2gram <- bigram_save_df %>%
        filter(as.character(bigram_save_df$'X1')==a[length(a)])
      
      #Calculate sum of occurrences of word  
      SumCount <- sum(Match2gram[, 'Count'])
      
      #Calculate scores
      Match2gram_scores <- Match2gram %>%
        mutate(Score  =  Count/SumCount)
      
      
      #if there is an actual match .. return the match with highest score
      if (nrow(Match2gram_scores) > 0){
        WordMatch <- Match2gram_scores[Match2gram_scores$Score==max(Match2gram_scores$Score),
                                       as.character("X2")]
        
      } else {
        
        WordMatch <- "Sorry, No match!"
      }
      
    
    
  } 

  
  WordMatch <- as.character(WordMatch)
  WordMatch <- ifelse (length(WordMatch) > 1, sample(WordMatch,1), WordMatch)
  WordMatch  
  
}


algorithm("hi hello")