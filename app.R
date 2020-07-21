#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
library(dplyr)
library(stringi)
library(stringr)



load(file = "BigramsOld.RData")
load(file = "TrigramsOld.RData")
load(file = "QuadgramsOld.RData")



# Define UI for application that draws a histogram
ui <- fluidPage(
  
   titlePanel("Capstone Project: Predicting the Next Word"),
   
   # Sidebar with a slider input for number of bins 
   
      sidebarPanel(
        helpText("Enter a sentence of 3 words or more and the application will predict the following word!"),
        br(),
        textInput("inputString", "Enter your sentence here",value = ""),
        br()
      ),
   
   mainPanel(
     tabsetPanel(type = "tabs", 
                 
                 tabPanel("Prediction",
                   tags$br(), 
                   h2("The Predicted Word:"),
                   verbatimTextOutput("prediction"),
                   helpText("Please wait while the algorithm predicts the next word."),
                   br(),
                   strong("You entered:"),
                   tags$style(type='text/css', '#text1 {background-color: rgba(255,255,0,0.40); color: blue;}'), 
                   textOutput('text1'),
                   br()
                 ),
                 
                 tabPanel("About the Application", 
                          tags$br(),
                          h2("How to Use the Application"),
                          tags$ol(
                            tags$li("Type in the sentence you want to predict the following word for."),
                            tags$li("Wait for a few seconds until the algorithm is executed."),
                            tags$li("See the predicted word in the output box!")
                          ),
                          tags$br(),
                          h2("The Algorithm used"),
                          h5("The text prediction alogorithm is based on the" ,
                          a (href='http://www.cs.cornell.edu/courses/cs4740/2014sp/lectures/smoothing+backoff.pdf',
                             "Ngram modeling technique with a discount factor")),
                          tags$br(),
                          h5("The R code files can be found at",
                          a (href='https://github.com/engiamin/CapstoneProject',
                             "Github"))
                 )
                 )) )
   
   

server <- function(input, output) {
   
  algorithm <- function(input_text){
    
    a <- input_text
    a <- removeNumbers(removePunctuation(tolower(a)))
    a <- removeWords(a, stopwords("english"))
    a <- stripWhitespace(a)
    a <- unlist(strsplit(a, " "))
    
    if (length(a) >= 3) {
      
      #1# Take last three words and match them with 3 words in quadgram
      Match4gram <- quadgram_save_df %>%
        dplyr::filter(as.character(quadgram_save_df$'X1')==a[length(a)-2] & 
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
        if (nrow(Match4gram_scores) > 0){
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
      
      WordMatch <- as.character(WordMatch)
      
      WordMatch <- ifelse (length(WordMatch) > 1, sample(WordMatch,1), WordMatch)
      WordMatch
      
    }
    
  }
  
  
  
  
  output$prediction <- renderPrint({
    result <- algorithm(input$inputString)
    result
  });
  
  output$text1 <- renderText({
    input$inputString});
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

