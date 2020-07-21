
library(dplyr)




############################
######### READ DATA ########
############################


BlogsData <- readLines("./data/final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
NewsData <- readLines("./data/final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
TwitterData <- readLines("./data/final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

#############################################################################################
#############################################################################################

##############################
######### SAMPLE DATA ########
##############################


df<-data.frame(files=c("BlogsData","NewsData","TwitterData"),
               length=c(length(BlogsData),length(NewsData),length(TwitterData)))


# The sample size extracted each file will be proportional to its size in the total corpora
df_ratio <- df %>%
  mutate(ratio = length/sum(length)) %>%
  mutate(SampleSize = round((length * ratio),0))



set.seed(1234)
blog_subset <- sample(BlogsData, size = df_ratio$SampleSize[1], replace = FALSE)
news_subset <- sample(NewsData, size = df_ratio$SampleSize[2], replace = FALSE)
twitter_subset <- sample(TwitterData, size = df_ratio$SampleSize[3], replace = FALSE)


AllData <- c(blog_subset,news_subset,twitter_subset)
#This HUGE file is then split into smaller files (of size 0.5 MB) using windows cmd to be processed 
#in Data_Processing.R