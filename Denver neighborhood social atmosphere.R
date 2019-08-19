#install all required packages
install.packages(c("ggplot2", "e1071", "caret", "quanteda", "irlba", "randomGorest"))

install.packages( "tidyverse")
library(tidyverse)


if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh(
  "trinker/qdapDictionaries",
  "trinker/qdapRegex",
  "trinker/qdapTools",
  "trinker/qdap"
)

#Load up the .CSV data and explore in RStudio
Tony.raw <- read.csv("denver_listings.csv", stringsAsFactors = FALSE)
View(Tony.raw)

# Clean up the data frame and view our handiwork.
Tony.raw <- Tony.raw[!is.na(Tony.raw)]
Tony.raw <- filter("denver_listings.csv",neighbourhood_cleansed=="Jefferson Park", select= c("id","description","amenities","neighborhood_overview", "neighbourhood_cleansed", "number_of_reviews_ltm", "review_scores_rating"))
View(Tony.raw)

class(Tony.raw)
Tony.raw$neighborhood_overview <- Tony.raw$neighborhood_overview[complete.cases(Tony.raw$neighborhood_overview)]
head(Tony.raw$neighborhood_overview)

# Check data to see if there are missing values.
length(which(!complete.cases(Tony.raw)))

#Convert our class label into a factor.
Tony.raw$neighbourhood <- as.factor(Tony.raw$review_scores_rating) #as.factor()# as.character()which()

# The first step , as always, is to expore the data.
#First, let's take a look at distribution of the class labels (i.e., ham vs. spam),
prop.table(table(Tony.raw$neighbourhood))

#Next up , let's get a feel for the distribution of text lengths of the SMS
# messages by adding a new dearture for the length of each message.
Tony.raw$TextLength <- nchar(Tony.raw$description) #nchar(labels(Tony.raw$description)[Tony.raw$description])
summary(Tony.raw$TextLength)

#Visualize distribution with ggplot2, adding segmentation for ham/spam
library(ggplot2)

ggplot(Tony.raw, aes(x=TextLength, fill = neighbourhood)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Text Count", x = "Length of Text",
       title = "Distribution of Text Lengths with class Labels")


##########
# At a minimum we need to split our data into a training set and a test set. 
# In a true project we would want to use a three way split of training , validtaion and test.

# As we know that our data has non trivial class imbalance, 
# we'll use the mighty caret package to create a random train/test split that ensures the correct ham/spam class lael proportions
# (ie we'll use caret for a random stratified split)
library(caret)
help(package = "caret")

# Use caret to creat a 70%/30% stratified split. Set the random seed for reproducibility.
set.seed(32984)
indexes <- createDataPartition(Tony.raw$Label, times =1,
                               p = 0.7, list = FALSE)

train <- Tony.raw[indexes,]
test <- Tony.raw[-indexes,]

#Verify Proportions.
prop.table(table(train$Label))
prop.table(table(test$Label))



######
library(qdap)
frequent_terms <- freq_terms(Tony.raw$neighborhood_overview, 10)
plot(frequent_terms)


######
install.packages('tm',)
library(tm)
reviews <- read.csv("denver_listings.csv", stringsAsFactors = FALSE)

review_text1 <- paste(reviews$neighborhood_overview, collapse=" ")
review_text2 <- paste(reviews$amenities, collapse=" ")
review_text3 <- paste(reviews$description, collapse=" ")


review_source <- VectorSource(review_text1) #review_text2 #review_text3
corpus <- Corpus(review_source)

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)

frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
frequency <- sort(frequency[1:15], decreasing=TRUE)

install.packages('wordcloud')
library(wordcloud)
words <- names(frequency)
wordcloud(words[1:100], frequency[1:100])


#######
install.packages(c("tm", "SnowballC", "wordcloud", "RColorBrewer", "RCurl", "XML")
                 
                 
filePath <-
              
                

rquery.wordcloud(x, type=c("text", "url", "file"), 
                 lang="english", excludeWords = NULL, 
                 textStemming = FALSE,  colorPalette="Dark2",
                 max.words=200)




