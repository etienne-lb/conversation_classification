# Packages
install.packages("MASS")
install.packages("pROC")
install.packages("tm")
install.packages("dplyr")
install.packages("nnet")
install.packages("caret")
install.packages("xml2")

library(MASS)
library(pROC)
library(tm)
library(dplyr)
library(caret)
library(e1071)
library(nnet)
library(xml2)
library(tidyr)
library(stringi)
library(stringr)


# Setting working directory
setwd("/Users/XXX/Desktop/iAdvize/MessagesCSV")
getwd()


## -----------------------  Data Preparation ----------------------- ##
# Reading all csvs of the training data
filenames <- list.files(full.names=TRUE)
filenames
df <- do.call("rbind",lapply(filenames,function(i){
  read.csv(i, header=TRUE,sep=";")
}))
df$TAG_GROUP <- as.factor(df$TAG_GROUP)
df <- subset(df,select=c("TAG_GROUP","LABEL_GROUP","CONVERSATION_ID"))


colnames(df)
# Reading all csvs of the test + training data
allmessages <- read.csv(file="../ALLCONV/ALL_MESSAGE_GOOD.csv",header=TRUE,sep=";",quote="")
nrow(allmessages)
allmessages <- subset(allmessages,select=c("id","date","message"))
colnames(allmessages)[colnames(allmessages)=="id"] <- "CONVERSATION_ID"
colnames(allmessages)[colnames(allmessages)=="message"] <- "MESSAGE"
colnames(allmessages)[colnames(allmessages)=="date"] <- "DATE"


# Joining to add the Label = catégory to the training data
df <- left_join(allmessages,df, by="CONVERSATION_ID")
df <- subset(allmessages,select=c("TAG_GROUP","LABEL_GROUP","CONVERSATION_ID","DATE","MESSAGE"))
df <- df %>% mutate(DATE=as.Date(DATE, format = "%d/%m/%Y"))
df <- df %>% mutate(YEAR=format(as.Date(DATE, format = "%Y-%m-%d"),"%Y"))
df <- df %>% mutate(MONTH=format(as.Date(DATE, format = "%Y-%m-%d"),"%m"))



colnames(df)
head(df)
nrow(df)

training <- df[!is.na(df$TAG_GROUP),]
test <- df[is.na(df$TAG_GROUP),]
nrow(test)
nrow(training)

# Creating corpus of words and cleaning
corpus <- Corpus(VectorSource(df$MESSAGE))

corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="fr")) %>%
  tm_map(stripWhitespace)


# Creating Document term matrix 
dtm <- DocumentTermMatrix(corpus.clean)
dim(dtm)


# Spliting into training and testing sets
head(test)
df.training <- training 
colnames(df.training)
df.test <- test
dim(df.test)
colnames(df.test)
dtm.training <- dtm[!is.na(df$TAG_GROUP),]
dtm.test <- dtm[is.na(df$TAG_GROUP),]
dim(dtm.test)

corpus.clean.training <- corpus.clean[!is.na(df$TAG_GROUP)]
corpus.clean.test <- corpus.clean[is.na(df$TAG_GROUP)]
length(corpus.clean.training)



#Further splitting for test year/month
rows.to.use <- which((df.test$YEAR==2019) & (df.test$MONTH=='09'))
rows.to.use
df.test <- df.test[rows.to.use,]
dtm.test <- dtm.test[rows.to.use,]
corpus.clean.test <- corpus.clean.test[rows.to.use]
length(corpus.clean.test)

# FREQUENCY : keep only words mentionned more than 5 times (based on the TRAINING SET)
fivefreq <- findFreqTerms(dtm.training, 5)
length(fivefreq)

# reducing the dtms to the most frequent words
dtm.train.reduit <- DocumentTermMatrix(corpus.clean.training, control=list(dictionary = fivefreq))
system.time( dtm.test.reduit <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq)) )
dim(dtm.test.reduit)


# Transform the word frequency to binary Yes/No
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}
training <- apply(dtm.train.reduit, 2, convert_count)


system.time( test <- apply(dtm.test.reduit, 2, convert_count) )


## -----------------------   Naive Bayes Model   ----------------------- ##
#creation model
system.time( monmodele <- naiveBayes(training,df.training$TAG_GROUP, laplace=1) )

#Applying model 
Sys.time()
system.time( predictionall <- predict(monmodele, test) ) # Takes maybe 30 min to run


  

#Formatting and writing results

allmessagestowrite <- df.test

allmessagestowrite$PREDICTION <- predictionall
allmessagestowrite$MESSAGE <- gsub(",","",allmessagestowrite$MESSAGE)
allmessagestowrite <- subset(allmessagestowrite,select=c("CONVERSATION_ID","DATE","PREDICTION","MESSAGE"))

correspondance <- read.csv(file="../TAG_CORRESPONDANCE.csv",header=TRUE,sep=";")
correspondance <- as.data.frame(correspondance)
colnames(correspondance)
colnames(correspondance)[colnames(correspondance)=="TAG_GROUP"] <- "PREDICTION"
correspondance$PREDICTION <- as.factor(correspondance$PREDICTION)


allmessagestowrite <- inner_join(allmessagestowrite,correspondance, by="PREDICTION")
allmessagestowrite <- subset(allmessagestowrite,select=c("CONVERSATION_ID","DATE","PREDICTION","LABEL_GROUP","MESSAGE"))
head(allmessagestowrite)
write.csv(allmessagestowrite,"../PREDICTION/PREDICTION_2019_09_v2.csv", sep=",") ### change output here


# On regarde que la fréquence/le nombre de chaque catégorie
allmessagestowrite %>%
  mutate(Total=sum(n())) %>% 
  group_by(PREDICTION) %>% 
  summarise(nombre=n())
#summarise(freq=n()/mean(Total),nombre=n())


