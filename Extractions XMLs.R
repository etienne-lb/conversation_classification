# Packages
install.packages("MASS")
install.packages("pROC")
install.packages("tm")
install.packages("RTextTools")
install.packages("dplyr")
install.packages("nnet")
install.packages("caret")
install.packages("xml2")
install.packages("XML")

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
library(XML)


# Setting working directory
setwd("/Users/XXX/Desktop/iAdvize/MessagesCSV")
getwd()



## -----------------------  Preparation of the XMLs ----------------------- ##
{
files <- paste0("../XMLS/", dir("../XMLS", pattern = '.xml'))
files

# Function to split the date
strsplit <- function(x,
                     split,
                     type = "remove",
                     perl = FALSE,
                     ...) {
  if (type == "remove") {
    # use base::strsplit
    out <- base::strsplit(x = x, split = split, perl = perl, ...)
  } else if (type == "before") {
    # split before the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=.)(?=", split, ")"),
                          perl = TRUE,
                          ...)
  } else if (type == "after") {
    # split after the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=", split, ")"),
                          perl = TRUE,
                          ...)
  } else {
    # wrong type input
    stop("type must be remove, after or before!")
  }
  return(out)
}


xmlDF <- function(path){
  # First we get the messages
  df <- read_xml(files) %>% 
    #Getting all the information we need from the XMLs
    xml_find_all(".//.//datetime | .//operator | .//visitor | .//alert ") %>%
    xml_text() %>% 
    str_replace("The visitor has closed the chat window","VISITOR_CHAT_CLOSE") %>% 
    str_replace("The agent has closed the chat window","OPERATOR_CHAT_CLOSE") %>% 
    paste0(collapse = ". ") %>% strsplit("..\\/..\\/.... ..\\:..", type = 'before' ) %>% 
    unlist() %>% 
    data.frame() %>% 
    separate(col = 1, sep = " ..\\:..", into = c('DATE', 'MESSAGE'))
  
    df = df[-c(1),]
    #head(df)
    #nrow(df)

  
  return(df)
  
}

head(df)

system.time( l <- lapply(files, xmlDF) )
system.time( data.message <- do.call('rbind', l) )

head(data.message)
nrow(data.message)

#Writing result
write.csv(data.message, file = "DATA_MESSAGES_with ID.csv")
}
