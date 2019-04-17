
# Loading Data ------------------------------------------------------------

library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)


# Reading in Data ---------------------------------------------------------

train <- read.csv("data/train.csv", stringsAsFactors = FALSE)
test <- read.csv("data/test.csv",stringsAsFactors = FALSE)


# Data and Size Structure -------------------------------------------------
dim(train)
str(train[,c(1:10,81)])

test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL

test$SalePrice <- NA

all <- rbind(train,test)
dim(all)
