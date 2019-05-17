#https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda


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

train <- read.csv("data/Zillow/train.csv", stringsAsFactors = FALSE)
test <- read.csv("data/Zillow/test.csv",stringsAsFactors = FALSE)


# Data and Size Structure -------------------------------------------------
dim(train)
str(train[,c(1:10,81)])

test_labels <- test$Id #keeping the id's in a vector
test$Id <- NULL
train$Id <- NULL

test$SalePrice <- NA #what we're trying to predict

all <- rbind(train,test)
dim(all)

names(test)


# Exploring Variables -----------------------------------------------------

##visualizing the response variable (SalesPrice)



ggplot(data = all[!is.na(all$SalePrice),],aes(x = SalePrice)) + 
        geom_histogram( fill = "blue", binwidth = 10000) + 
        scale_x_continuous(breaks  = seq(0,800000, by = 100000), labels = comma)

summary(all$SalePrice)

# which numeric variables have a high correlation with sales price
# numericVars <- which(map_lgl(.x = all, .f = ~ is.numeric(.))) #too many elements

# correlations with saleprice
numeric_vars <- which(sapply(all, is.numeric))
numericVarNames <- names(numeric_vars)

cat("there are", length(numeric_vars), "numeric variables")

all_numVar <- all[, numeric_vars]
cor_numVar <- cor(all_numVar, use = "pairwise.complete")

cor_sorted <- as.matrix(sort(cor_numVar[, "SalePrice"], decreasing = TRUE)) 
cor_sorted %>% View() #result not what I expected

CorHigh <- names(which(apply(cor_sorted, 1, function (x) abs(x) > 0.5)))

cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col = "black", tl.pos = "lt")

# visualizing the correlation between sales price and 
# overall quality, as well as above grade living area

#check for multicollinearity

# are there default settings that influence axis ticks? 
ggplot(data = all[!is.na(all$SalePrice), ], 
       aes(x = factor(OverallQual), y = SalePrice)) + 
        geom_boxplot(col = "blue", 
                     fill = "paleturquoise") + 
        labs(x = "Overall Quality", 
             y = "Sales Price") 




