'''
Guided Project - Predicting Car Prices

1. Understanding the Data

First we have to set up the data to be read in RStudio.
'''
library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
library(broom)
library(readxl)
library(ggplot2)
library(readr)
library(caret)

cars <- read_csv('~/Google Drive/Work/Data Analyst in R/Guided Projects/Predicting Car Prices/imports-85.data',
                 col_names = c('symboling', 'normalized_losses', 'make', 'fuel_type', 'aspiration', 'num_doors',
                               'body_style', 'drive_wheels', 'engine_location', 'wheel_base', 'length', 'width',
                               'height', 'curb_weight', 'engine_type', 'num_cylinders', 'engine_size', 'fuel_system', 
                               'bore', 'stroke', 'compression_ratio', 'horsepower', 'peak_rpm', 'city_mpg', 'highway_mpg', 'price'),
                 na = c('?', NA)) # converts all the ?s to nulls

cars <- cars[,c(1,2,6,10:14,16,17,19:26)] # only numeric columns

cars <- cars %>%
  drop_na()

num_doors <- c()
num_cylinders1 <- c()

for (row in 1:nrow(cars)) {
  if (cars[row, 3] == 'four') {
    num_doors <- c(num_doors, 4)
  }
  else if(cars[row, 3] == 'two') {
    num_doors <- c(num_doors, 2)
  }
}

for (row in 1:nrow(cars)) {
  if (cars[row, 9] == 'four') {
    num_cylinders1 <- c(num_cylinders1, 4)
  }
  else if(cars[row, 9] == 'three') {
    num_cylinders1 <- c(num_cylinders1, 3)
  }
  else if(cars[row, 9] == 'five') {
    num_cylinders1 <- c(num_cylinders1, 5)
  }
  else if(cars[row, 9] == 'six') {
    num_cylinders1 <- c(num_cylinders1, 6)
  }
  else if(cars[row, 9] == 'eight') {
    num_cylinders1 <- c(num_cylinders1, 8)
  }
}

cars$num_cylinders <- num_cylinders1
cars$num_doors <- num_doors


'''
Based on what we have done, all the columns have numeric values.
'''

'''
2. Examining Relationships Between Predictors

We must now try to make sense of the data to see trends in how different variables affect car price.
'''

featurePlot(cars$bore, cars$price)

'''
Based on the featurePlot function, we found that the following variables contribute to negative and positive relationships to price:

Positive:
wheel_base ~~
length ~
width ~
curb_weight ++
num_cylinders +
engine_size +
bore ~~
horsepower ++

Negative:
peak_rpm ~~
city_mpg ~
highway_mpg ~

There tends to be an outlier based on all the variables except for num_cylinders correlating to a car with 8 cylinders, which is understandable for
that variable but is a detriment to the others. These include mainly wheel_base, length, and bore. engine_size and horsepower are correlated with number
of cylinders, so the outlier does not pose too much of a shift. Therefore, removing the value would not harm other relationships.

'''
cars <- cars %>%
  filter(num_cylinders != 8)
'''

Further into the values, it seems like horsepower, curb_weight, num_cylinders, and engine_size play the largest roles in predicting price, based on
qualitative analysis of correlational strength.
'''

'''
3. Setting Up the Train-Test Split

Now that we have considered what variables play the largest role in pricing, let us split up the dataset.
'''
set.seed(1)

train_indices <- createDataPartition(y = cars[['price']], 
                                     p = 0.8, 
                                     list = FALSE)

train_listings <- cars[train_indices,]
test_listings <- cars[-train_indices,]
train_control <- trainControl(method = 'cv',
                              number = 5)

knn_model <- train(price ~ horsepower + curb_weight + num_cylinders + engine_size + width + length + city_mpg,
                   data = train_listings,
                   method = 'knn',
                   trControl = train_control,
                   preProcess = c("center", "scale"))

knn_model

'''
Gauging from multiple partitions, k = 5 looks to have the lowest RMSE values.
'''
predictions <- predict(knn_model, newdata = test_listings)
postResample(pred = predictions, obs = test_listings$price)

'''
The lowest RMSE achieved was 1676.24 off of 7 parameters.
'''
