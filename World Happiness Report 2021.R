###########################
# World Happiness Report - 2021
###########################

# Importing the library

library(tidyverse)
library(data.table)
library(corrplot)
library(GGally)
library(rworldmap)
library(caret)
library(data.table)
library(ggplot2)
library(randomForest)
library(car)
library(pscl)

#Importing the data

dh_2021 <- read.csv("world-happiness-report-2021.csv")

# Overview

glimpse(dh_2021)

# The data consists of 149 rows which represents 149 countries and 20 columns. 

# Cleaning the Data

# We need only the columns : Country.name, Regional.indicator, Ladder.score, Logged.GDP.per.capita, Social.support, Healthy.life.expectancy, Freedom.to.make.life.choices, Generosity, Perceptions.of.corruption, Dystopia...residual

dh_2021_new <- dh_2021[c(1:3,7:12)]

# Renaming the columns for the ease of access

dh_2021_new <- dh_2021_new %>% 
  rename("Country" = "ï..Country.name", 
         "region"="Regional.indicator", 
         "GDP"= "Logged.GDP.per.capita", 
         "score" ="Ladder.score", 
         "support" = "Social.support",
         "Life.exp" = "Healthy.life.expectancy", 
         "Freedom" ="Freedom.to.make.life.choices", 
         "corruption"="Perceptions.of.corruption")

# Checking for missing values

colSums(is.na(dh_2021_new))
sapply(dh_2021_new, class)


###########################
# Exploratory Data Analysis
###########################

# Visualizing the happiness data in the year 2021

# Top 10 and bottom 10 countries:

# Top 10 countries:

top10 <- dh_2021_new %>% 
  head(10) %>% 
  mutate(Level = "Top10")

# Visualizing top 10 countries:

ggplot(top10, aes(x= Country,  y=score)) +
  geom_point( shape = 21, 
              fill = "yellow",
              color = "blue", 
              size = 4) +
  ylab("Ladder score") + 
  ggtitle("Top 10 Countries with high Ladder Scores" ) + 
  coord_flip()

# Bottom 10 Countries:

bottom10 <- dh_2021_new %>% 
  tail(10) %>% 
  mutate(Level = "Bottom10")

# Visualizing bottom 10 countries:

ggplot(bottom10, aes(x= Country,  y=score)) +
  geom_point( shape = 21, 
              fill = "yellow",
              color = "blue", 
              size = 4) +
  ylab("Ladder score") +  
  ggtitle("Bottom 10 Countries with high Ladder Scores" ) + 
  coord_flip()

# Plotting happiness by region.

ggplot(dh_2021_new, aes(x=region, 
                        y= score, 
                        colour = region)) + 
  geom_boxplot() + 
  labs(title = "Ladder Score Boxplot",
       x = "Region",
       y = "Ladder Score") +
  theme(axis.text.x = element_text(angle = 35, 
                                   vjust = 1, 
                                   hjust = 1))

# Visualizing happiness on the world map

world2021 <- joinCountryData2Map(dh_2021_new, 
                                 joinCode = "NAME", 
                                 nameJoinColumn = "Country")

par(mar = c(1, 1, 1, 1))
map_2021 <- mapCountryData( world2021, 
                            nameColumnToPlot="score", 
                            addLegend = FALSE)

# Scatter plot matrix

ggpairs(dh_2021_new, 
        columns=c(3:9),
        upper=list(continuous="smooth"),
        lower=list(continuous="smooth"))

# Correlation matrix:
  
corr_dh <- dh_2021_new[c(3:9)]
M <- cor(corr_dh)
corrplot(M, 
         method = "pie", 
         tl.srt=45, 
         type="upper")


###########################
# Analysis
###########################

# Create model:
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y=dh_2021_new$score, 
                                  times = 1, 
                                  p = 0.2, 
                                  list = FALSE)

train_set <- dh_2021_new[-test_index,] 
test_set <- dh_2021_new[test_index,] 

# Method 1 : Linear Regression

model1 <- lm(score ~ GDP +
               support + 
               Life.exp + 
               Freedom +
               Generosity + 
               corruption,
             data = train_set)

summary(model1)

# p-value of Generosity is 0.073519, so we ignore the factor and recreate our model.

model1_re <-lm(score ~ GDP +
     support + 
     Life.exp + 
     Freedom + 
     corruption,
   data = train_set)

summary(model1_re)

# Prediction

predict_model1 <- predict(model1_re, 
                        newdata = test_set)

# RMSE

RMSE(predict_model1, test_set$score)

# We can see that GDP, Social support,  Healthy life expectancy and freedom to make choices are the strongest predictors. 


# Model 2 : Logistics Regression

model2 <- glm(score ~ GDP + 
                support + 
                Life.exp + 
                Freedom +
                corruption,
              data = train_set)

# disable scientific notation for model summary
options(scipen=999) 

summary(model2)

# Assessing the model fit

pR2(model2)["McFadden"]

# Checking for multi-collinearity

vif(model2)

# Prediction: 

predict_model2 <- predict(model2, test_set, type="response")

# RMSE: 

RMSE(predict_model2, test_set$score)


 
