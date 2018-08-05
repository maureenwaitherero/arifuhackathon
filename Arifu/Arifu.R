#Authur: Maureen Waitherero Wachira ;  
#For:ARIFU HACKATHON
#Date: 4TH AUGUST 2018

library(tidyr)
library(dplyr)
library(lubridate)
library(caTools)

getwd() # find current working directory

setwd("~/Documents/Arifu/") # set working directory

# read data file
data <- read.csv(file.choose())

#attach data
attach(data)

# Inspect the data type and levels of the attributes of the dataset
str(data) 




#-----Q2------#

Q2 <- data %>% 
  group_by(program_code) %>%
  summarise(no_rows <- length(program_code)); Q2

#PLB, HFS, SF

#-----Q3------#

Q3 <- data %>% 
  group_by(learner_id) %>%
  summarise(no_rows = length(variation_code)) ;Q3

#Q3 MAXIMUM value
apply(Q3,2,max)

#Q3 MINIMUM value
apply(Q3,2,min)


#-----Q4------#

Q4 <-subset(Q3, no_rows > 100);Q4
str(Q4)


#-----Q5------#
#<=100
Q5 <-subset(Q3, no_rows <= 100);Q5
str(Q5)

Summary(Q5)

# Distribution plot
hist(Q5$no_rows)

#-----Q6------#

date_ts2 <- as.Date(data$created_at) %>%   # convert it to date-time class
  as.POSIXlt(ts1$date_ts1)

unclass(date_ts2)

#group by day of month
dayofmonth <- date_ts2$wday #0-6 starting on Sunday

datedf <- cbind(data, dayofmonth)


Q6 <- datedf %>% 
  group_by(dayofmonth) %>%
  summarise(no_rows = length(variation_code)) ;Q6
#3(wednesday) had most interactions
#1(monday) had least interactions

#challenge2

# read data file
data2 <- read.csv(file.choose())
summary(data2)


sample <- sample.split(data2,SplitRatio = 0.7)
train <- subset(data2,sample ==TRUE)
test <- subset(data2, sample==FALSE)

#convert predictor into factor
data2[,18] <- as.factor(data2[,18])

#logistic regression model
names(data2)
model <- glm (default~ checking_balance + months_loan_duration +credit_history+purpose+amount+savings_balance+employment_length+installment_rate+personal_status+other_debtors+residence_history+property
              +age+installment_plan+housing+existing_credits+dependents+foreign_worker+job,
              data = data2, family = binomial)
summary(model)
