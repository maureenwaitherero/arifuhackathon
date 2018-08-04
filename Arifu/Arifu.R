#Authur: Maureen Waitherero Wachira ;  
#For:ARIFU HACKATHON
#Date: 4TH AUGUST 2018

library(tidyr)
library(dplyr)
library(lubridate)

getwd() # find current working directory

setwd("~/Documents/Arifu/") # set working directory

# read data file
data <- read.csv(file.choose())

#attach data
attach(data)

# check structure of dataframe
str(data) 


ts1 <- cbind(data$variation_code, data$created_at)
ts(ts1)

#-----Q1-----#
#422868  were in the housing trainings project.


#-----Q2------#

Q2 <- data %>% 
  group_by(program_code) %>%
  summarise(no_rows = length(program_code))

#-----Q3------#

Q3 <- data %>% 
  group_by(learner_id) %>%
  summarise(no_rows = length(variation_code)) 

#Q3 MAXIMUM value
apply(Q3,2,max)

#Q3 MINIMUM value
apply(Q3,2,min)




#-----Q4------#

Q4 <-subset(Q3, no_rows >= 100);Q4
 #767learners

#<100
Q41 <-subset(Q3, no_rows <= 100);Q41

#=100
Q42 <-subset(Q3, no_rows = 100);Q42
summary(Q42$no_rows)

# Distribution plot
hist(Q42$no_rows)



date_ts1 <- as.Date(data$created_at)    # convert it to date-time class

ts1 <- cbind(data$variation_code, date_ts1)
 
ts2<-ts(ts1)#convert to time series


date_ts2 <- as.Date(data$created_at) %>%   # convert it to date-time class
  as.POSIXlt(ts1$date_ts1)

unclass(date_ts2)

date_ts2$mday #extract day of month


#challenge2

# read data file
data2 <- read.csv(file.choose())
