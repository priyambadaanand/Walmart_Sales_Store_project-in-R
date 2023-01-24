
# Walmart Store Sales data analysis by Priyambada Anand.

#Import walmart dataset
walmart_data<- read.csv("Walmart_Store_sales.csv",header=TRUE,sep="," )
# Dataset has 6435  Obs of 8 variable

# Data exploration: Independent Variable = Weekly_Sales , Dependent variables other 7 variables
str(walmart_data)
names(walmart_data)
summary(walmart_data)
table(walmart_data$Store)
table(walmart_data$Holiday_Flag)
colSums(is.na(walmart_data)) #Observed no NA values
all(duplicated(walmart_data) == TRUE)#observed no duplicate values

#Which store has maximum sales
store_sales<- aggregate(Weekly_Sales ~ Store, data=walmart_data,sum )

store_sales

library(dplyr)
max_store_salessum<-max(store_sales$Weekly_Sales)
max_store_salessum # Maximum sales amount 301397792
max_sales_store_num <- store_sales[store_sales$Weekly_Sales==max_store_salessum,1]
max_sales_store_num #Store number 20 with highest sale

#Which store has maximum standard deviation i.e., the sales vary a lot. 
#Also, find out the coefficient of mean to standard deviation

store_std_dev<- aggregate(Weekly_Sales ~ Store, data=walmart_data,sd )
store_std_dev
store_mean <- aggregate(Weekly_Sales ~ Store, data=walmart_data,mean )
store_mean
store_coef<- numeric()

for (i in 1:length(store_mean$Store)){
  store_coef[i]=store_mean$Weekly_Sales[i]/store_std_dev$Weekly_Sales[i]
  print(store_coef[i])
}

store_stats<- merge(store_std_dev,store_mean,by="Store")
store_stats
store_stats$store_coef<-store_coef
store_final_stats<- merge(store_sales,store_stats,by="Store")
store_final_stats <- plyr :: rename(store_final_stats,c("Weekly_Sales.x"="std","Weekly_Sales.y"="mean"))
store_final_stats

max_store_std_dev<- max(store_final_stats$std)
max_store_std_dev # max std_dev= 317569.9
store_num_std <- store_final_stats[store_final_stats$std==max_store_std_dev,1]
store_num_std # store with max std_dev = 14

max_store_coef<- max(store_final_stats$store_coef)
max_store_coef # max coefficient of mean= 23.76193
store_num_coef <- store_final_stats[store_final_stats$store_coef==max_store_coef,1]
store_num_coef # store with max coefficient of mean = 37

#Which store/s has good quarterly growth rate in Q3’2012

walmart_data$Date<- as.Date(walmart_data$Date, format="%d-%m-%Y")# Date is in factor. Convert it to data format.
class(walmart_data$Date)

quar2_data<- walmart_data[walmart_data$Date>="2012-04-01" & walmart_data$Date<="2012-06-30", ]
Quar2_sales<- aggregate(Weekly_Sales ~ Store, data=quar2_data , sum)
Quar2_sales <- plyr :: rename(Quar2_sales,c("Weekly_Sales"="Q2_sales"))
Quar2_sales

quar3_data<- walmart_data[walmart_data$Date>="2012-07-01" & walmart_data$Date<="2012-09-30", ]
Quar3_sales<- aggregate(Weekly_Sales ~ Store, data=quar3_data , sum)
Quar3_sales <- plyr :: rename(Quar3_sales,c("Weekly_Sales"="Q3_sales"))
Quar3_sales

sales_qart<- merge(Quar2_sales,Quar3_sales,by="Store")
sales_qart

library(tidyverse)
sales_qart<- mutate(sales_qart,growth=Q3_sales-Q2_sales)
sales_qart
arrange(sales_qart,desc(sales_qart$growth)) # Store 7,16,26,39,35,41,24,23,40,44 shows growth in Quarter3-2012

# Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together
library(lubridate)
Super_Bowl= dmy(c("12-Feb-10", "11-Feb-11", "10-Feb-12", "8-Feb-13"))
Super_Bowl
Labour_Day = dmy(c("10-Sep-10", "9-Sep-11", "7-Sep-12", "6-Sep-13"))
Labour_Day
Thanksgiving= dmy(c("26-Nov-10", "25-Nov-11", "23-Nov-12", "29-Nov-13"))
Thanksgiving
Christmas= dmy(c("31-Dec-10", "30-Dec-11", "28-Dec-12", "27-Dec-13"))
Christmas

walmart_data_h<- dplyr::select(walmart_data,Date,Weekly_Sales)

walmart_data_h$holiday<- ifelse(walmart_data_h$Date %in% Super_Bowl,'super bowl',
                 ifelse(walmart_data_h$Date %in% Labour_Day ,'Labour Day ',
                        ifelse(walmart_data_h$Date %in% Thanksgiving, 'Thanksgiving',
                               ifelse(walmart_data_h$Date %in% Christmas,'Christmas','Non_holiday' ))))

holiday_sales_mean<- aggregate(Weekly_Sales ~ holiday,walmart_data_h, mean )
holiday_sales_mean # Super Bowl, Thanksgiving and Labor Day holiday sales means are greater than non-holiday sales mean. Non holiday sales mean is = 1041256.4 

#Provide a monthly and semester view of sales in units and give insights

monthly_sales<- dplyr::select (walmart_data, Store,Date,Weekly_Sales)
monthly_sales
#Month column,Year column,Semester column
monthly_sales<- mutate(monthly_sales, month=month(Date),year=year(Date),semester=semester(Date))
monthly_sales
# Summary of sales by month and year
monthyear_salessum <- aggregate(Weekly_Sales ~ Store+year+month, monthly_sales, sum)
monthyear_salessum
arrange(monthyear_salessum,desc(Weekly_Sales),group=1)
# Summary of sales by semester and year
semester_salessum <- aggregate(Weekly_Sales ~ Store+year+semester, monthly_sales, sum)
semester_salessum

library(ggplot2)
# 2010,2011,2012 monthly plot
month_plot<-ggplot(monthyear_salessum, aes(x=factor(Store), y = Weekly_Sales,color=month))+
  geom_col(aes(fill=month))+
  labs(title='Store sales per month',x='Store',y='month')+
  stat_smooth() +
  facet_wrap(~year)

month_plot
# Semester plot
sem_plot<-ggplot(semester_salessum, aes(x=factor(Store), y = Weekly_Sales,color=factor(semester)))+
      geom_col(fill='Light blue')+
      labs(title='Store sales per semester',x='Store',y='Semester')
sem_plot
  
#For Store 1 – Build  prediction models to forecast demand

# Select store 1 data regression model
walmart_data
Walmart_store1 <- walmart_data[walmart_data$Store %in% 1,]
Walmart_store1

#Change dates into days by creating new variable
#Walmart_store1$Date<- as.Date(Walmart_store1$Date, format="%d-%m-%Y")# Date is in factor. Convert it to data format.
Walmart_store1 <- mutate(Walmart_store1, day=day(Date))
Walmart_store1<- subset(Walmart_store1, select = -c(Store,Date,Holiday_Flag))


# Correlation matrix for variables in Walmart_store1 dataset
walmart_mcc <- cor(Walmart_store1[, 1:6]) # Contains the correlation matrix

View(walmart_mcc)

# Correlation Matrix shows Weekly_sales has -ve correlation with temperature,Unemployment and day .
# CPI and Fuel_price has a +ve relationship to weekly sales.

# sample the input data with 70% for training and 30% for testing
library(caTools)
set.seed(1)
# sample the input data with 70% for training and 30% for testing
sample <- sample.split(Walmart_store1$Weekly_Sales,SplitRatio=0.70)
sample
train_data <- subset(Walmart_store1,sample==TRUE)#split of the data into train and test data
test_data <- subset(Walmart_store1,sample==FALSE)

#model building
model <- lm(Weekly_Sales ~., data = train_data)

summary(model)
#Fuel price and day has p-value> 0.05. Drop these variables to build new model
model1 <- lm(Weekly_Sales ~ Temperature+CPI+Unemployment, data = train_data)

summary(model1)


#Unemployment has p-value> 0.05. Drop this variables to build new model
model2 <- lm(Weekly_Sales ~ Temperature+CPI, data = train_data)

summary(model2)

# Predict model with test data

test_pre<- predict(model2,test_data)
head(test_pre)
test_pre<- data.frame(test_pre)
#predicted weekly sales for test data
final_test_data<-cbind(test_data,test_pre)

#cal rmse 
sqrt(mean((final_test_data$Weekly_Sales - final_test_data$test_pre)^2)) 
# RMSE is way greater than 10 which shows this model is not a good fit



