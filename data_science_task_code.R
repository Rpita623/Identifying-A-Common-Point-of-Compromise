#Libraries
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)

#Importing given dataset
transactions_dataset <- read.csv("/Users/arpitabhattacharya/Desktop/Warwick /Internship/Data_Science_Task_FeatureSpace/PreInterviewTaskData.csv") 

#Counting the number of transactions per merchant
merchant_count <- transactions_dataset %>% count(transactions_dataset$merchant, sort = TRUE)
names(merchant_count)[names(merchant_count) == 'transactions_dataset$merchant'] <- 'merchant'

#Top 10 merchants with most transactions and the dates of those transactions 
for(i in 1:10) {
  Merchant <- paste("Merchant_", merchant_count$merchant[i], sep = "")
  merchant_nam <- filter(transactions_dataset, transactions_dataset$merchant == merchant_count$merchant[i])
  assign(Merchant, merchant_nam)
  Dates <- paste("Dates_", merchant_count$merchant[i], sep = "")
  Dates_tab <- merchant_nam %>% count(merchant_nam$date, sort = TRUE)
  names(Dates_tab)[names(Dates_tab) == 'merchant_nam$date'] <- 'Dates'
  names(Dates_tab)[names(Dates_tab) == 'n'] <- 'Transactions'
  assign(Dates, Dates_tab)
  print(ggplot(data = Dates_tab, aes(x = Dates, y = Transactions, group = 1, colour  = merchant_count$merchant[i])) + geom_point() + geom_line())
}

#The merchant highest number of transactions   
Dates_most = 0
for(i in 1:10) {
  merchant_nam <- filter(transactions_dataset, transactions_dataset$merchant == merchant_count$merchant[i])
  Dates_tab <- merchant_nam %>% count(merchant_nam$date, sort = TRUE)
  if (Dates_tab[1,2] > Dates_most){
    Dates_most = Dates_tab[1,2]
    Merchant_most = merchant_count$merchant[i]
    merchant_nam_most <- filter(transactions_dataset, transactions_dataset$merchant == merchant_count$merchant[i])
    Dates_tab_most <- merchant_nam_most %>% count(merchant_nam_most$date, sort = TRUE)
  }
}
names(Dates_tab_most)[names(Dates_tab_most) == 'merchant_nam_most$date'] <- 'Dates'
print(Dates_most)
print(Merchant_most)
#Merchant M61 has an unusually high number of transactions. 
  
#New dataset for fraudulent transactions 
fraud_transaction <- filter(transactions_dataset, transactions_dataset$fraud == "True")

FMerchant <- filter(fraud_transaction, fraud_transaction$merchant == Merchant_most)
FDates <- FMerchant %>% count(FMerchant$date, sort = TRUE)
names(FDates)[names(FDates) == 'FMerchant$date'] <- 'Dates'  

ggplot() +
  geom_point(data = Dates_tab_most, aes(x = Dates, y = n, group = 1, colour  = Merchant_most)) + 
  geom_line(data = Dates_tab_most, aes(x = Dates, y = n, group = 1, colour = Merchant_most)) +
  geom_point(data = FDates, aes(x = Dates, y = n, group = 1, colour  = Merchant_most)) + 
  geom_line(data = FDates, aes(x = Dates, y = n, group = 1, colour = Merchant_most)) +
  labs(x = "Dates", y = "Number of Transactions", title = "Transactions")



