#title: Bank loans
#author: Bashayr Alghamdi
#date:6/12/2020

#package 
library(rio)
library(tidyverse)
library(ggplot2)
library(plotly)
library(plyr)
library(flexdashboard)

#read data file 
loans <- import("Data/social-development-bank-loans-for-2019.xlsx")

#https://www.sdb.gov.sa/en-us/best-value/e-opendata
#Social Development Bank provides its Open Data to all visitors and 
#audiences to the bank’s website in order to enhance the transparency 
#principles and motivate the electronic collaboration. 
#The Bank has shared the data with the Open Data
#(https://www.data.gov.sa/Data/en/organization/social_development_bank) 
#Platform which contains number of files with different data in different 
#formats that make it easy to process and re-use.

#this dataset about Social Development Bank loans for 2019

#Last Updated:	June 23, 2020, 09:44 (AST)

dim(loans)
#there 15 features and 85426 observation 


glimpse(loans)

#ID: unique id for each row
length(unique(loans$ID)) == nrow(loans)

# branch: there are 27 branches 
loans %>% 
  count(branch) %>% 
  mutate(branch=fct_reorder(branch,n)) %>% 
  ggplot(aes(branch,n)) +
  geom_col()+
  geom_text(aes(label = n),
            size = 3,
            hjust = 0,
            vjust = 0)+
  coord_flip()+
  labs(y = "count") 


# type: there are 3 type of finance (business,individual and transportation)
loans %>% 
  count(type) %>% 
  mutate(type = fct_reorder(type,n)) %>% 
  ggplot(aes(type,n)) +
  geom_col(position = "dodge")+
  geom_text(aes(label = n),
            size = 4.5,
            hjust = 0.5,
            vjust = 0)+
  labs(y = "count")

# class: for each type there are class
loans %>% 
  count(type, class) %>% 
  mutate(class=fct_reorder(class,n)) %>% 
  ggplot(aes(class,n)) +
  geom_col(position = "dodge")+
  geom_text(aes(label = n),
            size = 4,
            hjust = 0.5,
            vjust = 0)+
  facet_wrap(~type,scales = "free")+
  labs(y = "count",x="classes",title = "Classes for each type of loans\n")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "none")


# sector:there are three values  (government employee, government retired and undefined)
loans %>% 
  count(sector) %>% 
  mutate(sector=fct_reorder(sector,n)) %>% 
  ggplot(aes(sector,n)) +
  geom_col(position = "dodge")+
  geom_text(aes(label = n),
            size = 4.5,
            hjust = 0.5,
            vjust = 0)+
  labs(y = "count")

# loan_amount: amount of loans(10,000 - 4,000,000)

plot(loans$loan_amount,ylab = "loan amount")
boxplot(loans$loan_amount,main="Boxplot",horizontal = TRUE)

summary(loans$loan_amount)
#Q1 = 42000
#Q3 = 60000
#IQR = 60000 - 42000


60000+(60000 - 42000)*1.5 #greater than 87000 is outlier
42000-(60000 - 42000)*1.5 #less than 15000 is outlier

#there are 3392 outliers
out <- boxplot.stats(loans$loan_amount)$out


# installment: there tow value (greater than or equal 1000 & less than 1000)  
loans %>% 
  count(installment) %>% 
  ggplot(aes(installment,n)) +
  geom_col()+
  geom_text(aes(label = n),
            size = 4.5,
            hjust = 0.5,
            vjust = 0)+
  labs(y = "count")

# transaction_date: date of loan transaction (from first to last month of 2019)
loans %>% 
  count(transaction_date) %>% 
  ggplot(aes(transaction_date,n)) +
  geom_point()+ geom_line() +
  geom_text(aes(label = n),
            size = 4,
            hjust = 0.5,
            vjust = 0)+
  labs(y = "count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# gender: (female and male)  
loans %>% 
  count(gender) %>% 
  ggplot(aes(gender,n)) +
  geom_col()+
  geom_text(aes(label = n),
            size = 4.5,
            hjust = 0.5,
            vjust = 0)+
  labs(y = "count")

# age: there are four category (< 30, >= 30,  >= 40 and >= 60 )
loans %>% 
  count(age) %>% 
  ggplot(aes(age,n)) +
  geom_col()+
  geom_text(aes(label = n),
            size = 4.5,
            hjust = 0.5,
            vjust = 0)+
  labs(y = "count")

# status: there are five category (abandoned, divorced, married, single and widowed)   
loans %>% 
  count(status) %>% 
  ggplot(aes(status,n)) +
  geom_col()+
  geom_text(aes(label = n),
            size = 4.5,
            hjust = 0.5,
            vjust = 0)+
  labs(y = "count")

# disabled: (yes and no)  
loans %>% 
  count(disabled) %>% 
  ggplot(aes(disabled,n)) +
  geom_col()+
  geom_text(aes(label = n),
            size = 4.5,
            hjust = 0.5,
            vjust = 0)+
  labs(y = "count")

# family_members: there are four category (< 02, >= 02, >= 05 and >= 10 )
loans %>% 
  count(family_members) %>% 
  ggplot(aes(family_members,n)) +
  geom_col()+
  geom_text(aes(label = n),
            size = 4.5,
            hjust = 0.5,
            vjust = 0)+
  labs(y = "count")

# saving_loan: (yes and no)
loans %>% 
  count(saving_loan) %>% 
  ggplot(aes(saving_loan,n)) +
  geom_col()+
  geom_text(aes(label = n),
            size = 4.5,
            hjust = 0.5,
            vjust = 0)+
  labs(y = "count")

# salary: there are four category (< 5000, >= 10000, >= 5000 and >= 7500)
loans %>% 
  count(salary) %>% 
  ggplot(aes(salary,n)) +
  geom_col()+
  geom_text(aes(label = n),
            size = 4.5,
            hjust = 0.5,
            vjust = 0)+
  labs(y = "count")









