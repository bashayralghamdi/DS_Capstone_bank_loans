#title: Bank loans
#author: Bashayr Alghamdi
#date:6/12/2020
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


#package 
library(rio) #reading xlsx file
library(tidyverse)
library(Hmisc)
library(rsample) #data split 
library(caret) #for near zero var preprocess
library(recipes) #for preprocess blueprint
library(vip) # for important feature  
library(ranger) # for random forest model
library(reshape2)# for table of correlation 
library(scales)#for scale yaxis 
library(ggeasy)#title of plot

#read data file 
loans <- import("Data/social-development-bank-loans-for-2019.xlsx")



dim(loans)
#there 15 features and 85426 observation 


#ID: unique id for each row
length(unique(loans$ID)) == nrow(loans)

# branch: there are 27 branches 
loans %>% 
  count(branch) %>%  
  mutate(branch=fct_reorder(branch,n)) %>% 
  ggplot(aes(branch,n,fill=branch)) +
  geom_col()+
  geom_text(aes(label = n),
            size = 3,
            hjust = 0,
            vjust = 0)+
  coord_flip()+
  labs(y = "count") 



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
  geom_col(fill="#2b9195")+
  geom_text(aes(label = n),
            size = 4,
            hjust = 0.5,
            vjust = -0.5)+
  labs(y = "count",x="month")+
  scale_x_discrete(labels = c("Jan","Feb","Mar",
                              "Apr","May","June",
                              "July","Aug","Sep",
                              "Oct","Nov","Dec")) +
  theme_classic()



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
  geom_col(fill="#2b9195")+
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

#for each type what the limit of amount of loan
loans %>% 
  ggplot(aes(x=type, y=loan_amount, fill=type))+
  geom_boxplot(fill=c("#80c062","#2b9195","#0b88b3"))+
  stat_summary(fun.data=mean_sdl, 
               geom="pointrange")+
  facet_wrap(~type,scales = "free")+
  ggtitle("loan amount for each type\n") +
  scale_y_continuous(labels = comma_format(big.mark = ".",
                                           decimal.mark = ","))+
  theme(plot.title = element_text(hjust = 0.5))

#summary of each type(max,min,mean and sum of loan amount)
loans %>% 
  group_by(type) %>% 
  summarise(mean=mean(loan_amount),maximum=max(loan_amount),minimum=min(loan_amount),sum=sum(loan_amount))



# class: for each type there are class
loans %>% 
  count(type, class) %>% 
  mutate(class=fct_reorder(class,n)) %>% 
  ggplot(aes(class,n,fill = type)) +
  ggtitle("Classes for each type of loans\n") +
  scale_fill_manual(values=c("#80c062","#2b9195","#0b88b3"))+
  geom_col(position = "dodge")+
  geom_text(aes(label = n),
            size = 4,
            hjust = 0.5,
            vjust = 0)+
  facet_wrap(~type,scales = "free")+
  labs(y = "count",x="classes")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
       legend.position = "none",plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = comma_format(big.mark = ".",
                                           decimal.mark = ","))


#the range of loan amount for each type of business, salary and it installment
loans %>% 
  group_by(type ,installment,salary) %>% 
  summarise(minimum=min(loan_amount),maximum=max(loan_amount),mean=mean(loan_amount)) %>% 
  ggplot(aes(x=salary, y=mean, group=type, color=installment)) + 
  geom_line() +
  geom_point()+
  geom_text(aes(label=sprintf("%0.1f", round(mean, digits = 2))),
            size = 4,
            hjust = 0.5,
            vjust = -1,
            color="black")+
  geom_errorbar(aes(ymin=minimum, ymax=maximum), width=.2,
                position=position_dodge(0.05))+
 
  scale_color_manual(values=c("#80c062","#2b9195","#0b88b3"))+
  facet_grid(type ~installment )+
  theme_classic()+
  scale_y_continuous(labels = comma_format(big.mark = ".",
                                           decimal.mark = ","))

#amount of loan depend on status and family members
loans %>%   
  group_by(status,family_members) %>% 
  summarise( mean=mean(loan_amount),maximum=max(loan_amount),minimum=min(loan_amount)) %>% 
  ggplot(aes(x=status, y=mean, group=family_members, color=family_members)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=minimum, ymax=maximum), width=.2,
                position=position_dodge(0.05))+
  scale_color_manual(values=c("#80c062","#2b9195","#0b88b3","#1c3c6f"))+
  facet_wrap(~family_members,scales = "free")+
  theme_classic()+
  scale_y_continuous(labels = comma_format(big.mark = ".",
                                           decimal.mark = ","))



# loan_amount: amount of loans(10,000 - 4,000,000)
#amount of loan in general 
loans %>% 
ggplot(aes(x=ID,y=loan_amount))+
  geom_jitter(aes(colour = type))+
  scale_color_manual(values=c("#80c062","#2b9195","#1c3c6f"))+
  scale_y_continuous(labels = comma_format(big.mark = ".",
                                           decimal.mark = ","))+
  scale_x_continuous(labels = comma_format(big.mark = ".",
                                           decimal.mark = ","))+
  ggtitle("amount of loan \n") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()


#total loan amount for each type
loans %>% 
  group_by(type) %>% 
  summarise(max=max(loan_amount),min=min(loan_amount),sum=sum(loan_amount)) %>% 
  ggplot( aes(x="", y=sum, fill=type))+
  geom_bar(width = 1, stat = "identity",color="white")+
  coord_polar("y", start=0)+
  scale_fill_manual(values=c("#80c062","#2b9195","#1c3c6f")) +
  theme_void()+
  labs(title = "total of loan amount")

  
  
#machine learning
#split the data
set.seed(123)
split  <- initial_split(loans, prop = 0.7, strata = "loan_amount")
loans_train  <- training(split)
loans_test   <- testing(split)

# Do the distributions line up?  
ggplot(loans_train, aes(x = loan_amount)) + 
  geom_line(aes(col="train"),
            stat = "density", 
            trim = TRUE,col = "black") + 
  geom_line(aes(col="test"),
            data = loans_test, 
            stat = "density", 
            trim = TRUE, col = "red")+
  scale_x_continuous(limits = c(0,150000))# to be clear


#the target is positive skewed 
boxplot(loans_train$loan_amount,main="Boxplot",horizontal = TRUE)

summary(loans_train$loan_amount)
Q1 <-  42000
Q3 <-  60000
IQR <-  Q3 - Q1


Q3+(IQR)*1.5 #greater than 87000 is outlier
Q1-(IQR)*1.5 #less than 15000 is outlier


out <- boxplot.stats(loans_train$loan_amount)$out
length(out)#there are about 2300 outliers


#the distribution of the target 
#the target is numeric feature
loans_train%>% 
  ggplot(aes(loan_amount))+
  geom_histogram(fill="#2b9195")

  
  #positive distribution 

loans_train%>% 
  ggplot(aes(log10(loan_amount)))+
  geom_histogram()

loans_train%>% 
  ggplot(aes(sqrt(loan_amount)))+
  geom_histogram()

loans_train%>% 
  ggplot(aes(forecast::BoxCox(loan_amount,lambda = 0)))+
  geom_histogram(fill="#2b9195")+
  labs(y = "loan_amount",x="normaliezed",title = "   normalize the distribution\n")
  

#missing value
sum(is.na(loans_train))#there is 9 missing value


#remove missing value
loans_train <- loan_train %>% 
  na.omit()

# the variance across the features
zero_var <- nearZeroVar(loans_train, saveMetrics= TRUE) 

zero_var %>% 
  filter(zeroVar==TRUE | nzv==TRUE )
#type of loan and disabled features is near to zero


# the categorical features
#change all categorical feature to factor type
loans_train[sapply(loans_train, is.character)] <- lapply(loans_train[sapply(loans_train, is.character)], 
                                       as.factor)



#all features is categorical except the target 

blueprint <- recipe(loan_amount ~ ., data = loans_train) %>%
  step_nzv(type,disabled) %>%
  step_integer(installment,age,family_members,salary) %>% 
  step_dummy(all_nominal(), one_hot = TRUE) %>% 
  step_YeoJohnson(loan_amount) 


# create a resampling method
cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 5
)

#linear model

set.seed(123)
cv_model <- train(
  loan_amount ~ .,
  data = loans_train, 
  method = "lm",
  trControl = cv
)
#RMSE      Rsquared   MAE    
#26783.53  0.8091587  4701.47

set.seed(123)
cv_model_bp <- train(
  blueprint, 
  data = loans_train, 
  method = "lm",
  trControl = cv
)
#RMSE        Rsquared   MAE        
#0.01157444  0.8367604  0.005439656
vip(cv_model_bp)

#K-nearest neighbors model
cv_k <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 5,
  classProbs = TRUE,                 
  summaryFunction = twoClassSummary
)

# Create a hyperparameter grid search
hyper_grid_k <- expand.grid(
  k = floor(seq(1, nrow(loans_train)/3, length.out = 20))
)

# create a hyperparameter grid search
hyper_grid <- expand.grid(k = seq(1, 20, by = 2))


knn_fit_bp <- train(
  blueprint, 
  data = loans_train, 
  method = "knn", 
  trControl = cv_k, 
  tuneGrid = hyper_grid_k,
  metric = "RMSE"
)


#k   RMSE       Rsquared      MAE      
#1  0.9249365  0.0336901709  0.2494145
#3  0.9155264  0.0205271985  0.2679265  ###best fit
#5  0.9198624  0.0108576559  0.2860523
#7  0.9225220  0.0062043513  0.2969513
#9  0.9236239  0.0040525607  0.3016562
#11  0.9244081  0.0025842406  0.3025753
#13  0.9244139  0.0017667946  0.3009780
#15  0.9241501  0.0012817420  0.2984068
#17  0.9240094  0.0010086844  0.2957839
#19  0.9239290  0.0008098471  0.2934000

#RMSE was used to select the optimal model using the smallest value.
#The final value used for the model was k = 3.


#k   RMSE        Rsquared      MAE       
#1  0.03679410  0.0150426979  0.02196157
#3  0.03195852  0.0046647908  0.02252592
#5  0.03095562  0.0013738040  0.02240612
#7  0.03040140  0.0006895855  0.02209874
#9  0.03005556  0.0004575714  0.02187220
#11  0.02984470  0.0003189310  0.02170517
#13  0.02968814  0.0001913861  0.02157939
#15  0.02955183  0.0001754910  0.02147338
#17  0.02945072  0.0001510519  0.02139324
#19  0.02936938  0.0001302248  0.02132956  ## best fit

#RMSE was used to select the optimal model using the smallest value.
#The final value used for the model was k = 19.

#Regularized model 

#feature correlation 
cor_matrix <- loans_train %>%
  mutate_if(is.factor, as.numeric) %>%
  cor()

melted_cormat <- melt(cor_matrix)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") 


#Regularized regression
X <- model.matrix(loan_amount ~ ., loans_train)[, -1]

#is there near to zero variance 
zero_var_X <- nearZeroVar(X, saveMetrics= TRUE) 
zero_var_X %>% 
  filter(zeroVar==TRUE | nzv==TRUE )


Y <- log10(loans_train$loan_amount)

hyper_grid_g <- expand.grid(
  alpha = seq(0, 1, by = .25),
  lambda = c(0.1, 10, 100, 1000, 10000)
)

set.seed(123)
cv_glmnet <- train(
  x = X,
  y = Y,
  method = "glmnet",
  preProc = c("nzv"),
  trControl = cv,
  tuneGrid = hyper_grid_g,
  tuneLength = 10
)


cv_glmnet$results %>%
  filter(
    alpha == cv_glmnet$bestTune$alpha,
    lambda == cv_glmnet$bestTune$lambda
  )
#log
#alpha lambda     RMSE  Rsquared       MAE      RMSESD RsquaredSD       MAESD
#1     0    0.1 0.229464 0.7272193 0.1125861 0.006667417 0.01004568 0.002457152

#log10
#alpha lambda      RMSE  Rsquared        MAE      RMSESD RsquaredSD       MAESD
#1     0    0.1 0.1133967 0.6677777 0.05504779 0.003499256 0.01234095 0.001007531

vip(cv_glmnet)


#random forest
features <- setdiff(names(loans_train), "loan_amount")

# perform basic random forest model
fit_default <- ranger(
  formula    = loan_amount ~ ., 
  data       = loans_train, 
  num.trees  = length(features) * 10, #140 trees 
  mtry       = floor(length(features) / 3), #4
  respect.unordered.factors = 'order',
  verbose    = FALSE,
  seed       = 123
)
sqrt(fit_default$prediction.error) #27606.02

#Type:                             Regression 
#Number of trees:                  140 
#Sample size:                      59792 
#Number of independent variables:  14 
#Mtry:                             4 
#Target node size:                 5 
#Variable importance mode:         none 
#Splitrule:                        variance 
#OOB prediction error (MSE):       762092608 
#R squared (OOB):                  0.7827091






