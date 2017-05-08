setwd("D:/Practice/R/AnalyticsVidya/Loan Prediction 3/")
train = read.csv("train.csv")
test = read.csv("test.csv")

library(mice)
library(caTools)  
library(caret)
library(ggplot2)  
library(dplyr)

train$Loan_ID = NULL
str(train)
test_id = test$Loan_ID
test$Loan_ID = NULL

loan_status = train$Loan_Status
train$Loan_Status = NULL
str(loan_status)

full = rbind(train,test)
str(full)

#GENDER
summary(full$Gender)

ggplot(train,aes(x = Gender,y = loan_status)) + 
        geom_jitter()

table(train$Gender,loan_status)
#Imputing with Mode
full$Gender[full$Gender == ''] = 'Male'
summary(full$Gender)
full$Gender = factor(full$Gender)
# 
# male_impute = subset(full,full$Gender == '')
# male_impute_1 = subset(full,full$Gender != '')
# table(is.na(male_impute_1))
# male_impute_1 = na.omit(male_impute_1)
# imp_gender_model = glm(Gender~.,data = male_impute_1,family = binomial())
# step(imp_gender_model)
# summary(imp_gender_model)
# 
# step_imp_gender_model = glm(formula = Gender ~ Married + Dependents + Education + ApplicantIncome + 
#                               CoapplicantIncome + Loan_Amount_Term + Property_Area, family = binomial(), 
#                             data = male_impute_1)
# summary(step_imp_gender_model)
# 
# male_impute$Gender = NULL
# step_imp_gender_model.pred = predict(step_imp_gender_model,newdata = male_impute,type = "response")
# 
# gender_impute <- vector()
# for(i in 1:length(step_imp_gender_model.pred))
# {
#   if(step_imp_gender_model.pred[i] >=0.5)  
#     gender_impute[i] = "Male"
#   else
#     gender_impute[i] = "Female"
# }
# j = 1
# for(i in which(full$Gender ==''))
# {
#   full$Gender[i] = gender_impute[j]
#   j = j + 1
# }
# 
# summary(full$Gender)
# full$Gender = factor(full$Gender)

##Married
summary(full$Married)
summary(train$Married)
which(full$Married == '')
full = full[!full$Married == '',]
full$Married = factor(full$Married)
str(full$Married)
loan_status = loan_status[-c(105,229,436)]
length(loan_status)
##Dependents
summary(full$Dependents)
summary(train$Dependents)
full$Dependents[full$Dependents == ''] = 0
full$Dependents = factor(full$Dependents)

##Education
summary(full$Education)
str(full$Education)

##Self-Employed
summary(full$Self_Employed)
summary(train$Self_Employed)
full$Self_Employed[full$Self_Employed == ''] = 'No'
full$Self_Employed = factor(full$Self_Employed)
##Applicant-Income
summary(full$ApplicantIncome)
which(full$ApplicantIncome == 0)

##Coapplicant income
summary(full$CoapplicantIncome)

summary(train$ApplicantIncome)

##Loan Amount
summary(full$LoanAmount)
which(is.na(full$LoanAmount))

##Loan Amount Term
summary(full$Loan_Amount_Term)
which(is.na(full$Loan_Amount_Term))

##Credit History
summary(full$Credit_History)

##Property_Area
summary(full$Property_Area)

md.pattern(full)

#Splitting out the data
train_new = full[1:611,]
train_new$Loan_Status = loan_status
test_new = full[612:978,]

##Now going to Python
write.csv(train_new,"train_new.csv",row.names = F)
write.csv(test_new,"test_new.csv",row.names = F)

##Splitting data
split = sample.split(train_new$Loan_Status,SplitRatio = 0.6)
tr = subset(train_new,split == T)
te = subset(train_new,split == F)


