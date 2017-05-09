rm(list = ls())
#setwd("D:/Practice/R/AnalyticsVidya/Loan Prediction 3/")
setwd("C:/data/loan-av-1/")
train = read.csv("train.csv",na.strings = c("","NA"))
test = read.csv("test.csv",na.strings = c("","NA"))

library(mice)
library(caTools)  
library(caret)
library(ggplot2)  
library(dplyr)

train$Loan_ID = NULL
test_id = test$Loan_ID
test$Loan_ID = NULL
loan_status = train$Loan_Status
train$Loan_Status = NULL
str(loan_status)

# ###Trying Mice Imputaiton
# imputed_train = mice(train,m = 5,method = "cart")
# summary(imputed)
# train_new = complete(imputed_train,1)
# 
# imputed_test = mice(test,m = 5,method = "cart")
# test_new = complete(imputed_test,1)

# ###Dropping the NA values
#train_new = train[complete.cases(train),]

full = rbind(train,test)
str(full)

#GENDER
summary(full$Gender)

ggplot(train,aes(x = Gender,y = ApplicantIncome)) +
  geom_bar(stat = "identity")

table(train$Gender,loan_status)
 #Imputing with Mode
 full$Gender[is.na(full$Gender)] = 'Male'
 summary(full$Gender)
 full$Gender = factor(full$Gender)
 
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
which(is.na(full$Married))
full = full[!is.na(full$Married),]
full$Married = factor(full$Married)
str(full$Married)
loan_status = loan_status[-c(105,229,436)]
length(loan_status)
##Dependents
ggplot(full,aes(x = Dependents,y = Married)) +
        geom_jitter()

ggplot(full,aes(x = Dependents,y = ApplicantIncome)) + 
        geom_boxplot()

summary(full$Dependents)
summary(train$Dependents)
full$Dependents[is.na(full$Dependents) & full$Married == "No"] = 0

levels(full$Dependents)[4] = 3
sum(is.na(full$Dependents[full$Married == "Yes"]))
table(full$Dependents[full$Married == "Yes"])
mean(as.numeric(full$Dependents[full$Married == "Yes"]),na.rm = T)
full$Dependents[is.na(full$Dependents) & full$Married == "No"] = 0
full$Dependents[is.na(full$Dependents) & full$Married == "Yes"] = 2

full$Dependents = factor(full$Dependents)

##Education
summary(full$Education)
str(full$Education)

##Self-Employed
summary(full$Self_Employed)
summary(train$Self_Employed)

table(full$Self_Employed,full$ApplicantIncome)
ggplot(full,aes(x = Self_Employed,y = Dependents)) +
      geom_jitter()
summary(full$ApplicantIncome[full$Self_Employed == "No"])
summary(full$ApplicantIncome[full$Self_Employed == "Yes"])
full[full$ApplicantIncome > 50000,]
# full$Self_Employed[full$Self_Employed == ''] = 'No'
# full$Self_Employed = factor(full$Self_Employed)
# ##Applicant-Income
# summary(full$ApplicantIncome)
# which(full$ApplicantIncome == 0)
# 
# ##Coapplicant income
# summary(full$CoapplicantIncome)
# 
# summary(train$ApplicantIncome)
# 
# ##Loan Amount
# summary(full$LoanAmount)
# summary(train$LoanAmount)
# full$LoanAmount[is.na(full$LoanAmount)] = 142.5
# ##Loan Amount Term
# summary(full$Loan_Amount_Term)
# summary(train$Loan_Amount_Term)
# 
# full$Loan_Amount_Term[is.na(full$Loan_Amount_Term)] = 360
# 
# ##Credit History
# summary(full$Credit_History)
# summary(train$Credit_History)
# full$Credit_History = factor(full$Credit_History)
# full$Credit_History[is.na(full$Credit_History)] = 2
# 
# ##Property_Area
# summary(full$Property_Area)
# 
# ##New variable EMI
# full$EMI = full$LoanAmount/(full$Loan_Amount_Term/30)
# head(full$EMI)
# summary(full$EMI)
# full[full$EMI == 650,]
# 
# ##New Variable - Total Income
# 
# full$totalI = NULL
# #divinding data back
# length(loan_status)
# train_new = full[1:611,]
# test_new = full[612:978,]

train_new$Loan_Status = loan_status

#Splitting the training data
split = sample.split(train_new$Loan_Status,SplitRatio = 0.65)
tr = subset(train_new,split == T)
te = subset(train_new,split == F)

# #Logistic Model
# log_model = glm(Loan_Status ~ .,data =tr,family = binomial())
# summary(log_model)
# log_model.pred = predict(log_model,newdata = te,type = "response")
# table(log_model.pred>0.5,te$Loan_Status)
# 
# step(log_model)
# log_model_step = glm(formula = Loan_Status ~ Married + Credit_History + Property_Area, 
#                      family = binomial(), data = tr)
# summary(log_model_step)
# log_model_step.pred = predict(log_model_step,newdata = te,type = "response")
# table(te$Loan_Status,log_model_step.pred > 0.5)
# 
# ##CART
# library(rpart)
# library(rpart.plot)
# cart_model = rpart(Loan_Status ~ .,data =tr)
# prp(cart_model)
# cart_model.pred = predict(cart_model,newdata = te,type = "class")
# table(te$Loan_Status,cart_model.pred)
# 
# #RandomForest
# library(randomForest)
# RF_model = randomForest(Loan_Status ~ .,data =tr)
# RF_model.pred = predict(RF_model, newdata = te)
# table(te$Loan_Status,RF_model.pred)
# 
# #LDA
# library(MASS)
# lda_model = lda(Loan_Status ~ .,data =tr)
# lda_model.pred = predict(lda_model,newdata = te,type = "response")
# table(lda_model.pred$class,te$Loan_Status)


##Trying XGBoost

#Converting Gender to numeric
#test_new = test
#xg_te = test_new
str(tr)
head(tr$Gender)
tr$Gender = ifelse(tr$Gender == "Male",1,0)
te$Gender = ifelse(te$Gender == "Male",1,0)
test_new$Gender = ifelse(test_new$Gender == "Male",1,0)
str(tr$Gender)

#Married
tr$Married = ifelse(tr$Married == "Yes",1,0)
te$Married = ifelse(te$Married == "Yes",1,0)
test_new$Married = ifelse(test_new$Married == "Yes",1,0)
str(tr$Married)

#Dependents
levels(tr$Dependents)[4] = 3
tr$Dependents = as.numeric(tr$Dependents)
levels(te$Dependents)[4] = 3
te$Dependents = as.numeric(te$Dependents)
levels(test_new$Dependents)[4] = 3
test_new$Dependents = as.numeric(test_new$Dependents)


#Education
tr$Education = ifelse(tr$Education == "Not Graduate",1,0)
te$Education = ifelse(te$Education == "Graduate",1,0)
test_new$Education = ifelse(test_new$Education == "Graduate",1,0)


#Self Employed
tr$Self_Employed = ifelse(tr$Self_Employed == "Yes",1,0)
te$Self_Employed = ifelse(te$Self_Employed == "Yes",1,0)
test_new$Self_Employed = ifelse(test_new$Self_Employed == "Yes",1,0)

#Credit History
head(tr$Credit_History)
tr$Credit_History = as.numeric(tr$Credit_History)
te$Credit_History = as.numeric(te$Credit_History)
test_new$Credit_History = as.numeric(test_new$Credit_History)

#Property_area
tr$Property_Area = as.numeric(tr$Property_Area)
te$Property_Area = as.numeric(te$Property_Area)
test_new$Property_Area = as.numeric(test_new$Property_Area)

library(xgboost)
tr_sp = sparse.model.matrix(Loan_Status ~ . -1,data = tr)
te_sp = sparse.model.matrix(Loan_Status ~ . -1,data = te)
length(tr_sp)

tr_sp1 = tr
tr_sp1$Loan_Status = NULL
te_sp1 = te
te_sp1$Loan_Status = NULL

loan_tr = tr$Loan_Status
loan_te = te$Loan_Status

levels(loan_tr) = 0:1
levels(loan_tr)
length(loan_tr)

xg_model = xgboost(data = as.matrix(tr_sp1),label = as.matrix(loan_tr),
                   objective = "binary:logistic",max.depth = 2,
                   eta = 0.1,nthread = -1,nround = 10)

xg_model.pred = predict(xg_model,as.matrix(te_sp1))
table(te$Loan_Status,xg_model.pred > 0.5)
(112+23)/(112+23+29+4)

train_bind = rbind(tr,te)
length(train_bind)
nrow(train_bind)

loan_tr_st = train_bind$Loan_Status
train_bind$Loan_Status = NULL
levels(loan_tr_st) = 0:1

final_xg = xgboost(data = as.matrix(train_bind),label = as.matrix(loan_tr_st),
                   objective = "binary:logistic",max.depth = 2,
                   eta = 0.1,nthread = -1,nround = 3)

final_xg.pred = predict(xg_model,as.matrix(test_new))
head(final_xg.pred)
length(final_xg.pred)
output = ifelse(final_xg.pred >= 0.5,"Y","N")
output
final_Submit = data.frame(test_id,output)
colnames(final_Submit) = c("Loan_ID","Loan_Status")
write.csv(final_Submit,"sub1.csv",row.names = F)
