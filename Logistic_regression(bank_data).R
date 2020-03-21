# Output variable -> y
# y -> Whether the client has subscribed a term deposit or not 
# Binomial ("yes" or "no")

bank_data<-read.table("bank-full.csv",sep=";",head=TRUE)
View(bank_data)
#attach(bank_data)
# display first 10 rows of data
head(bank_data, n=10)
# display the dimensions of the dataset
dim(bank_data)
summary(bank_data)
bank_data<-bank_data[-c(9,16)] # removing features contact and p_outcome since it has large number of unknown data
str(bank_data)


# list types for each attribute
sapply(bank_data, class)

# standard deviations and mean for each class
sapply(bank_data[,c(1,6,10,12:15)], mean)
sapply(bank_data[,c(1,6,10,12:15)], sd)
xn<-colnames(bank_data[,c(1,6,10,12:15)])
x<-c(c(1,6,10,12:15))
z<-sapply(bank_data[,c(1,6,10,12:15)], mean)
barplot(z, main = "Average Value of Feature",
        xlab = "Feature Name",
        ylab = "Average Value")

barplot(sapply(bank_data[,c(1,6,10,12:15)], sd), main = "Standard Devaition of Feature",
        xlab = "Feature Name",
        ylab = "Standard Devaition")

#table of output for bank_data
table(bank_data$y)
# table or proportation of enteries in the datasets.
round(prop.table(table(bank_data$y))*100,1)
# distribution of class variable
z <- as.factor(bank_data$y)
cb <- cbind(freq=table(z), percentage=prop.table(table(z))*100)
barplot(table(z), main = "Frequency Distribution of All Classes",
        xlab = "Class Name",
        ylab = "Number of Data Points", legend = TRUE)

# calculate a correlation matrix for numeric variables
library(corrplot)
correlations <- cor(bank_data[,c(1,6,10,12:15)])
# display the correlation matrix
print(correlations)
corrplot(correlations, method = "circle")


#Encoding Categorical Data
bank_data$y<-factor(bank_data$y,
                        levels = c('no','yes'),
                        labels = c(0, 1))

bank_data$job<-factor(bank_data$job,
                      levels = c("admin.","unemployed","management","housemaid",
                                 "entrepreneur","student","blue-collar",
                                 "self-employed","retired","technician",
                                 "services"),
                      labels = c(1:11))
bank_data$marital<-factor(bank_data$marital,
                      levels = c("married","divorced","single"),
                      labels = c(1:3))

bank_data$education<-factor(bank_data$education,
                          levels = c("secondary","primary","tertiary"),
                          labels = c(1:3))
bank_data$default<-factor(bank_data$default,
                    levels = c('no','yes'),
                    labels = c(0, 1))
bank_data$month<-factor(bank_data$month,
                          levels = c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'),
                          labels = c(1:12))
bank_data$housing<-factor(bank_data$housing,
                          levels = c('no','yes'),
                          labels = c(0, 1))
bank_data$loan<-factor(bank_data$loan,
                          levels = c('no','yes'),
                          labels = c(0, 1))
summary(bank_data)

# Taking care of missing data
#install.packages("mlr")
??mlr
library(mlr)
imp <- impute(bank_data, classes= list(factor = imputeMode(), integer = imputeMean()), 
              dummy.classes = c("integer","factor"), dummy.type = "numeric")
?impute
imp_data<-imp$data
summary(imp_data)
attach(imp_data)
imp_data1<-imp_data[-c(16,17)]

#fitting model by logistic regression
model1<-glm(y~.,family = "binomial",data=imp_data1)
summary(model1)
library("MASS")
stepAIC(model1)

#fitting model by logistic regression by eliminating insignificant features(age and default)
model2<-glm(formula = y ~ job + marital + education + balance + housing + 
              loan + day + month + duration + campaign + pdays + previous, 
            family = "binomial", data = imp_data1)
summary(model2)
stepAIC(model2)

exp(coef(model2))
table(imp_data1$y)

#confusion matrix
prob<-predict(model2,type=c("response"),data = imp_data1[-15])
y_pred<-ifelse(prob>0.5,1,0)
confusion<-table(prob>0.5,bank_data$y)
confusion

# Model Performance 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
Error <- 1-Accuracy
Error
Recal<-confusion[4]/(confusion[3]+confusion[4]) #TPR
Recal
Precision<-confusion[4]/(confusion[2]+confusion[4])
Precision
F1_score<-2*Precision*Recal/(Precision+Recal)
F1_score
Specificity<-confusion[1]/(confusion[1]+confusion[2])#TNR
Specificity

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,imp_data1$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

#fitting model by logistic regression by eliminating insignificant features(day)
model3<-glm(formula = y ~ job + marital + education + balance + housing + 
              loan + month + duration + campaign + pdays + previous, 
            family = "binomial", data = imp_data1)
summary(model3)
stepAIC(model3)

exp(coef(model3))
table(imp_data1$y)

#confusion matrix
prob<-predict(model3,type=c("response"),data = imp_data1[-15])
y_pred<-ifelse(prob>0.5,1,0)
confusion<-table(prob>0.5,bank_data$y)
confusion

# Model Performance 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
Error <- 1-Accuracy
Error
Recal<-confusion[4]/(confusion[3]+confusion[4]) #TPR
Recal
Precision<-confusion[4]/(confusion[2]+confusion[4])
Precision
F1_score<-2*Precision*Recal/(Precision+Recal)
F1_score
Specificity<-confusion[1]/(confusion[1]+confusion[2])#TNR
Specificity

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,imp_data1$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
