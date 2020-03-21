#Assignment on Logistic Regression
#classify that a person had an affair or not 

Affairs<-read.csv("affairs.csv")
summary(Affairs)
View(Affairs)
#attach(Affairs)
# display first 10 rows of data
head(Affairs, n=10)
# display the dimensions of the dataset
dim(Affairs)
str(Affairs)


# list types for each attribute
sapply(Affairs, class)

# standard deviations and mean for each class
sapply(Affairs[,c(3,4,6:9)], mean)
sapply(Affairs[,c(3,4,6:9)], sd)
xn<-colnames(Affairs[,c(3,4,6:9)])
x<-c(c(3,4,6:9))
z<-sapply(Affairs[,c(3,4,6:9)], mean)
barplot(z, main = "Average Value of Feature",
        xlab = "Feature Name",
        ylab = "Average Value")

barplot(sapply(Affairs[,c(3,4,6:9)], sd), main = "Standard Devaition of Feature",
        xlab = "Feature Name",
        ylab = "Standard Devaition")

#table of output for Affairs
table(Affairs$affairs)
# table or proportation of enteries in the datasets.
round(prop.table(table(Affairs$affairs))*100,1)
# distribution of class variable
z <- as.factor(Affairs$affairs)
cb <- cbind(freq=table(z), percentage=prop.table(table(z))*100)
barplot(table(z), main = "Frequency Distribution of All Classes",
        xlab = "Class Name",
        ylab = "Number of Data Points", legend = TRUE)

# calculate a correlation matrix for numeric variables
library(corrplot)
correlations <- cor(Affairs[,c(3,4,6:9)])
# display the correlation matrix
print(correlations)
corrplot(correlations, method = "circle")

#Encoding Categorical Data
Affairs$affairs<-ifelse(Affairs$affairs>0, 1, 0)
Affairs$affairs<-factor(Affairs$affairs,
                        levels = c(0,1),
                        labels = c(0, 1))
Affairs$children<-factor(Affairs$children,
                         levels = c('no','yes'),
                         labels = c(0, 1))
Affairs$gender<-factor(Affairs$gender,
                       levels = c('female','male'),
                       labels = c(0, 1))
summary(Affairs)
attach(Affairs)



# Feature Scaling
Affairs[, c(3,4,7,8,9)] = scale(Affairs[, c(3,4,7,8,9)])


#fitting model1 on Affairs data
model1<-glm(affairs~gender+age+yearsmarried+children+religiousness+education+occupation+rating, 
            data = Affairs,family = "binomial")
summary(model1)
library("MASS")
stepAIC(model1)

# fitting modelafter removing feature occuptation
model2<-glm(affairs~gender+age+yearsmarried+children+religiousness+education+rating, 
            data = Affairs,family = "binomial")
summary(model2)

# fitting modelafter removing feature occuptation and education
model3<-glm(factor(affairs)~factor(gender)+age+yearsmarried+factor(children)+religiousness+rating, 
            data = Affairs,family = "binomial")
summary(model3)

stepAIC(model3) 

# fitting modelafter removing feature occuptation, children and education
model4<-glm(factor(affairs)~factor(gender)+age+yearsmarried+religiousness+rating, 
            data = Affairs,family = "binomial")
summary(model4)

stepAIC(model4) 

exp(coef(model4))
table(Affairs$affairs)

# Confusion matrix table 
prob <- predict(model4,type=c("response"),data = Affairs[-1])
prob
y_pred<-ifelse(prob>0.5,1,0)
confusion<-table(prob>0.5,Affairs$affairs)
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
Precision

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,Affairs$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

