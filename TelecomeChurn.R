library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)




churn <- read.csv('C://Users/Administrator/Downloads/WA_Fn-UseC_-Telco-Customer-Churn.csv')
View(churn)

sapply(churn, function(x) sum(is.na(x)))

churn <- churn[complete.cases(churn), ]

cols_recode1 <- c(10:15)


for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues(churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))
min(churn$tenure)
max(churn$tenure)


group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}


churn$tenure_group <- sapply(churn$tenure,group_tenure)
View(churn)
churn$tenure_group <- as.factor(churn$tenure_group)

churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

churn$customerID <- NULL
churn$tenure <- NULL

numeric.var <- sapply(churn, is.numeric) ## Find numerical variables

corr.matrix <- cor(churn[,numeric.var])  ## Calculate the correlation matrix
View(corr.matrix)
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numeric Variables", method="number")

churn$TotalCharges <- NULL


p1 <- ggplot(churn, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + 
  coord_flip() + theme_minimal()
p1
p2 <- ggplot(churn, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2
p3 <- ggplot(churn, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3
p4 <- ggplot(churn, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4
grid.arrange(p1, p2, p3, p4, ncol=2)


#Logistic Regression Model Fitting
#Split the data into training and testing sets.

intrain<- createDataPartition(churn$Churn,p=0.7,list=FALSE)

training<- churn[intrain,]
testing<- churn[-intrain,]
dim(training); dim(testing)


LogModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))


anova(LogModel, test="Chisq")


#Assessing the predictive ability of the Logistic Regression model

testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results
fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results
misClasificError <- mean(fitted.results != testing$Churn)
misClasificError
print(paste('Logistic Regression Accuracy',1-misClasificError))

#confusion matrix

table(testing$Churn, fitted.results > 0.5)
#accuracy
(1402+262)/(1402+262+298+146)


#Decision Tree


# we are going to use only three variables for plotting Decision Trees, they are "Contract", "tenure_group" and "PaperlessBilling".

tree <-ctree(Churn~Contract+tenure_group+PaperlessBilling, training)

plot(tree)
fancyRpartPlot(tree)
{{plot(tree,uniform=TRUE,margin=0.2)
  text(tree,use.n=TRUE, all=TRUE, cex=.8)}}


pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testing$Churn)


p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))



rfModel <- randomForest(Churn ~., data = training)
print(rfModel)

pred_rf <- predict(rfModel, testing)
View(pred_rf)
class(pred_rf)
testing$Churn<-as.factor(testing$Churn)

caret::confusionMatrix(pred_rf, testing$Churn)

plot(rfModel)


#Tune Random Forest Model

t <- tuneRF(training[, -18], training[, 18], stepFactor = 0.5, 
            plot = TRUE, ntreeTry = 200, trace = TRUE, improve = 0.05)

# OOB error rate is at the lowest when mtry is 2. Therefore, we choose mtry=2.

#Fit the Random Forest Model After Tuning

rfModel_new <- randomForest(Churn ~., data = training, ntree = 200, mtry = 2, importance = TRUE, proximity = TRUE)
print(rfModel_new)

pred_rf_new <- predict(rfModel_new, testing)
caret::confusionMatrix(pred_rf_new, testing$Churn)

varImpPlot(rfModel_new, sort=T, n.var = 10, main = 'Top 10 Feature Importance')
