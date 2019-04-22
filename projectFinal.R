# file preview shows a header row
setwd("~/Desktop")
diabates=read.csv("~/Desktop/stat project/diabetes.csv", header = TRUE)
dim(diabates)
summary(diabates)
names(diabates)
# removing missing values which are shown by 0:
diabates$Outcome = factor(diabates$Outcome)
for (i in 2:6) {
  diabates = diabates[-which(diabates[, i] == 0), ]
}
dim(diabates)
# modify names to make working easier
names(diabates)[7]="dpf"
names(diabates)=tolower(names(diabates))
names(diabates)
summary(diabates)
par(mfrow=c(2,2))

hist(diabates$pregnancies)

hist(diabates$glucose)
hist(diabates$bmi)
hist(diabates$dpf)
par(mfrow=c(1,2))
# boxplot
with(diabates,boxplot(dpf~outcome,
                      ylab="Diabetes Pedigree Function",
                      xlab="Presence of Diabetes",names=c("No","Yes"),
                      main="Figure A",
                      outline=FALSE))
with(diabates,boxplot(bloodpressure~outcome,
                      ylab="BloodPressue Level",
                      xlab="Presence of Diabetes",
                      main="Figure B",names=c("No","Yes"),
                      outline=FALSE))

#subsetting based on response
par(mfrow=c(1,2))
yes=diabates[diabates$outcome==1 , ]
no=diabates[diabates$outcome==0 , ]
# density plot for glucose
plot(density(yes$glucose),
     xlim=c(0,250),
     ylim=c(0.00,0.02),
     xlab="Gloucos Level",
     main="Figure C",
     lwd=2)
lines(density(no$glucose),
      col="Red",
      lwd=2)
legend("topleft",
       col = c("black","red"),
       legend = c("With Diabetes","Without Diabates"),
       lwd=2,
       bty = "n")
# density plot for  insulin
plot(density(yes$insulin),
     xlim=c(0,250),
     ylim=c(0.00,0.02),
     xlab="Insulin Level",
     main="Figure D",
     lwd=2)
lines(density(no$insulin),
      col="Red",
      lwd=2)
legend("topleft",
       col = c("black","red"),
       legend = c("With Diabetes","Without Diabates"),
       title.col = text.col,
       lwd=2,
       bty = "n",
       title.adj = 0.5,
       yjust = 1, x.intersp = 1)

#finding cor between variables
library(reshape2)
cor_melt = melt(cor(diabates[, 1:8]))
cor_melt
cor_melt = cor_melt[which(cor_melt$value > 0.5 & cor_melt$value != 1), ]
cor_melt[1:3,]
# choosing between variables based on Best subset Selection Method
library(leaps)
regfit.full2=regsubsets(outcome~.,data=diabates,nvmax=19)
regfit.full2
reg.summary2=summary(regfit.full2)
names(reg.summary2)
reg.summary2$bic
coef(regfit.full2,3)
reg.summary2
library(ISLR)
library(caret)
library(lattice)
library(ggplot2)

#changing outcome levels to "helthy" , "diabates" to be able to use twoclasssummary in trainControl Function
levels(diabates$outcome) <- c('healthy', 'diabates')
library(ROCR)
library(gplots)
library(pROC)
head(twoClassSummary)
# Define train control for k fold cross validation
train_control= trainControl(method="cv", number=10, classProbs = TRUE,summaryFunction = twoClassSummary)
train_controlS= trainControl(method="cv", number=10)
set.seed(10)
train_control
library(randomForest)
library(kernlab)

#fir glm model
modelglm2 = train(outcome~glucose+age+bmi, method="glm",data=diabates,metric = "ROC",trControl=train_control,family=binomial)
print(modelglm2)

modelglmS = train(outcome~glucose+age+bmi, method="glm",data=diabates,trControl=train_controlS,family=binomial)
print(modelglmS)

#fit KNN model
modelknn2 = train(outcome~glucose+age+bmi, method="knn",data=diabates,metric = "ROC", trControl=train_control)
print(modelknn2)

modelknn2S = train(outcome~glucose+age+bmi, method="knn",data=diabates, trControl=train_controlS)
print(modelknn2S)
#fit the LDA model
modelLDA2 = train(outcome~glucose+age+bmi, method="lda",data=diabates, metric = "ROC", trControl=train_control)
print(modelLDA2)

modelLDA2S = train(outcome~glucose+age+bmi, method="lda",data=diabates, trControl=train_controlS)
print(modelLDA2S)
#fit Quadratic Discriminant Analysis QDA:
modelQDA2 = train(outcome~glucose+age+bmi, method="qda",data=diabates, metric = "ROC", trControl=train_control)
print(modelQDA2)

modelQDA2S = train(outcome~glucose+age+bmi, method="qda",data=diabates, trControl=train_controlS)
print(modelQDA2S)
#fit the Random Forests :
modelRF2 = train(outcome~glucose+age+bmi, method="rf",data=diabates, metric = "ROC", trControl=train_control)
print(modelRF2)

modelRF2S = train(outcome~glucose+age+bmi, method="rf",data=diabates, trControl=train_controlS)
print(modelRF2S)





