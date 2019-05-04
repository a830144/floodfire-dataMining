#decision tree using party
df<- read.csv("D:\\06_Master's degree\\04.CLU MSIT\\04.courses\\IT-531-01 Data mining\\assignments\\homework assignment3\\spambase.csv",header=FALSE)
library(party)
set.seed(1234)
ind <- sample(2, nrow(df), replace=TRUE, prob=c(0.7, 0.3))
trainData <- df[ind==1,]
testData <- df[ind==2,]
myFormula <- df$V58~.
spam_ctree <- ctree(myFormula, data=trainData)
plot(spam_ctree, type="simple")
testPred <- predict(spam_ctree, newdata = testData)
table(testPred, testData$V58)
#decision tree using rpart
spambase<- read.csv("D:\\06_Master's degree\\04.CLU MSIT\\04.courses\\IT-531-01 Data mining\\assignments\\homework assignment3\\spambase.csv",header=FALSE)
#install.packages("rpart")
library(rpart)
set.seed(1234)
ind <- sample(2, nrow(spambase), replace=TRUE, prob=c(0.7, 0.3))
spambase.trainData <- spambase[ind==1,]
spambase.testData <- spambase[ind==2,]
myFormula <- spambase$V58~spambase$V1+spambase$V2+spambase$V3+spambase$V4+spambase$V5+spambase$V6+spambase$V7+spambase$V8+spambase$V9+spambase$V10+spambase$V11+spambase$V12+spambase$V13+spambase$V14+spambase$V15+spambase$V16+spambase$V17+spambase$V18+spambase$V19+spambase$V20+spambase$V21+spambase$V22+spambase$V23+spambase$V24+spambase$V25+spambase$V26+spambase$V27+spambase$V28+spambase$V29+spambase$V30+spambase$V31+spambase$V32+spambase$V33+spambase$V34+spambase$V35+spambase$V36+spambase$V37+spambase$V38+spambase$V39+spambase$V40+spambase$V41+spambase$V42+spambase$V43+spambase$V44+spambase$V45+spambase$V46+spambase$V47+spambase$V48+spambase$V49+spambase$V50+spambase$V51+spambase$V52+spambase$V53+spambase$V54+spambase$V55+spambase$V56+spambase$V57
#myFormula <- spambase$V58 ~ .
spambase_rpart <- rpart(myFormula, data = spambase.trainData, control = rpart.control(minsplit = 10))
printcp(spambase_rpart) 
print(spambase_rpart)
plot(spambase_rpart)
text(spambase_rpart, use.n=T)

opt <- which.min(spambase_rpart$cptable[,"xerror"])
cp <- spambase_rpart$cptable[opt, "CP"]
spambase_rpart_prune <- prune(spambase_rpart, cp = cp)
print(spambase_rpart_prune)
plot(spambase_rpart_prune)
text(spambase_rpart_prune, use.n=T)


spambase_pred <- predict(spambase_rpart_prune, spambase.testData)
table(spambase_pred, testData$V58)




#Naïve Bayes 
install.packages("e1071")
library(e1071)
model<-naiveBayes(df[,1:57], df[,58])
table(predict(model, df[,-58]), df[,58])
library(caret)
result <- confusionMatrix(predict(model, df[1:57]), df[,58])
print(result)
