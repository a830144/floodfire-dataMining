df<-read.table("car.data",sep=",")
set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))

View(df)
trainData<-df[ind==1,]
testData<-df[ind==2,]
library(party)
myFormula <- V7 ~ V1 + V2 + V3 + V4 + V5 +V6
df_ctree <- ctree(myFormula, data=trainData)
print(df_ctree)
plot(df_ctree)
testPred <- predict(iris_ctree, newdata = testData)
table(testPred, testData$Species)

