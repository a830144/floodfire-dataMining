#The party package provides nonparametric regression trees

library(party)
df<-read.csv("Tweet.csv")
#Use tweet dataset for this example
#Split the data into two subsets: training (70%) and test (30%)
set.seed(1234)
ind <- sample(2, nrow(df), replace=TRUE, prob=c(0.7, 0.3))
trainData <- df[ind==1,]
testData <- df[ind==2,]
#Learn the decision tree
#myFormula <- Lang ~ FavouritesCount+StatusesCount+FriendsCount+isVerified+isTranslator+isRetweetedByMe
myFormula <- FavouritesCount ~ FriendsCount+StatusesCount+Lang+isVerified+isTranslator+isRetweetedByMe
tweet_ctree <- ctree(myFormula, data=trainData)
print(tweet_ctree)
plot(tweet_ctree)
plot(tweet_ctree, type="simple")

#Evaluate the decision tree
#Predict on test data

testPred <- predict(tweet_ctree, newdata = testData)
table(testPred, testData$Lang)
