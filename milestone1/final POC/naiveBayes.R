df<- read.csv("Tweet.csv")
#Create Naïve Bayes classifier
library(e1071) 

myFormula <- Lang ~ FavouritesCount+StatusesCount+FriendsCount+isVerified+isTranslator+isRetweetedByMe
 model<-naiveBayes(myFormula, data = df)

#Evaluate the Classifier
 table(predict(model, df[,-14]), df[,14])
 library(caret)
 result <- confusionMatrix(predict(model, df[1:14]), df[,14])
 print(result)
