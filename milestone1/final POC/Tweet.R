#read csv file into data frame
df<- read.csv("Tweet.csv")
#assign the data frame to a vector
mydata<-df$FriendsCount.0
#descriptive stat of mean in friend counts
mean(mydata)
#descriptive stat of median in friend counts
median(mydata)
#descriptive stat of standard deivation in friend counts
sd(mydata)
#boxplot
boxplot(FriendsCount.0 ~ Lang, data=df, xlab="Lang", ylab="FriendsCount")
#bar chart based on language type
barplot(table(df$Lang))


