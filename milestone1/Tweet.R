#read csv file into data frame
df<- read.csv("D:\\06_Master's degree\\04.CLU MSIT\\04.courses\\IT-531-01 Data mining\\01_group project\\Tweet.csv")
#assign the data frame to a vector
mydata<-df$FriendsCount
#descriptive stat of mean in friend counts
mean(mydata)
#descriptive stat of median in friend counts
median(mydata)
#descriptive stat of standard deivation in friend counts
sd(mydata)
#boxplot
boxplot(FriendsCount ~ Lang, data=df, xlab="Lang", ylab="FriendsCount")
#bar chart based on language type
barplot(table(df$Lang))