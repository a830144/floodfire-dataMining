#use arules library
library(arules)
#read the csv file
df<-read.csv("D:\\06_Master's degree\\04.CLU MSIT\\04.courses\\IT-531-01 Data mining\\assignments\\homework assignment3\\car.csv")
#use apriori, if using support=0.2, the result is not enough 
rules <- apriori(df, parameter = list(supp=0.1, conf=0.6))
#inspect
inspect(rules)
#prune the uniteresting rules
rules <- apriori(df, parameter = list(supp=0.1, conf=0.6),appearance = list( rhs = c( 'classifier=unacc', 'classifier=acc', 'classifier=good', 'classifier=v-good' ), default = 'lhs' ))
#inspect again
inspect(rules)
