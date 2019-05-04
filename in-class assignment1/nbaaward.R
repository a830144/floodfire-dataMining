#an script to use Apriori to find the association rule of NBA award winner with other awards 
#in the same year 
# Load the libraries
library(arules)
# load the csv file nbaawards.csv
df<-read.csv("nbaawards.csv")
# get the rules
rules <- apriori(df, parameter = list(supp = 0.001, conf = 0.8,minlen=2))
# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:5])