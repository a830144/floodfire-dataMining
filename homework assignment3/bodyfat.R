  library(rpart)
#Use Bodayfat dataset for this example. Generate training and testing set
  data("bodyfat", package = "TH.data")
 set.seed(1234)
 ind <- sample(2, nrow(bodyfat), replace=TRUE, prob=c(0.7, 0.3))
 bodyfat.train <- bodyfat[ind==1,]
 bodyfat.test <- bodyfat[ind==2,]
#Learn the decision tree
 myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
 bodyfat_rpart <- rpart(myFormula, data = bodyfat.train, control = rpart.control(minsplit = 10))

 
 
 #See the results
 printcp(bodyfat_rpart) # display the results 
 plotcp(bodyfat_rpart) # visualize cross-validation results 
 summary(bodyfat_rpart) # detailed summary of splits
 #Visualize the tree
 print(bodyfat_rpart)
  text(bodyfat_rpart, use.n=T)
 #Prune the decision tree:  select the tree with the minimum prediction error 
  opt <- which.min(bodyfat_rpart$cptable[,"xerror"])
  cp <- bodyfat_rpart$cptable[opt, "CP"]
  bodyfat_prune <- prune(bodyfat_rpart, cp = cp)
  print(bodyfat_prune)
  plot(bodyfat_prune)
  text(bodyfat_prune, use.n=T)
  
  
  
 # Evaluate the decision  tree
  DEXfat_pred <- predict(bodyfat_prune, newdata=bodyfat.test)
  table(DEXfat_pred, bodyfat.test$DEXfat)
  xlim <- range(bodyfat$DEXfat)

  
 