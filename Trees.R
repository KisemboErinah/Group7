data("trees") #load dataset "trees" 
View(trees) #have a look at the dataset
plot(Volume ~ Girth, data = trees, log = "xy") #plot scatter graph of volume against Girth
cor(trees$Girth, trees$Volume) #Evaluate correlation between Girth and volume
TreesModel <- lm(Volume ~ Girth, data=trees)  # build linear regression model on full data
print(TreesModel)
abline(TreesModel)
summary(TreesModel)  


a <- data.frame(Girth = 15) #make a data frame for the predict funtion
result <-  predict(TreesModel,a) # predict function
print(result) 