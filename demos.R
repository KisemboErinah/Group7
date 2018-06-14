data("trees") #load data set trees
View(trees) #have a look at the data set
plot(Volume ~ Girth, data = trees, log = "xy") #plot scatter graph of Volume against Girth
TreesModel <- lm(Volume ~ Girth, data = trees) #Evaluate the correlation between Girth and volume
print(TreesModel)
abline(TreesModel)
summary(TreesModel)

a <- data.frame(Girth = 15) #make a data frame of the predict function
result <- predict(TreesModel,a) #predict function
print(result)
