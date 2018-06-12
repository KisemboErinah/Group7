chess <- read.csv("players.csv",TRUE,",") #obtain the dataset

#plot number of games played against year of birth
plot(chess$B.YEAR,chess$GAMES ,xlab = "year of birth", ylab="number of games", main="relationship between years and number of games played")

#find the relationship between the number of games played and the year of birth
relationship<- lm(chess$GAMES~chess$B.YEAR)
summary(relationship)
coeff <- coefficients(relationship)
print(coeff)

abline(lm(chess$GAMES~chess$B.YEAR))#draw the line of regression
pre <- data.frame(chess$B.YEAR <- 1999)


#predicting the number of games played given the year of birth
var<-predict(relationship,pre)
print(var)


