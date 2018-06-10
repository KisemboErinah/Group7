chess <- read.csv("chessplayers.csv",TRUE,",")

plot(chess$B.YEAR,chess$GAMES ,xlab = "year of birth", ylab="number of games", main="relationship between years and number of games played")
lm(chess$GAMES~chess$B.YEAR)
abline(lm(chess$GAMES~chess$B.YEAR))
pre<-data.frame(chess$B.YEAR=1973)
var<-lm(chess$GAMES~chess$B.YEAR)
predict(var,pre)
