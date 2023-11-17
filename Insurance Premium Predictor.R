rm(list=ls())
library(rio)
library(moments)
library(car)
library(stargazer)

#importing data set
cars_data_set = import("D:/QMB_Project/cars.xlsx")

#subsetting the data to balance the data (48.5/51.5) split
cars_data = subset(cars_data_set,(brand == "Europe." | brand == "Japan."))
colnames(cars_data)
 
#Required fields X1,X2,X3 and Y
cars = cars_data[c("mpg","hp","weightlbs","brand")]
nrow(cars)
cars

#Multiple Regression
par(mfrow = c(3, 3))
model1 = lm(mpg~hp,data=cars)
summary(model1)
qqnorm(model1$residuals,pch=19,main="Normality plot")
qqline(model1$residuals,col="red",lwd=4)

model2 = lm(mpg~weightlbs,data=cars)
summary(model2)
qqnorm(model2$residuals,pch=19,main="Normality plot")
qqline(model2$residuals,col="red",lwd=4)

model3 = lm(mpg~brand,data=cars)
summary(model3)
qqnorm(model3$residuals,pch=19,main="Normality plot")
qqline(model3$residuals,col="red",lwd=4)

model4 = lm(mpg~hp+weightlbs,data=cars)
summary(model4)
qqnorm(model4$residuals,pch=19,main="Normality plot")
qqline(model4$residuals,col="red",lwd=4)

model5 = lm(mpg~hp+brand,data=cars)
summary(model5)
qqnorm(model5$residuals,pch=19,main="Normality plot")
qqline(model5$residuals,col="red",lwd=4)

model6 = lm(mpg~weightlbs+brand,data=cars)
summary(model6)
qqnorm(model6$residuals,pch=19,main="Normality plot")
qqline(model6$residuals,col="red",lwd=4)

model7 = lm(mpg~hp+weightlbs+hp*weightlbs,data=cars)
summary(model7)
qqnorm(model7$residuals,pch=19,main="Normality plot")
qqline(model7$residuals,col="red",lwd=4)

model8 = lm(mpg~hp+hp*hp,data=cars)
summary(model8)
qqnorm(model8$residuals,pch=19,main="Normality plot")
qqline(model8$residuals,col="red",lwd=4)

model9 = lm(mpg~hp+weightlbs+hp*weightlbs,data=cars)
summary(model9)
qqnorm(model9$residuals,pch=19,main="Normality plot")
qqline(model9$residuals,col="red",lwd=4)

model10 = lm(mpg~weightlbs+weightlbs*weightlbs,data=cars)
summary(model10)
qqnorm(model10$residuals,pch=19,main="Normality plot")
qqline(model10$residuals,col="red",lwd=4)










