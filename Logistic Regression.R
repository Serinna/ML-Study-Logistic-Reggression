install.packages("ggplot2")
library(ggplot2)

mydata<-read.csv("/path/to/mayo_permafrost_data_train.csv")
summary(mydata)
sapply(mydata, sd)

pres=mydata$PRESENCE
esred=mydata$TM.CH3
esnearinf=mydata$TM.CH4
esmidinf=mydata$TM.CH5
estherminf=mydata$TM.CH6
equilat=mydata$EQUIVALENT.LATITUDE

model=lm(pres ~ esred + esnearinf + estherminf + equilat)
print(summary(model))
print(anova(model))

plot(model$fitted.values,model$residuals,main="yhat vs. y")

mylogit <- glm(pres ~ esred+esnearinf+esmidinf+estherminf+equilat, data = mydata, family = "binomial")
print(summary(mylogit))
print(anova(mylogit, test="Chi"))

mylogit <- glm(pres ~ esred+esnearinf+esmidinf+equilat, data = mydata, family = "binomial")
print(summary(mylogit))
#null deviance-residual deviance, find df and respponded p-value to reject null hepothesis(null var is better)
anova(mylogit, test="Chisq")

a=mylogit$coefficients[1]
b=mylogit$coefficients[2]
c=mylogit$coefficients[3]
d=mylogit$coefficients[4]
e=mylogit$coefficients[5]

testdata=read.csv("/path/to/mayo_permafrost_data_test.csv")
x1=testdata$TM.CH3
x2=testdata$TM.CH4
x3=testdata$TM.CH5
x4=testdata$EQUIVALENT.LATITUDE

yhat=exp(a+b*x1+c*x2+d*x3+e*x4)/(1+exp(a+b*x1+c*x2+d*x3+e*x4))
expectedpres=yhat
expectedpres[which(yhat > 0.5)]=1
expectedpres[which(yhat <= 0.5)]=0

plot(testdata$PRESENCE, expectedpres)
table(testdata$PRESENCE, expectedpres)

acc=(length(which(testdata$PRESENCE-expectedpres == 0))/length(testdata$PRESENCE))*100.0
print(acc)
