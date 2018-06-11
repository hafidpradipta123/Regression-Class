dataproject<-read.table("C:/Users/Hafid Pradipta/OneDrive/Data set/Chapter  1 Data Sets/Data_615_clean.txt", header=T)
attach(dataproject)


#preliminary analysis
#plot for response variable
boxplot(ut, xlab="ut", main="Tertiary College Enrollment" )
hist(ut, main="Tertiary College Enrollment" )

#correlation among variable
pairs(dataproject)
cor(dataproject)
library(corrplot)


#normality check of dependent variable
shapiro.test(ut)

#error terms are independent from each other because there are no sequential variables

#full model
dataprojreg<-lm(ut~up+gi+mi+sg+as.factor(dc), data=dataproject)
plot(predict(dataprojreg), dataprojreg$residuals)
plot(dataprojreg)
rstudent(dataprojreg)

#critical value
qt(0.9998, 105)
#rstudentzied that above critical value
abs(rstudent(dataprojreg))> 3.657524

#how many rstudentized that above the critical value
which(abs(rstudent(dataprojreg))> 3.657524)


#omit the outliers with 25th observation
dataproject2<-dataproject[-25,]
nrow(dataproject2)

#full model without the outliers
dataprojreg00<-lm(ut~up+gi+mi+sg+as.factor(dc), data=dataproject2)
plot(dataprojreg00)

#variable selection using stepwise
library(MASS)
step(dataprojreg00)

#variable selection using leaps
library(leaps)
## if library doesn't contain leaps
## install.packages("leaps")
temp<-regsubsets(ut ~ up+gi+mi+sg+as.factor(dc), data=dataproject2)
bestsub<-summary(temp)
bestsub

#reduced model
dataprojreg2<-lm(ut~ up + gi + as.factor(dc), data=dataproject2)
summary(dataprojreg2)
plot(dataprojreg2)
#we will no further on this model bcause dc is not significant

#partial F test for model 2 with model 3 from AIC
anova(dataprojreg3,dataprojreg2)

#method to find 
selcri<-function(lmout)
{
  n <- length(lmout$fit)
  rsq <- summary(lmout)$r.sq
  adj.rsq <- summary(lmout)$adj.r.sq
  aic <- extractAIC(lmout)[2]
  bic <- extractAIC(lmout, k = log(n))[2]
  press <- sum((lmout$residuals/(1 - hatvalues(lmout)))^2)
  cbind(rsq, adj.rsq, aic, bic, press)
}
selcri(dataprojreg)
selcri(dataprojreg2)
selcri(dataprojreg3)
selcri(dataprojreg4)

#Mallow C's 
library(leaps)
leaps( x=dataproject[,1:5], y=dataproject[,1], names=names(dataproject)[1:5], method="Cp")
#mallow C's
(15866/200)-(105-2*3)

#reduced model3
dataprojreg3<-lm(ut ~ up + gi, data=dataproject2)
summary(dataprojreg3)
anova(dataprojreg3)
plot(dataprojreg3)
shapiro.test(dataprojreg3$residuals)
library(lmtest)
bptest(dataprojreg3)

#boxcox on this regression model
boxcox (dataprojreg3)

#transofrmed y into squareroot
dataproject2$squt<-sqrt(dataproject2$ut)
dataprojreg4<-lm(dataproject2$squt~up+gi, data=dataproject2)
summary(dataprojreg4)
plot(dataprojreg4)
shapiro.test(dataprojreg4$residuals)
bptest(dataprojreg4)


## end of the project code##





 


