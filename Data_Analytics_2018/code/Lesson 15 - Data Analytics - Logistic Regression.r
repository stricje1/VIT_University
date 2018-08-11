library(gains)
library(MASS)
library(car)
file = "C:/Users/Jeff/Documents/VIT_University/data/Banking.csv"
read.csv(file) -> bank
summary(bank)

#Set up Training and Testing Sets
bank_train<-bank[2:22606,]
bank_test<-bank[22607:45211,]

#View some plots of the Bank_Train
plot(bank_train[,3:6])
plot(bank_train[,"age"])
plot(bank_train[,"education"])

#Get Correlation Matrix
cor(bank_train[,4:11])

#Build Back model using glm 
bank.model <- glm( RESP ~ job+marital+education+age+balance+homeowner+loans+default+contact+length+campaign+pdays+previous+poutcome , data = bank_train, family=binomial(logit))
summary(bank.model)
# Build deviance models
null.model <- glm(RESP ~ 1, data = bank_train, family=binomial(logit))
summary(null.model) 
bank.model1 <- glm( RESP ~ marital+education+age+balance+homeowner+loans+default+contact+length+campaign+pdays+previous+poutcome , data = bank_train, family=binomial(logit))
summary(bank.model1)
bank.model2 <- glm( RESP ~ education+age+balance+homeowner+loans+default+contact+length+campaign+pdays+previous+poutcome , data = bank_train, family=binomial(logit))
summary(bank.model2)
bank.model3 <- glm( RESP ~ homeowner+loans+default+contact+length+campaign+pdays+previous+poutcome , data = bank_train, family=binomial(logit))
summary(bank.model3)
#Check model deviance
# Null deviance
-2*log(bank.model$deviance/bank.model$null.deviance)
-2*log(12054/16285)
# Model deviance
-2*log(bank.model$deviance/bank.model1$deviance)
-2*log(bank.model$deviance/bank.model2$deviance)
-2*log(12054/12177)
-2*log(12054/12237)
#Model4 deviance
-2*log(12054/12303)

#GLM for bank_train
bank.model2 <- glm( RESP ~ job+marital+homeowner+loans+default+contact+length , data = bank_train, family=binomial(logit))
anova(bank.model2, test="Chisq")
pval<-1 - pchisq(5053.5, df=18)
pval

#Frequency Table for "RESP"
cbind( Freq=table(bank_train["RESP"]), Cumul=cumsum(table(bank_train["RESP"])), relative=prop.table(table(bank_train["RESP"])))
#Alternative Freq Table
tbl <- table(bank_train["RESP"])

# Plot the fitted data
plot(bank.model$fitted)
abline(v=11303,col="red",lwd=2)
abline(h=.25,col="green",lwd=2)
abline(h=.5,col="green",lwd=2)
abline(h=.75,col="green",lwd=2)
text(15,.6,"RESP = 0")
text(400,.1,"RESP = 1")

#The standard error is found as sqrt of diagonal in variance:
sqrt(diag(summary(bank.model)$cov.scaled))
c(bank.model$deviance, -2*logLik(bank.model))
#"Null.deviance" is -2 times the logLik for the same model with only a constant term 
c(bank.model$null.dev, -2*logLik(update(bank.model, formula = .~ 1)))

# Confidence interval
confint(bank.model)
# exponentiated coefficients
exp(bank.model$coefficients)
# 95% CI for exponentiated coefficients
exp(confint(bank.model))
# Analysis of variance for individual terms
Anova(bank.model, type="II", test="Wald")
# Pseudo-R-squared
library(rcompanion)

# Overall p-value for model
library(lmtest)
anova(bank.model, 
      update(bank.model, ~1),    # update here produces null model for comparison
      test="Chisq")
lrtest(bank.model)

# Plotting the model
plot(RESP ~ age,
     data = bank_train, 
     xlab="Age", 
     ylab="Response", 
     pch=19)              

curve(predict(bank.model2,newdata = bank_test,type="response"), 
      lty=1, lwd=2, col="blue",                            
      add=TRUE)

require(gains)
bank.pred<-predict(bank.model2, newdata = bank_test, type="response")
bank.gains<-gains(actual=bank.model$fitted.values,predicted=bank.pred,optimal=TRUE)
bank.gains

#Gain chart for mean predicted response.
plot(bank.gains$mean.prediction,type="o")
#Gain chart for mean response
plot(bank.gains$mean.resp,type="o")

#Gain chart for mean response, cumulative mean response, and mean predicted response.
plot(with(subset(bank_train,RESP==0),
gains(actual=bank.model$fitted.values,predicted=bank.pred,optimal=TRUE)),
main="Test Gains Table Plot")

fi1<-fitted(bank.model)
fi1c<-cut(fi1,br=c(0,quantile(fi1,p=seq(.1,.9,.1)),1))
table(fi1c)
fi1c=cut(fi1,br=c(0,quantile(fi1,p=seq(.1,.9,.1)),1),labels=F)
table(fi1c)

bank_train$RESP1 <- ifelse(bank_train$RESP=="NO",0,1)
bank_train$RESP <- bank_train$RESP1 
bank_train$RESP

E=matrix(0,nrow=10,ncol=2)
O=matrix(0,nrow=10,ncol=2)
for(j in 1:10){
 E[j,2]=sum(fi1[fi1c==j])
 E[j,1]=sum((1-fi1)[fi1c==j])
 O[j,2]=sum(bank_train$RESP[fi1c==j])
 O[j,1]=sum((1-bank_train$RESP)[fi1c==j]) }
 
 sum((O-E^2/E))
 1-pchisq(sum((O-E^2/E)),8)
 
require(graphics)
infglm.SR <- influence.measures(bank.model)
infglm.any<-which(apply(infglm.SR$is.inf, 1, any))
summary(infglm.any)
# which observations 'are' influential
summary(infglm.SR)
plot(rstudent(bank.model) ~ hatvalues(bank.model))
plot(bank.model, which = 5)
## The 'infl' argument is not needed, but avoids recomputation:
rs <- rstandard(bank.model)
iflSR <- influence(bank.model)
ident<-identical(rs, rstandard(bank.model, infl = iflSR))
## to "see" the larger values:
1000 * round(dfbetas(bank.model, infl = iflSR), 3)
cat("PRESS :"); (PRESS <- sum( rstandard(bank.model, type = "deviance")^2 ))
stopifnot(all.equal(PRESS, sum( (residuals(bank.model) / (1 - iflSR$hat))^2)))
# Create new column filled with default colour
data$Color="black"
# Set new column values to appropriate colours
ifelse(data$col_name2>.2,"red","blue")
ifelse(data$col_name2<-.2,"red","blue")
data$Color[data$col_name2>= .2]="red"
data$Color[data$col_name2 <-.2]="red"
data$Color[data$col_name2 < .2]="blue"
data$Color[data$col_name2>=-.2]="blue"
# Plot all points at once, using newly generated colours
plot(data$col_name1,data$col_name2, ylim=c(0,5), col=data$Color, ylim=c(0,10))
#dfbeta plots
dfbetasPlots(bank.model)
#model selection
stepAIC(glm(RESP~-1+1,data=bank_train,family=binomial),scope=~job*age*education*marital)

p1=matrix(0,nrow=13,ncol=3)
i=1
for(p in seq(min(fitted(bank.model)),.95,.05)){
t1=table(fitted(bank.model)>p,bank_train$RESP)
p1[i,]=c(p,t1[2,2])/sum(t1[,2]),(t1[1,1])/sum(t1[,1]))
i=i+1
}
plot(1-p1[,3],p1[,2],type="o")
#*********************************************************
#In this example, we are going to train a logistic regression to predict the class in the test data. The dataset I chose for this example in Longitudinal Low Birth Weight Study (CLSLOWBWT.DAT). [Hosmer and Lemeshow (2000) Applied Logistic Regression: Second Edition.] These data are copyrighted by John Wiley & Sons Inc. I have split the data so each class is represented by a training set and testing set: train1 is the half of the set (245 rows) and test1 is the other half (245 rows).
#-----------------------------------------------------
#Variable Description Codes/Values Name
##Identification Code ID Number ID
##Birth Number 1-4 BIRTH
##Smoking Status 0 = No, 1 = Yes SMOKE During Pregnancy
##Race 1 = White, 2 = Black RACE 3 = Other
##Age of Mother Years AGE
##Weight of Mother at Pounds LWT Last Menstrual Period
##Birth Weight Grams BWT
#-----------------------------------------------------
##Low Birth Weight 1 = BWT <=2500g, LOW 0 = BWT >2500g
#Problem Statement: In this example, we want to predict Low Birth Weight using the remaining dataset variables. Low Birth Weight, the dependent variable, 1 = BWT <=2500g and 0 = BWT >2500g.
#-----------------------------------------------------
#Load the files
file1 = "C:/Users/Strickland/Documents/VIT University/train1.csv"
file2 = "C:/Users/Strickland/Documents/VIT University/test1.csv"
read.csv(file1) -> lbw_train
read.csv(file2) -> lbw_test
summary(lbw_train)
summary(lbw_test)
#-----------------------------------------------------
#fit the model
lbw.model <- glm( LOW ~ BIRTH + SMOKE + RACE + AGE + LWT + BWT, data = lbw_train, family=binomial(logit))
summary(lbw.model)
#-----------------------------------------------------
#Frequency Table for "LOW"
cbind( Freq=table(lbw_train["LOW"]), Cumul=cumsum(table(lbw_train["LOW"])), relative=prop.table(table(lbw_train["LOW"])))
#Alternative Freq Table
tbl <- table(lbw_train["LOW"])
tbl
#-----------------------------------------------------
# Plot the fitted data
plot(lbw.model$fitted)
abline(v=125,col="red",lwd=2)
abline(h=.25,col="green",lwd=2)
abline(h=.5,col="green",lwd=2)
abline(h=.75,col="green",lwd=2)
text(15,.6,"LOW = 0")
text(200,.1,"LOW = 1")
#--------------------------------------------------
#fit the model using probit link function
lbw.model2 <- glm( LOW ~ BIRTH + SMOKE + RACE + AGE + LWT + BWT, data = lbw_train, family=binomial(probit))
summary(lbw.model2)
#-----------------------------------------------------
#Frequency Table for "LOW"
cbind( Freq=table(lbw_train["LOW"]), Cumul=cumsum(table(lbw_train["LOW"])), relative=prop.table(table(lbw_train["LOW"])))
#Alternative Freq Table
tbl <- table(lbw_train["LOW"])
tbl
#-----------------------------------------------------
# Plot the fitted data
plot(lbw.model2$fitted)
abline(v=125,col="red",lwd=2)
abline(h=.25,col="green",lwd=2)
abline(h=.5,col="green",lwd=2)
abline(h=.75,col="green",lwd=2)
text(15,.6,"LOW = 0")
text(200,.1,"LOW = 1")
#-----------------------------------------------------
#Gaussian model
lbw.model3 <- glm( LOW ~ BIRTH + SMOKE + RACE + AGE + LWT + BWT, data = lbw_train, family=gaussian(identity))
#Poisson model
lbw.model4 <- glm( LOW ~ BIRTH + SMOKE + RACE + AGE + LWT + BWT, data = lbw_train, family=poisson)
#-----------------------------------------------------
#Get model names
model.names<-c('logit', 'probit', 'gaussian', 'poisson')
loglike <-c(logLik(lbw.model),logLik(lbw.model2),logLik(lbw.model3), logLik(lbw.model4))
numparms<-c(3,3,3,3)
#-----------------------------------------------------
#Akaike information criterion
AIC.func<-function(LL,K,n,modelnames)
{
AIC<- -2*LL + 2*K
AICc<-AIC + 2*K*(K+1)/(n-K-1)
output<-cbind(LL,K,AIC,AICc)
colnames(output)<-c('LogL','K','AIC','AICc')
minAICc<-min(output[,"AICc"])
deltai<-output[,"AICc"]-minAICc
rel.like<-exp(-deltai/2)
wi<-round(rel.like/sum(rel.like),3)
out<-data.frame(modelnames,output,deltai,wi)
out
}
#Show model comparison
AIC.func(loglike,numparms,dim(lbw_train)[1],model.names)
#-----------------------------------------------------
require(gains)
lbw.pred<-predict(lbw.model, newdata = lbw_test, type="response")
lbw.gains<-gains(actual=lbw.model$fitted.values,predicted=lbw.pred,optimal=TRUE)
lbw.gains
#-----------------------------------------------------
#Gain chart for mean response, cumulative mean response, and mean predicted response.
plot(with(subset(lbw_train,LOW==0),
gains(actual=lbw.model$fitted.values,predicted=lbw.pred,optimal=TRUE)),
main="Test Gains Table Plot")