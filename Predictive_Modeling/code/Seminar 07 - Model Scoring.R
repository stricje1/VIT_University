library(gains)
library(MASS)
library(car)
file = "C:/Users/Jeff/Documents/VIT_University/data/Banking.csv"
read.csv(file) -> bank
summary(bank)
#Set up Training and Testing Sets
bank_train<-bank[2:22606,]
bank_test<-bank[22607:45211,]
#Build Back model using glm 
bank.model <- glm( RESP ~ job+marital+education+age+balance+homeowner+loans+default+contact+length+campaign+pdays+previous+poutcome , data = bank_train, family=binomial(logit))
summary(bank.model)
# Build deviance models
null.model <- glm(RESP ~ 1, data = bank_train, family=binomial(logit))
summary(null.model) 
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

### simulate data for uplift modeling
set.seed(123)
dd <- sim_pte(n = 1000, p = 20, rho = 0, sigma = sqrt(2), beta.den = 4)
dd$treat <- ifelse(dd$treat == 1, 1, 0)
### fit uplift random forest
fit1 <- upliftRF(y ~ X1 + X2 + X3 + X4 + X5 + X6 + trt(treat),
                 data = dd,
                 mtry = 3,
                 ntree = 100,
                 split_method = "KL",
                 minsplit = 200, # need small trees as there is strong uplift effects in the data
                 verbose = TRUE)
summary(fit1)
### predict on new data
dd_new <- sim_pte(n = 2000, p = 20, rho = 0, sigma = sqrt(2), beta.den = 4)
dd_new$treat <- ifelse(dd_new$treat == 1, 1, 0)
pred <- predict(fit1, dd_new)
head(pred)
perf <- performance(pred[, 1], pred[, 2], dd_new$y, dd_new$treat, direction = 1)
### compute Qini coefficient
Q <- qini(perf, plotit = TRUE)
Q
niv.1 <- niv(y ~ X1 + X2 + X3 + X4 + X5 + X6 + trt(treat), data = dd)            
niv.1$niv
niv.1$nwoe