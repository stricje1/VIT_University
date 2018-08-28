#-----------------------------------------------------------------
# OLD LESSON 11
#-----------------------------------------------------------------
#Galapagos Islands: There are 30 cases (Islands) and 7 variables in the dataset. 
gala <- read.csv("C:/Users/Jeff/Documents/VIT_University/data/gala1.csv")
data(gala)
gala
gala2 <- read.csv("C:/Users/Jeff/Documents/VIT_University/data/gala2.csv")
data(gala2)
gala2
#1. Linear model: 
model1<-lm(Species~Area,data=gala)
logLik(model1)
AIC(model1)

plot(gala$Area,gala$Species, xlab='Area', ylab='Species')
abline(model1,col=2, lty=2)
mtext('Model 1: linear model', side=3, line=.5)

#2. Gleason model:  
model2<-lm(Species~log(Area),data=gala)
logLik(model2)
AIC(model2)

model3<-lm(Species~log10(Area),data=gala)
logLik(model2)
AIC(model2)

plot(log(gala$Area), gala$Species, xlab='log(Area)', ylab='Species')
abline(model2,col=2,lty=2)
mtext( 'Model 2: Gleason model', side=3, line=.5)

#log-Arrhenius model
model3<-lm(log(Species)~log(Area), data=gala)
logLik(model3)
AIC(model3)


plot(log(gala$Area), log(gala$Species), xlab='log(Area)', ylab='log(Species)')
abline(model3,col=2,lty=2)
mtext('Model 3: log-Arrhenius model', side=3, line=.5)

model.names<-c('Linear', 'Gleason', 'Log-Arrhenius')
loglike <-c(logLik(model1),logLik(model2),logLik(model3))
numparms<-c(3,3,3)

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
AIC.func(loglike,numparms,dim(gala)[1],model.names)

#We now consider additional variables: Endemics, Elevation, Nearest, Scruz, and Adjacent. 
gfit <- lm(Species~Area + Elevation + Nearest + Scruz + Adjacent, data=gala)
summary(gfit)
lm(formula = Species~Area + Elevation + Nearest + Scruz + Adjacent, data=gala)
summary(gfit)

x <- cbind(1,gala[,-c(1,2)])
y <- gala$Species
x <- as.matrix(x)
t(x) %*% x

xtxi <- solve(t(x) %*% x)
xtxi

gfit <- lm(Species~Area+Elevation+Endemics+Nearest+Scruz+Adjacent,data=gala2)
gs <- summary(gfit)
gs$cov.unscaled

names(gs)
names(gfit)
gfit$fit
gfit$res
xtxi %*% t(x) %*% y

solve(t(x) %*% x, t(x) %*% y)

root1<-sum((gfit$res)^2)
sqrt(root1/(30-6))
sqrt(diag(xtxi))*60.97519
1-sum((gfit$res)^2)/sum((y-mean(y))^2)

gfit2 <- lm(Species~Area+Elevation+Endemics+Nearest+Scruz+Adjacent,data=gala2)
gs <- summary(gfit)

#The Log-Arrhenius Model
gala <- read.csv("C:/Users/Strickland/Documents/VIT University/gala2.csv")
data(gala)
x <- cbind(1,gala[,-c(1,2)])
y <- gala$Species
x <- as.matrix(x)
t(x) %*% x
xtxi <- solve(t(x) %*% x)
xtxi
gfit <- lm(Species~Area+Elevation+Endemics+Nearest+Scruz+Adjacent,data=gala)
gs <- summary(gfit)
gs$cov.unscaled
names(gs)
names(gfit)
gfit$fit
gfit$res
xtxi %*% t(x) %*% y
solve(t(x) %*% x, t(x) %*% y)
root1<-sum((gfit$res)^2)
sqrt(root1/(30-6))
sqrt(diag(xtxi))*60.97519
1-sum((gfit$res)^2)/sum((y-mean(y))^2)
gs

#lm versus glm
gfit1 <- lm(Species~Area+Elevation+Endemics+Nearest+Scruz+Adjacent,data=gala)
gs1 <- summary(gfit1)
gfit2 <- glm(Species~Area+Elevation+Endemics+Nearest+Scruz+Adjacent,data=gala,family=gaussian(link=identity))
gs2 <- summary(gfit2)
gfit3 <- glm(Species~Area+Elevation+Endemics+Nearest+Scruz+Adjacent,data=gala,family=inverse.gaussian(link=1/mu^2))
gs3 <- summary(gfit3)
gs1; gs2; gs3
#lm vs. glm vs. glm-logit
pima <- read.csv("C:/Users/Jeff/Documents/VIT_University/data/pima_indians.csv")
pima.fit<-lm(x9~x1+x2+x3+x4+x5+x6+x7+x8,data=pima)
pima.fit2<-glm(x9~x1+x2+x3+x4+x5+x6+x7+x8,data=pima,family=gaussian(link=identity))
pima.fit3<-glm(x9~x1+x2+x3+x4+x5+x6+x7+x8,data=pima,family=binomial(link = "logit"))
model.names<-c('Linear', 'GLM', 'Logit')
loglike <-c(logLik(pima.fit),logLik(pima.fit2),logLik(pima.fit3))
numparms<-c(3,3,3)
AIC.func(loglike,numparms,dim(gala)[1],model.names)
#-----------------------------------------------------------------
# OLD LESSON 12
#-----------------------------------------------------------------
# Data on 150 rats contain identifying "litter", "rx" (indicator of injection of drug after initial administration of carcinogen), time in days on study (ignored initially, and "status" which is indicator of tumor, our binary response variable.
library(survival)
summary(rats)
plot(rats) 
#Three Models
fitB1 = glm(cbind(status,1-status) ~ rx, family=binomial, data = rats)
#NOTE you can use the column headers as variable names if you specify the data-frame using "data="
fitB1
summary(fitB1)$coef
#The standard error is found as sqrt of diagonal in variance:
sqrt(diag(summary(fitB1)$cov.scaled))
c(fitB1$deviance, -2*logLik(fitB1))
#"Null.deviance" is -2 times the logLik for the same model with only a constant term 
c(fitB1$null.dev, -2*logLik(update(fitB1, formula = .~ 1)))
#Next use "update" to change the fitB1 model not in its model formula but in its specified link within the binomial “family”.
fitB2 = update(fitB1, family=binomial(link="probit"))
rbind(logit= fitB1$coef, probit= fitB2$coef, rescal.probit = fitB2$coef/0.5513)
#use this idea to examine the quality of the model with predictor log(time) in the model along with rx.
fitB3 = update(fitB1, formula= . ~ . + I(log(time)))
summary(rats$time[rats$status==1])
cbind(rats[1:10,], model.matrix(fitB3)[1:10,])
summary(fitB3)$coef
#Or alternatively compare deviances or logLik’s with fitB1
c(2*(logLik(fitB3)-logLik(fitB1)), fitB1$dev-fitB3$dev)
1-pchisq(24.373,1)
#We could try to enter additional terms like log(time)^2 or rx * log(time)
fitB4 = update(fitB3, .~. + I(rx*log(time)) + I(log(time)^2))
summary(fitB4)$coef
#This result is to be compared with chisq 2df, so it is not at all significant.
fitB3$dev - fitB4$dev
#We can also do these deviance comparisons all at once by looking at an “analysis of deviance” table
anova(fitB4)
Devs = c(fitB1$null.dev, fitB1$dev, fitB3$dev, update(fitB3, .~.+I(rx*log(time)))$dev, fitB4$dev)
Devs
round (-diff(Devs), 3 )### successive differences of llks
plot(fitB1$residuals)
plot(fitB2$residuals)
plot(fitB3$residuals)
plot(fitB4$residuals)
#------------------------------------------------------------------------------
#Using the Longley dataset, a macroeconomic data set which provides a well-known example for a highly collinear regression.
require(stats); require(graphics)
longley.x <- data.matrix(longley[, 1:6])
summary(longley.x)
longley.y <- longley[, "Employed"]
pairs(longley, main = "longley data")
longley.mod1<-glm(Employed ~ .,data=longley,family=gaussian(identity))
summary(longley.mod1)
longley.mod1<-glm(formula = Employed ~ ., family = Gamma(inverse), data = longley)
longley.mod2<-glm(formula = Employed ~ ., family = gaussian(identity), data = longley)
longley.mod3<-glm(formula = Employed ~ ., family = poisson(log), data = longley)
longley.mod4<-glm(formula = Employed ~ ., family = inverse.gaussian, data = longley)
rbind(Gamma= longley.mod1$coef, gaussian= longley.mod2$coef, poisson=longley.mod3$coeff, inverse.gaussian=longley.mod4$coef)
rbind(Gamma= longley.mod1$aic, gaussian= longley.mod2$aic, poisson=longley.mod3$aic, inverse.gaussian=longley.mod4$aic)
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# LESSON 13
#-----------------------------------------------------------------
#Poisson Regression
require(ggplot2)
require(sandwich)
require(msm)
#In this example, num_awards is the outcome variable and indicates the number of awards earned by students at a high school in a year, math is a continuous predictor variable and represents students' scores on their math final exam, and prog is a categorical predictor variable with three levels indicating the type of program in which the students were enrolled. It is coded as 1 = "General", 2 = "Academic" and 3 = "Vocational". Let's start with loading the data and looking at some descriptive statistics.
p <- read.csv("http://www.ats.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id <- factor(id)
})
summary(p)
#We can use the tapply function to display the summary statistics by program type.
with(p, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
#A conditional histogram separated out by program type is plotted to show the distribution.
ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")
#Poisson regression - Poisson regression is often used for modeling count data. Poisson regression has a number of extensions useful for count models.
#Poisson model analysis using the glm function.
m1 <- glm(num_awards ~ prog + math, family="poisson", data=p)
summary(m1)
#Use the R package sandwich to obtain the robust standard errors and calculate the p-values.
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
"Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
LL = coef(m1) - 1.96 * std.err,
UL = coef(m1) + 1.96 * std.err)
r.est
##Deviance residuals are approximately normally distributed if the model is specified correctly. In our example, it shows a little bit of skeweness since median is not quite zero.
##Poisson regression coefficients for each of the variables along with the standard errors, z-scores, p-values and 95% confidence intervals for the coefficients. 
###The coefficient for math is .07. This means that the expected log count for a one-unit increase in math is .07. 
###The indicator variable progAcademic compares between prog = "Academic" and prog = "General", the expected log count for prog = "Academic" increases by about 1.1. 
###The indicator variable prog.Vocational is the expected difference in log count (\(\approx .37\)) between prog = "Vocational" and the reference group (prog = "General").
###Use the residual deviance to perform a goodness of fit test for the overall model. The residual deviance is the difference between the deviance of the current model and the maximum deviance of the ideal model where the predicted values are identical to the observed. If the residual difference is small enough, the goodness of fit test will not be significant, indicating that the model fits the data. This model fits reasonably well because the goodness-of-fit chi-squared test is not statistically significant.
with(m1, cbind(res.deviance = deviance, df = df.residual,
  p = pchisq(deviance, df.residual, lower.tail=FALSE)))
#test the overall effect of prog by comparing the deviance of the full model with the deviance of the model excluding prog. The two degree-of-freedom chi-square test indicates that prog, taken together, is a statistically significant predictor of num_awards.
## update m1 model dropping prog
m2 <- update(m1, . ~ . - prog)
## test model differences with chi square test
anova(m2, m1, test="Chisq")
#To compute the standard error for the incident rate ratios, use the Delta method. Use the function deltamethod implemented in R package msm.
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), coef(m1), cov.m1)
## exponentiate old estimates dropping the p values
rexp.est <- exp(r.est[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s
rexp.est
##The output above indicates that the incident rate for prog = "Academic" is 2.96 times the incident rate for the reference group (prog = "General"). 
##The incident rate for prog = "Vocational" is 1.45 times the incident rate for the reference group holding the other variables at constant. 
##The percent change in the incident rate of num_awards is by 7% for every unit increase in math. 
#What are the expected counts for each program type holding math score at its overall mean? To answer this question, use of the predict function. First, make a small data set to apply the predict function to it.
(s1 <- data.frame(math = mean(p$math),
  prog = factor(1:3, levels = 1:3, labels = levels(p$prog))))
predict(m1, s1, type="response", se.fit=TRUE)
##The predicted number of events for level 1 of prog is about .21, holding math at its mean. 
##The predicted number of events for level 2 of prog is higher at .62, and the predicted number of events for level 3 of prog is about .31. 
##The ratios of these predicted counts (\(\frac{.625}{.211} = 2.96\), \(\frac{.306}{.211} = 1.45\)) match the IRR.
#graph the predicted number of events with the commands below. The graph indicates that the most awards are predicted for those in the academic program (prog = 2), especially if the student has a high math score. The lowest number of predicted awards is for those students in the general program (prog = 1). The graph overlays the lines of expected values onto the actual points, although a small amount of random noise was added vertically to lessen overplotting.
## calculate and store predicted values
p$phat <- predict(m1, type="response")

## order by program and then by math
p <- p[with(p, order(prog, math)), ]

## create the plot
ggplot(p, aes(x = math, y = phat, colour = prog)) +
  geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Math Score", y = "Expected number of awards")
#---------------------------------------------------------------------------------
#Negative binomial regression is for modeling count variables, usually for over-dispersed count outcome variables.
require(foreign)
require(ggplot2)
require(MASS)
#School administrators study the attendance behavior of high school juniors at two schools. Predictors of the number of days of absence include the type of program in which the student is enrolled and a standardized test in math.
#Attendance data on 314 high school juniors from two urban high schools is in the file nb_data. The response variable of interest is days absent, daysabs. The variable math gives the standardized math score for each student. The variable prog is a three-level nominal variable indicating the type of instructional program in which the student is enrolled.
#The foreign package allows us to read .dta files.
dat <- read.dta("http://www.ats.ucla.edu/stat/stata/dae/nb_data.dta")
dat <- within(dat, {
  prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
  id <- factor(id)
})
summary(dat)
ggplot(dat, aes(daysabs, fill = prog)) +
  geom_histogram(binwidth=1) +
  facet_grid(prog ~ ., margins=TRUE, scales="free")
##Each variable has 314 valid observations and their distributions seem quite reasonable. The unconditional mean of our outcome variable is much lower than its variance.
#The table below shows the average numbers of days absent by program type and seems to suggest that program type is a good candidate for predicting the number of days absent, our outcome variable, because the mean value of the outcome appears to vary by prog. The variances within each level of prog are higher than the means within each level. These are the conditional means and variances. These differences suggest that a Poisson model, in which these values are assumed to be equal, could be inappropriate to this data.
with(dat, tapply(daysabs, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
#stats::glm does not have a family for negative binomial. Use the glm.nb function from the MASS package to estimate a negative binomial regression.
summary(m1 <- glm.nb(daysabs ~ math + prog, data = dat))
##The variable math has a coefficient of -0.006, which is statistically significant. This means that for each one-unit increase in math, the expected log count of the number of days absent decreases by 0.006. 
##The indicator variable shown as progAcademic is the expected difference in log count between group 2 and the reference group (prog=1). 
##The expected log count for level 2 of prog is 0.44 lower than the expected log count for level 1. 
##The indicator variable for progVocational is the expected difference in log count between group 3 and the reference group.
##The expected log count for level 3 of prog is 1.28 lower than the expected log count for level 1. 
##To determine if prog itself, overall, is statistically significant, we can compare a model with and without prog. 
##The reason it is important to fit separate models, is that unless we do, the overdispersion parameter is held constant.
m2 <- update(m1, . ~ . - prog)
anova(m1, m2)
##The two degree-of-freedom chi-square test indicates that prog is a statistically significant predictor of daysabs.
##The null deviance is calculated from an intercept-only model with 313 degrees of freedom. The residual deviance, the deviance from the full model. The AIC and 2*log likelihood are also shown.
##The theta parameter shown is the dispersion parameter. 
#As we mentioned earlier, negative binomial models assume the conditional means are not equal to the conditional variances. Use a likelihood ratio test to compare these two and test this model assumption. To do this, run our model as a Poisson.
m3 <- glm(daysabs ~ math + prog, family = "poisson", data = dat)
X2 <- 2 * (logLik(m1) - logLik(m3))
X2
pchisq(X2, df = 1, lower.tail=FALSE)
##In this example, 2 times the difference in log likelihoods is 926, which in the case of nested models is distributed as chi-square with degrees of freedom equal to the difference in the degrees of freedom of the two models, here df=5-4=1. This very large chi-square strongly suggests the negative binomial model, which estimates the dispersion parameter, is more appropriate than the Poisson model.
#We can get the confidence intervals for the coefficients by profiling the likelihood function.
(est <- cbind(Estimate = coef(m1), confint(m1)))
#We might be interested in looking at incident rate ratios rather than coefficients. To do this, we can exponentiate our model coefficients. The same applies to the confidence intervals.
exp(est)
##The output above indicates that the incident rate for prog = 2 is 0.64 times the incident rate for the reference group (prog = 1). Likewise, the incident rate for prog = 3 is 0.28 times the incident rate for the reference group holding the other variables constant. The percent change in the incident rate of daysabs is a 1% decrease for every unit increase in math.
#Look at predicted counts for each value of prog while holding math at its mean. To do this, create a new dataset with the combinations of prog and math to find predicted values, then use the predict command.
newdata1 <- data.frame(math = mean(dat$math),
  prog = factor(1:3, levels = 1:3, labels = levels(dat$prog)))
newdata1$phat <- predict(m1, newdata1, type = "response")
newdata1
##the predicted number of events (e.g., days absent) for a general program is about 10.24, holding math at its mean. The predicted number of events for an academic program is lower at 6.59, and the predicted number of events for a vocational program is about 2.85.
#Obtain the mean predicted number of events for values of math across its entire range for each level of prog and graph these.
newdata2 <- data.frame(
  math = rep(seq(from = min(dat$math), to = max(dat$math), length.out = 100), 3),
  prog = factor(rep(1:3, each = 100), levels = 1:3, labels =
  levels(dat$prog)))
newdata2 <- cbind(newdata2, predict(m1, newdata2, type = "link", se.fit=TRUE))
newdata2 <- within(newdata2, {
  DaysAbsent <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})
ggplot(newdata2, aes(math, DaysAbsent)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = prog), alpha = .25) +
  geom_line(aes(colour = prog), size = 2) +
  labs(x = "Math Score", y = "Predicted Days Absent")
##The graph shows the expected count across the range of math scores, for each type of program along with 95 percent confidence intervals. Note that the lines are not straight because this is a log linear model, and what is plotted are the expected values, not the log of the expected values.