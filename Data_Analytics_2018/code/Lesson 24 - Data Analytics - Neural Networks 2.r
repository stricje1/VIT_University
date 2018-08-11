install.packages('neuralnet')
library("neuralnet")
#------------------------------------------------- 
#Going to create a neural network to perform square rooting
#Type ?neuralnet for more information on the neuralnet library 
#Generate 50 random numbers uniformly distributed between 0 and 100
#And store them as a dataframe
#-------------------------------------------------
traininginput <-  as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)
#------------------------------------------------- 
#Column bind the data into one variable
#-------------------------------------------------
trainingdata <- cbind(traininginput,trainingoutput)
colnames(traininginput) <- c("Input","Output")
#------------------------------------------------- 
#Train the neural network
#Going to have 10 hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
#-------------------------------------------------
net.sqrt <- neuralnet(Output~Input,traininginput, hidden=10, threshold=0.01)
print(net.sqrt)
plot(net.sqrt)
#-------------------------------------------------
traininginput <-  as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")
net.sqrt <- neuralnet(Output~Input,trainingdata, hidden=10, threshold=0.01)
print(net.sqrt)
plot(net.sqrt)
#-------------------------------------------------
testdata <- as.data.frame((1:10)^2) 
ls(net.sqrt)
print(net.sqrt$net.result)
#-------------------------------------------------
cleanoutput <- cbind(testdata,sqrt(testdata), as.data.frame(net.sqrt$net.result))
colnames(cleanoutput) <- c(“Input”,”Expected Output”,”Neural Net Output”)
print(cleanoutput)
#*****************************************************************************
# infert is a dataset for Infertility after Spontaneous and Induced Abortion and and consist of 248 observations and 8 variables: education, age, parity, induced, case, spontaneous, stratum, pooled.stratum
#The following code runs the network (with 2 hidden layers) classifying ‘case’  (a binary variable) as a function of several independent varaibles. The neural network is estimated, and the results are stored in the data frame ‘nn.’
#As can be seen below, the output weights correspond directly with the visualized network.
#Example: The weight for the path from input ‘age’ to the first hidden layer is  -3.0689 (age.to.1layhid1) which can easily be found in the network diagram. After all inputs feed into hidden layer 1, the weight associated with the path from hidden layer 1(1layhid.1.to.case) to the output layer (which along with information from the other layers of the network will give us the classification of ‘case’) is -1001.15.
#------------------------------------------------------------------
#   Get the data
#------------------------------------------------------------------ 
 
library(datasets)
 
names(infert)
 
#------------------------------------------------------------------
#  Train the network
#------------------------------------------------------------------
 
library(neuralnet)
 
nn <- neuralnet(
 case~age+parity+induced+spontaneous,
 data=infert, hidden=2, err.fct="ce",
 linear.output=FALSE)
 
#------------------------------------------------------------------
#   Output training results
#------------------------------------------------------------------  
 
# basic
nn
 
# reults options
names(nn)
 
 
# result matrix
 
nn$result.matrix
 
# The given data is saved in nn$covariate and
# nn$response as well as in nn$data for the whole data
# set inclusive non-used variables. The output of the
# neural network, i.e. the fitted values o(x), is provided
# by nn$net.result:
 
out <- cbind(nn$covariate,nn$net.result[[1]])
 
dimnames(out) <- list(NULL, c("age", "parity","induced","spontaneous","nn-output"))
 
head(out)
 
# generalized weights
 
# The generalized weight expresses the effect of each
# ovariate xi and thus has an analogous interpretation
# as the ith regression parameter in regression models.
# However, the generalized weight depends on all
# other covariates. Its distribution indicates whether
# the effect of the covariate is linear since a small variance
# suggests a linear effect
 
# The columns refer to the four covariates age (j =
# 1), parity (j = 2), induced (j = 3), and spontaneous (j=4)
 
head(nn$generalized.weights[[1]])
 
# visualization
 
plot(nn)

#****************************************************************
# The dataset
#****************************************************************
# We are going to use the Boston dataset in the MASS package.
# The Boston dataset is a collection of data about housing values in the suburbs of Boston. Our goal is to predict the median value of owner-occupied homes (medv) using all the other continuous variables available.
#-------------------------------------------------------------
set.seed(500)
library(MASS)
data <- Boston
#-------------------------------------------------------------
# First we need to check that no datapoint is missing, otherwise we need to fix the dataset.
#-------------------------------------------------------------
apply(data,2,function(x) sum(is.na(x)))
#-------------------------------------------------------------
# If there is no missing data, then good. We proceed by randomly splitting the data into a train and a test set, then we fit a linear regression model and test it on the test set. 
#-------------------------------------------------------------
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
#-------------------------------------------------------------
# Before fitting a neural network, some preparation need to be done. Neural networks are not that easy to train and tune.
# As a first step, we are going to address data preprocessing. It is good practice to normalize your data before training a neural network. We therefore scale and split the data before moving on:
#-------------------------------------------------------------
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
#-------------------------------------------------------------
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]
#-------------------------------------------------------------
# As far as I know there is no fixed rule as to how many layers and neurons to use although there are several more or less accepted rules of thumb. Usually, if at all necessary, one hidden layer is enough for a vast numbers of applications. As far as the number of neurons is concerned, it should be between the input layer size and the output layer size, usually 2/3 of the input size.
# Since this is a toy example, we are going to use 2 hidden layers with this configuration: 13:5:3:1. The input layer has 13 inputs, the two hidden layers have 5 and 3 neurons and the output layer has, of course, a single output since we are doing regression.
#  Now fit the neural network:
## For some reason the formula y~. is not accepted in the neuralnet() function. You need to first write the formula and then pass it as an argument in the fitting function.
## The hidden argument accepts a vector with the number of neurons for each hidden layer, while the argument linear.output is used to specify whether we want to do regression linear.output=TRUE or classification linear.output=FALSE
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
#-------------------------------------------------------------
# The neuralnet package provides a nice tool to plot the model:
#-------------------------------------------------------------
plot(nn)
#-------------------------------------------------------------
#The black lines show the connections between each layer and the weights on each connection while the blue lines show the bias term added in each step. The bias can be thought as the intercept of a linear model.
#The network is essentially a black box so we cannot say that much about the fitting, the weights and the model. Suffice to say that the training algorithm has converged and therefore the model is ready to be used.
#Predicting medv using the neural network
#Now we can try to predict the values for the test set and calculate the MSE. Remember that the net will output a normalized prediction, so we need to scale it back in order to make a meaningful comparison (or just a simple prediction).
#-------------------------------------------------------------
pr.nn <- compute(nn,test_[,1:13])
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
#-------------------------------------------------------------
#we then compare the two MSEs
#-------------------------------------------------------------
print(paste(MSE.lm,MSE.nn))
#-------------------------------------------------------------
#Apparently the net is doing a better work than the linear model at predicting medv. Once again, be careful because this result depends on the train-test split performed above. Below, after the visual plot, we are going to perform a fast cross validation in order to be more confident about the results.
#A first visual approach to the performance of the network and the linear model on the test set is plotted below
#-------------------------------------------------------------
par(mfrow=c(1,2))

plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)
#-------------------------------------------------------------
#By visually inspecting the plot we can see that the predictions made by the neural network are (in general) more concetrated around the line (a perfect alignment with the line would indicate a MSE of 0 and thus an ideal perfect prediction) than those made by the linear model.
#-------------------------------------------------------------
plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))
#-------------------------------------------------------------
#A (fast) cross validation
#Cross validation is another very important step of building predictive models. While there are different kind of cross validation methods, the basic idea is repeating the following process a number of time:
#-------------------------------------------------------------
#train-test split
##Do the train-test split
##Fit the model to the train set
##Test the model on the test set
##Calculate the prediction error
##Repeat the process K times

#Then by calculating the average error we can get a grasp of how the model is doing.

#We are going to implement a fast cross validation using a for loop for the neural network and the cv.glm() function in the boot package for the linear model.
#As far as I know, there is no built-in function in R to perform cross validation on this kind of neural network, if you do know such a function, please let me know in the comments. Here is the 10 fold cross validated MSE for the linear model:
#-------------------------------------------------------------
library(boot)
set.seed(200)
lm.fit <- glm(medv~.,data=data)
cv.glm(data,lm.fit,K=10)$delta[1]
#-------------------------------------------------------------
#Now the net. Note that I am splitting the data in this way: 90% train set and 10% test set in a random way for 10 times. I am also initializing a progress bar using the plyr library because I want to keep an eye on the status of the process since the fitting of the neural network may take a while.
#-------------------------------------------------------------
set.seed(450)
cv.error <- NULL
k <- 10

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
    index <- sample(1:nrow(data),round(0.9*nrow(data)))
    train.cv <- scaled[index,]
    test.cv <- scaled[-index,]   
    nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)   
    pr.nn <- compute(nn,test.cv[,1:13])
    pr.nn <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)   
    test.cv.r <- (test.cv$medv)*(max(data$medv)-min(data$medv))+min(data$medv)    
    cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
    pbar$step()
}
#-------------------------------------------------------------
# After a while, the process is done, we calculate the average MSE and plot the results as a boxplot
#-------------------------------------------------------------
mean(cv.error)
cv.error
#-------------------------------------------------------------
# The code for the box plot:
#-------------------------------------------------------------
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)
#-------------------------------------------------------------
# The average MSE for the neural network (10.33) is lower than the one of the linear model although there seems to be a certain degree of variation in the MSEs of the cross validation.
