# Classification and Regression Training
# Adapted from the caret vignette
#---------------------------------------
# caretList is a flexible function for fitting many different caret models, with the same resampling parameters, to the same dataset. 
# It returns a convenient list of caret objects which can later be passed to caretEnsemble and caretStack. 
# caretList has almost exactly the same arguments as train (from the caret package), with the exception that the trControl argument comes last. 
# It can handle both the formula interface and the explicit x, y interface to train. 
# As in caret, the formula interface introduces some overhead and the x, y interface is preferred.
# caretEnsemble has 2 arguments that can be used to specify which models to fit: methodList and tuneList. 
# methodList is a simple character vector of methods that will be fit with the default train parameters.
# tuneList can be used to customize the call to each component model and will be discussed in more detail later. 
# First, lets build an example dataset (adapted from the caret vignette):
library("caret")
library("mlbench")
library("pROC")
data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTrain,]
testing <- Sonar[-inTrain,]
my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(training$Class, 25),
  summaryFunction=twoClassSummary
  )
#Now we can use caretList to fit a series of models (each with the same trControl):
library("rpart")
library("caretEnsemble")
model_list <- caretList(
  Class~., data=training,
  trControl=my_control,
  methodList=c("glm", "rpart")
)
# We can use the predict function to extract predicitons from this object for new data:
p <- as.data.frame(predict(model_list, newdata=head(testing)))
print(p)
# If you desire more control over the model fit, use the caretModelSpec to contruct a list of model specifications for the tuneList argument. 
#This argumenent can be used to fit several different variants of the same model, and can also be used to pass arguments through train down to the component functions (e.g. trace=FALSE for nnet):
library("mlbench")
library("randomForest")
library("nnet")
model_list_big <- caretList(
  Class~., data=training,
  trControl=my_control,
  metric="ROC",
  methodList=c("glm", "rpart"),
  tuneList=list(
    rf1=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2)),
    rf2=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=10), preProcess="pca"),
    nn=caretModelSpec(method="nnet", tuneLength=2, trace=FALSE)
  )
)
# caretList is the preferred way to construct list of caret models in this package, as it will ensure the resampling indexes are identical across all models. Lets take a closer look at our list of models:
xyplot(resamples(model_list))
# As you can see from this plot, these 2 models are un-correlated, and the rpart model is ocassionally anti-predictive, with a few re-samples showing AUCS around 0.3 to 0.4.
# We can confirm the 2 model"s correlation with the modelCor function from caret (caret has a lot of convienent functions for analyzing lists of models):
modelCor(resamples(model_list))
# These 2 models make a good candidate for an ensemble: their predicitons are fairly un-correlated, but their overall accuaracy is similar. 
# We do a simple, linear greedy optimization on AUC using caretEnsemble:
  greedy_ensemble <- caretEnsemble(
    model_list, 
    metric="ROC",
    trControl=trainControl(
      number=2,
      summaryFunction=twoClassSummary,
      classProbs=TRUE
    ))
summary(greedy_ensemble)
# The ensemble"s AUC on the training set resamples is 0.76, which is about 7% better than the best individual model. 
# We can confirm this finding on the test set:
library("caTools")
model_preds <- lapply(model_list, predict, newdata=testing, type="prob")
model_preds <- lapply(model_preds, function(x) x[,"M"])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=testing, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, testing$Class)
# The ensemble"s AUC on the test set is about 6% higher than the best individual model.
# We can also use varImp to extract the variable importances from each member of the ensemble, as well as the final ensemble model:
  varImp(greedy_ensemble)
# caretStack allows us to move beyond simple blends of models to using "meta-models" to ensemble collections of predictive models. 
# DO NOT use the trainControl object you used to fit the training models to fit the ensemble. The re-sampling indexes will be wrong. 
# Fortunately, you don"t need to be fastidious with re-sampling indexes for caretStack, as it only fits one model, and the defaults train uses will usually work fine:
  glm_ensemble <- caretStack(
    model_list,
    method="glm",
    metric="ROC",
    trControl=trainControl(
      method="boot",
      number=10,
      savePredictions="final",
      classProbs=TRUE,
      summaryFunction=twoClassSummary
    )
  )
  model_preds2 <- model_preds
  model_preds2$ensemble <- predict(glm_ensemble, newdata=testing, type="prob")
  CF <- coef(glm_ensemble$ens_model$finalModel)[-1]
  colAUC(model_preds2, testing$Class) 
  
  CF/sum(CF)
  
# Note that glm_ensemble$ens_model is a regular caret object of class train. 
# The glm-weighted model weights (glm vs rpart) and test-set AUCs are extremely similar to the caretEnsemble greedy optimization.
# We can also use more sophisticated ensembles than simple linear weights, but these models are much more succeptible to over-fitting, and generally require large sets of resamples to train on (n=50 or higher for bootstrap samples). 
# Lets try one anyways:
  library("gbm")
  gbm_ensemble <- caretStack(
    model_list,
    method="gbm",
    verbose=FALSE,
    tuneLength=10,
    metric="ROC",
    trControl=trainControl(
      method="boot",
      number=10,
      savePredictions="final",
      classProbs=TRUE,
      summaryFunction=twoClassSummary
    )
  )
  model_preds3 <- model_preds
  model_preds3$ensemble <- predict(gbm_ensemble, newdata=testing, type="prob")
  colAUC(model_preds3, testing$Class)
# In this case, the sophisticated ensemble is no better than a simple weighted linear combination. 
  # Non-linear ensembles seem to work best when you have:
# 1. Lots of data.
# 2. Lots of models with similar accuracies.
# 3. Your models are un-correllated: each one seems to capture a different aspect of the data, and different models perform best on different subsets of the data.
  