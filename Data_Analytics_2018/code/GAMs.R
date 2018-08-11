#generalized additive models (GAMs) 
library(MapGAM)
data(CAdata)
summary(CAdata)
plot(CAdata$X,CAdata$Y) 

data(CAdata)
data(CAmap)
plot(CAmap)
points(CAdata$X,CAdata$Y)

obj <- list(grid=data.frame(CAdata$X,CAdata$Y),fit=CAdata$time) 
colormap(obj, CAmap, legend.name = "Time")

# map the same data using a divergent color palette anchored to the median 
if (require(colorspace)) 
{ 
  newpal <- diverge_hsv(201) # from the colorspace library 
  colormap(obj, CAmap, legend.name = "Time", col.seq = newpal, 
           legend.add.line=median(obj$fit)) 
}

fit <- gamcox(Surv(time,event)~AGE + factor(INS) + lo(X,Y),data=CAdata, 
              span=0.2,loess.trace="approximate") 
fit 
pred = predict(fit) 
colormap(list(fit=pred$pred,grid=data.frame(X=CAdata$X,Y=CAdata$Y)),map=CAmap, 
         border.gray=0.5)


data(CAdata) 
data(CAmap) 
fit <- gamcox(Surv(time,event)~AGE + factor(INS) + lo(X,Y),data=CAdata, 
              span=0.2,loess.trace="approximate") 
fit 
pred1 = predict(fit) 
colormap(list(fit=pred1$pred,grid=data.frame(X=CAdata$X,Y=CAdata$Y)),map=CAmap, 
         border.gray=0.5)
data(CAgrid) 
pred2 = predict(fit,CAgrid[,c("X","Y")]) 
colormap(list(fit=pred2$pred,grid=data.frame(X=CAgrid$X,Y=CAgrid$Y)),map=CAmap, 
         border.gray=0.5, legend.name="log hazard ratio")
## Circle significant areas based on the confidence intervals specified by conf.low and conf.high 

print(fit)
summary(fit)
plot(fit$smooth)
residuals(fit)
plot(residuals(fit))

toformula(data=CAdata, surv=TRUE)


data(CAdata) 
Y = CAdata[,c("time","event")] 
X = CAdata[,c(3:5)] 
eta = coxph(Surv(time,event)~AGE,data=CAdata)$linear.predictors 
result = dls(Y,X,1:2,eta,span=0.2) 
plot(eta,result$deltaeta)


