x<-c(39,65,62,90,82,75,20,98,36,78)
y<-c(47,53,58,86,62,68,65,91,51,84)
#extracting the coefficients
model<-lm(y~x)
model
b0<-coef(model)[1]
b1<-coef(model)[2]
#computing the predictor model yhat
yhat<-b0+b1*x
yhat
#displaying the actual and predicted value of y
cbind(y,yhat)
#computing the value of SSE
SSE<-0
for(i in 1: length(y)){
  SSE=SSE+(Y[i]-yhat[i])^2
}
 print(SSE)
 #computing MSE
 MSE<-SSE/length(y)
 print(MSE)
 #computing RMSE
 RMSE<-sqrt(MSE)
 print(RMSE)
  