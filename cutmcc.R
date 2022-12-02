library(mltools)
cutoff <- function(n,data,beta,k)
{
  mccp=c(1:n)*0
  cutp=c(1:n)*0
  mccp1=c(1:n)*0
  sigm=c(1:50)/100
  for(i in 1:n){
    test=data[sample(nrow(data),i),]
    output=test[,k]
    X_test = cbind(1,test)
    mp=X_test % * % beta
    valp=pnorm(mp)
    pred=c(1:i)*0
    mccp[i]=0
    for(k in sigm){
      for(j in 1:i){
        if(valp[j]>k)  pred[j]=1
        else pred[j]=0
      }
      mccp1[i]=mcc(pred,output)
      if(mccp1[i]>mccp[i]){
        mccp[i]=mccp1[i]
        cutp[i]=k
      }
      else {mccp[i]=mccp[i]
            cutp[i]=cutp[i]}
    }
  }
  cutp
}
