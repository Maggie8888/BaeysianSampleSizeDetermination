Afun <- function(x1, n2, pi, R1, R2){
  y<-min(n2,(n2+x1-R1-R2))
  return(pbinom(y,n2,(1-pi)))
}

Maximumfunc<- function(lambda,n1,x1,n2,pi0,pi1,R1,R2){
  power0=dbinom(x1,n1,pi0)*Afun(x1,n2,pi0,R1,R2)
  power1=dbinom(x1,n1,pi1)*Afun(x1,n2,pi1,R1,R2)
  return(power1-lambda*power0)
}

# approximated solution for R2(x_1)
lambdaFun <- function(lambda, pi0, pi1, n1, R1, newn2){
  m <- n1-R1+1
  newR2 <- rep(0,m)
  for (k in 1:m){
    x1 <- R1+(k-1)
    a <- dbinom(x1,n1,pi0)
    b <- dbinom(x1,n1,pi1)
    p0<- pi0*(1-pi0)
    p1<- pi1*(1-pi1)
    k0<- R1-x1-newn2*pi0
    k1<- R1-x1-newn2*pi1
    A <- 1/p1-1/p0
    B <- 2*k1/p1-2*k0/p0
    C <- k1^2/p1-k0^2/p0+(2*newn2)*log(lambda*a*sqrt(p1)/(b*sqrt(p0)))

    newR2cand<-rep(0,6)
    newR2cand[2]=newn2
    iter=2
    if (A==0){
      if ((-C/B<newn2)&&(-C/B>0))
        newR2cand[3]=floor(-C/B)
      newR2cand[4]=ceiling(-C/B)
      iter=4
    }else{
      if (B^2-4*A*C>0){
        if (((-B+sqrt(B^2-4*A*C))/(2*A)<newn2)&&((-B+sqrt(B^2-4*A*C))/(2*A)>0)){
          newR2cand[3]<- floor((-B+sqrt(B^2-4*A*C))/(2*A))
          newR2cand[4]<- ceiling((-B+sqrt(B^2-4*A*C))/(2*A))
          iter=4}
        if (((-B-sqrt(B^2-4*A*C))/(2*A)<newn2)&&((-B-sqrt(B^2-4*A*C))/(2*A)>0)){
          newR2cand[5]<- floor((-B-sqrt(B^2-4*A*C))/(2*A))
          newR2cand[6]<- ceiling((-B-sqrt(B^2-4*A*C))/(2*A))
          iter=6}
      }
    }
    maxfun=Maximumfunc(lambda,n1,x1, newn2, pi0,pi1, R1,newR2cand[1])
    for (candnum in 2:iter){
      if (Maximumfunc(lambda,n1,x1, newn2, pi0,pi1, R1,newR2cand[candnum])>maxfun){
        minfun=Maximumfunc(lambda,n1,x1, newn2, pi0,pi1, R1,newR2cand[candnum])
        newR2[k]=newR2cand[candnum]
      }
    }
  }
  return(newR2)
}



newR2fun<-function(n1,R1,n2,R2,newn2,pi0,pi1,alpha){
  m=n1-R1+1
  testlambda <- seq(0.1, 10, 0.01)
  typeI <- rep(0, length(testlambda))
  power <- rep(0, length(testlambda))

  for (k in 1:length(testlambda)){
    lambda <- testlambda[k]
    newR2 <- lambdaFun(lambda, pi0, pi1, n1, R1, newn2)
    for (s in 1:m){
      x1=R1+s-1
      typeI[k]=typeI[k]+dbinom(x1,n1,pi0)*Afun(x1, newn2, pi0, R1, newR2[s])
      power[k]=power[k]+dbinom(x1,n1,pi1)*Afun(x1, newn2, pi1, R1, newR2[s])
    }
  }

  testlambda <- testlambda[typeI<=alpha]
  power <- power[typeI<=alpha]
  typeI <- typeI[typeI<=alpha]
  testlambda <- testlambda[order(typeI)]
  power <- power[order(typeI)]
  typeI <- typeI[order(typeI)]
  AGpower <- power[length(typeI)]
  AGtypeI <- typeI[length(typeI)]

  lambda <- testlambda[length(testlambda)]
  newR2 <- lambdaFun(lambda, pi0, pi1, n1, R1, newn2)
  CtypeI <- rep(0, m)
  for (k in 1:m){
    CtypeI[k] <- Afun(R1-1+k, newn2, pi0, R1, newR2[k])
  }

  ###########################################################################

  qx <- rep(0, m)
  for (k in 1:m){
    x1 <- R1+k-1
    qx[k] <- Afun(x1, n2, pi0, R1, R2)
  }

  KCnewR2 <- rep(newn2, m)
  KCtypeI=0;
  KCpower=0;
  KCCtypeI<-rep(0,m)
  for (s in 1:m){
    x1=R1+s-1
    for (testR2 in newn2:1){
      if (Afun(x1, newn2, pi0, R1, testR2)<=qx[s])
        KCnewR2[s] <- testR2
      else break;
      end
    }
    KCCtypeI[s]=Afun(x1, newn2, pi0, R1, KCnewR2[s])
    KCtypeI=KCtypeI+choose(n1,x1)*Afun(x1, newn2, pi0, R1, KCnewR2[s])*pi0^x1*(1-pi0)^(n1-x1)
    KCpower=KCpower+choose(n1,x1)*Afun(x1, newn2, pi1, R1, KCnewR2[s])*pi1^x1*(1-pi1)^(n1-x1)
  }
  AGtypeI=rep(AGtypeI,m)
  AGpower=rep(AGpower,m)
  X1=R1-1+(1:m)
  KCtypeI=rep(KCtypeI, m)
  KCpower=rep(KCpower, m)

  cbind(X1, newR2,AGtypeI, AGpower, CtypeI, KCnewR2, KCtypeI, KCpower, KCCtypeI,qx)
}




###########################################################################
###########################################################################
#Changed Stage II sample size
###########################################################################
###########################################################################
# specify alpha, pi0, pi1,n1, n2, R1, R2, newn2
alpha=0.05;pi0=0.05;pi1=0.15;
n1=30;R1=2;n2=22;R2=4;
newn2=floor(1.5*n2)

res=as.data.frame(newR2fun(n1,R1,n2,R2,newn2,pi0,pi1,alpha))
cbind(pi0,pi1,n1,R1,n2,R2,newn2,res$AGtypeI[1],res$AGpower[1],res$KCtypeI[1],res$KCpower[1])
