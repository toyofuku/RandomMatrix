N=1000
X <- matrix(runif(N*N,-1,1),nc=N)
X[lower.tri(X)]<-0
S<-X+t(X)

Eigen=eigen(S)

VarX=4/12
f <- function(x){
  sqrt(4*VarX-x*x)/(2*pi*VarX)
}

#par(mfrow=c(2,2))

hist(Eigen$values/sqrt(N),
     freq=FALSE,
     breaks=seq(-1.5,1.5,by=0.05),
     main="Wigner's Semicircle Law")

curve( f, -2*sqrt(VarX),2*sqrt(VarX), add=TRUE, col="blue")

