M=2000
N=1000
A <- matrix(runif(M*N,-1,1),M,N)
J=t(A) %*% A
Eigen=eigen(J)

alpha <- M/N
VarX=4/12
b = VarX*(sqrt(alpha)+1)^2
a = VarX*(sqrt(alpha)-1)^2

f <- function(x){
  sqrt((b-x)*(x-a))/(2*pi*VarX*x)
}

#par(mfrow=c(2,2))

hist(Eigen$values/1000,
     freq=FALSE, 
     breaks=seq(0,2.0,by=0.02),
     main="Marchenko Pastur Law")
curve( f, a, b, add=TRUE, col="blue")
