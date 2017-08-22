X <- matrix(rnorm(120*120),nc=120)
X[lower.tri(X)]<-0
S<-X+t(X)
#diag(S)<-rnorm(120,0,sqrt(2))

par(mfrow=c(2,3))
for(N in seq(20,120,20)){
  Eigen=eigen(S[1:N,1:N])
  hist(Eigen$values/sqrt(N),
       breaks=seq(-4,4,by=0.4),
       main=paste("N",N,sep="="),
       ylim=c(0,N*0.2))
}
