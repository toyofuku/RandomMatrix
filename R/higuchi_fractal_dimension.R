
N <- 10000

testDataGen <- function(fn) {
  x <- fn(N)
  for(i in 2:N) { x[i] = x[i-1] + x[i] }
  return(x)
}

L <- function(k) {
  aL <- c()
  for(m in 1:k){
    iseq <- seq(m,N,by=k)
    y <- x[iseq]
    vL <- sum(abs(diff(y))) * (N-1) / ((length(y)-1) * k)
    vL <- vL / k
    aL <- c(aL, c(vL))
  }
  return(aL)
}

higuchi <- function(x){
  result <- c()
  p <- floor(log2(N/10))
  for(i in 1:p){
    k <- 2 ** i
    log_k = log(k)
    lk <- L(k)
    log_lk = log(mean(lk))
#    print(c(i,sd(lk)))
    result <- c(result, c(log_k, log_lk))
  }
  dim(result) <- c(2,p)
  return(t(result))  
}

# z2 <- read_delim("~/github/RandomMatrix/unifseq.csv", "\t", escape_double=FALSE, trim_ws=TRUE)

randfn <- c(rnorm, runif, rcauchy)

par(mfrow=c(4,2))
for(i in 1:3){
  x <- testDataGen(randfn[[i]])
  ts.plot(x)
  spec.pgram(x)
}

for(i in 1:3){
  x <- testDataGen(randfn[[i]])
  result <- higuchi(x)
  (paramp <- lsfit(result[,1],result[,2])$coefficients )
  (corp <- cor(result[,1],result[,2]))
  if(i > 1) {
    par(new=T)
  }
  plot(result,ylim=c(0,12))
  abline( paramp )
  message(paste("D =",paramp[[2]]))
}

