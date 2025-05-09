## R Script for GSOC tests
## Modern time series estimation
### Medium

ar1sim=function(N=100,M=10,correlation=c(-0.99,0.99)){
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  
  if(correlation[1]>correlation[2]){correlation=rev(correlation)} # if not ordered, order it
  if(correlation[1]< (-1)){stop("Lower correlation bound is less than -1")}
  if(correlation[2]>1){stop("Upper correlation bound is larger than 1")}
  if(M<=0){stop("M must be positive")}
  if(!is.wholenumber(M)){stop("M must be an integer")}
  if(N<=0){stop("N must be positive")}
  if(!is.wholenumber(N)){stop("N must be an integer")}
  return(replicate(M,arima.sim(model=list(ar=runif(1,correlation[1],correlation[2])),n=N)))
}

## Testing
sims=ar1sim()
sims=ar1sim(correlation=c(0.2,0.4))
sims=ar1sim(-5)
sims=ar1sim(M=0)
sims=ar1sim(N="some text") # doesn't fail nicely but does fail
sims=ar1sim(M="some text") # doesn't fail nicely but does fail
