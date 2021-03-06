#' Compare hazard rates of two samples
#'
#' @param x a vector of data from the first sample
#' @param y a vector of data from the second sample
#' @return Returns the test statistic and p-value for the hazard rate test
#' @examples
#' x = rexp(n=10, rate=1)
#' y = rexp(n=10, rate=1/2)
#' hazard_test(x,y)
#'
#' @import stats
#' @import utils
#' @export
hazard_test = function(x,y){
  n = length(x)
  m = length(y)
  N = n+m
  x.pairs = t(combn(x,2))
  y.pairs = t(combn(y,2))
  phi = kernel(x.pairs, y.pairs)
  W = phi/choose(n,2)/choose(m,2)
  null.var = 16*m*n*N-(11*m^2 + 11*n^2+6*n*m)-3*N+8
  null.var = null.var/210/choose(n,2)/choose(m,2)
  Z = W/sqrt(null.var)

  #Normal Approximation
  if(n >= 10 | m >= 10){p.value = 2*pnorm(-abs(Z))}
  #Exact Distributions for Small Samples
  else
  {
    u = min(n,m)
    v = max(n,m)
    if (u==3 & v==3){dist = n3m3}
    if (u==3 & v==4){dist = n3m4}
    if (u==3 & v==5){dist = n3m5}
    if (u==3 & v==6){dist = n3m6}
    if (u==3 & v==7){dist = n3m7}
    if (u==3 & v==8){dist = n3m8}
    if (u==3 & v==9){dist = n3m9}
    if (u==4 & v==4){dist = n4m4}
    if (u==4 & v==5){dist = n4m5}
    if (u==4 & v==6){dist = n4m6}
    if (u==4 & v==7){dist = n4m7}
    if (u==4 & v==8){dist = n4m8}
    if (u==4 & v==9){dist = n4m9}
    if (u==5 & v==5){dist = n5m5}
    if (u==5 & v==6){dist = n5m6}
    if (u==5 & v==7){dist = n5m7}
    if (u==5 & v==8){dist = n5m8}
    if (u==5 & v==9){dist = n5m9}
    if (u==6 & v==6){dist = n6m6}
    if (u==6 & v==7){dist = n6m7}
    if (u==6 & v==8){dist = n6m8}
    if (u==6 & v==9){dist = n6m9}
    if (u==7 & v==7){dist = n7m7}
    if (u==7 & v==8){dist = n7m8}
    if (u==7 & v==9){dist = n7m9}
    if (u==8 & v==8){dist = n8m8}
    if (u==8 & v==9){dist = n8m9}
    if (u==9 & v==9){dist = n9m9}
    dist = as.data.frame(dist)
    p.value = sum(dist$Probability[dist$W>=abs(W) | dist$W<=-abs(W)])
  }
  out = list(W, Z, p.value)
  names(out) = c("W", "Z", "p.value")
  return(out)
}
