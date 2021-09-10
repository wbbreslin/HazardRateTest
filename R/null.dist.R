#' Finds the exact null distribution for hazard rate test
#'
#' @param m the sample size of the first sample
#' @param n the sample size of the second sample
#' @return Returns a frequency distribution table for the hazard rate test statistic
#' @examples
#' null.dist(3,3)
#'
#' @import stats
#' @import utils
#' @export
null.dist = function(m,n){
  N = n+m
  Ranks = 1:N
  rows = choose(N,n)

  x.data = t(combn(Ranks,m))
  y.data = t(combn(Ranks,n))
  y.data = y.data[rows:1,]

  distribution = rep(NA,rows)

  for (i in 1:rows){
    x.pairs = t(combn(x.data[i,],2))
    y = y.data[i,]
    phi = sum(apply(x.pairs, MARGIN=1, FUN=kernel, y=y))
    phi = phi/choose(n,2)/choose(m,2)
    distribution[i] = phi
  }

  out = as.data.frame(table(distribution))
  out$Probability = out$Freq/sum(out$Freq)
  names(out) = c("Test Statistic", "Frequency", "Probability")
  return(out)}
