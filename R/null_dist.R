#' Finds the exact null distribution for hazard rate test
#'
#' @param m the sample size of the first sample
#' @param n the sample size of the second sample
#' @return Returns a frequency distribution table for the hazard rate test statistic
#' @examples
#' null_dist(3,3)
#'
#' @import stats
#' @import utils
#' @export
null_dist = function(m,n){
  N = m+n
  Ranks = 1:N
  x.data = t(combn(Ranks,m)); y.data=c()
  for (i in 1:nrow(x.data)){
    if (i == 1){
      y.data = t(as.data.frame(setdiff(Ranks,x.data[1,])))}
    else{
      temp = setdiff(Ranks, x.data[i,])
      y.data = rbind(y.data,temp)}
    x.pairs = t(combn(x.data[i,],2))
    y.pairs = t(combn(y.data[i,],2))
    if (i == 1){
      w = kernel(x.pairs,y.pairs)/(choose(n,2)*choose(m,2))}
    else{
      temp = kernel(x.pairs,y.pairs)/(choose(n,2)*choose(m,2))
      w = c(w,temp)}}
  support = sort(unique(w))
  for (i in support){
    if (i == min(support)){
      freq = sum(w==i)
      prob = freq/length(w)}
    else{
      freq = c(freq, sum(w==i))
      prob = c(prob, sum(w==i)/length(w))}}
  table = cbind(support,freq,prob)
  colnames(table)=c("W","Occurrences","Probability")
  return(table)}
