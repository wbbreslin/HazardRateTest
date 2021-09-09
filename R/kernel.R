kernel = function(xx,y){
  #Sequence: Y<Y<X<X
  p1 = choose(sum(y<min(xx)),2)
  #Sequence: X<Y<Y<X
  p2 = choose(sum(min(xx)<y & y<max(xx)),2)
  #Sequence: X<X<Y<Y
  m1 = choose(sum(y>max(xx)),2)
  #Sequence: Y<X<X<Y
  m2 = sum(y>max(xx))*sum(y<min(xx))
  #Summation
  total = p1+p2-m1-m2
  return(total)
}
