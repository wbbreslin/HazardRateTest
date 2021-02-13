kernel = function(x.pairs,y.pairs){
  w=0
  for (i in 1:nrow(x.pairs)){
    for (j in 1:nrow(y.pairs)){
      x1 = x.pairs[i,1]; x2 = x.pairs[i,2]
      y1 = y.pairs[j,1]; y2 = y.pairs[j,2]
      if (min(x1,x2) > max(y1,y2)){
        w = w+1}
      if (max(x1,x2) > max(y1,y2) & min(x1,x2) < min(y1,y2)){
        w = w+1}
      if (min(y1,y2) > max(x1,x2)){
        w = w-1}
      if (max(y1,y2) > max(x1,x2) & min(y1,y2) < min(x1,x2)){
        w = w-1}}}
  return(w)}
