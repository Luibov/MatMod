x = c(21, 34, -1, -10, 0, 38, 19, 2)
median(x)
x = c(21, 34, -1, -10, 0, 38, 19, 2)
x = sort(x)
x
x[(length(x)/2)]
x[round(length(x)/2)] 
med = function(p) 
{sort(p) 
  return(p[round(length(p)/2)])}
x
med(x)
