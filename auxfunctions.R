convert2numeric <- function(obj){
  out <- lapply(obj, as.numeric)
  as.data.frame(out)
}

calculateCFS <- function(cormatrix) {
  k=dim(cormatrix)[1]-1
  rcf <- mean(cormatrix[k+1,1:k])
  rff <- (sum(cormatrix[1:k,1:k])-k)/(k*(k-1))
  (k*rcf)/(sqrt(k+k*(k-1)*rff))
}