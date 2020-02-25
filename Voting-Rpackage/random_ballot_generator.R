random_ballot_generator <-function(m,n){
  candida<-letters[1:n]
  df<-setNames(data.frame(matrix(ncol = n, nrow = m)), candida)
  for(i in 1:m){
    df[i,]<-sample(candida)
  }
  df
}
