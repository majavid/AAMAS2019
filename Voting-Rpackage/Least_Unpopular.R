LU<-function(df){
  # (The Least Unpopular procedure (L.U)) The social choice(s) in this procedure is
  # (are) the alternative(s) that appear(s) less than the others at the bottom of individual preference
  # lists.
  top_candid<-c()
  for (i in 1:nrow(df)) {
    top_candid<-c(top_candid,as.character(df[i,ncol(df)]))
  }
  count_letters<-c()
  for (i in 1:ncol(df)) {
    count_letters[i]<-sum(top_candid==colnames(df)[i])
  }
  colnames(df)[which(count_letters==min(count_letters))]
}