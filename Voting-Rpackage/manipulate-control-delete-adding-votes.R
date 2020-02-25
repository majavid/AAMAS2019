manipulate<-function(df,percent){
  newdf<-df
  insert<-sample(colnames(df))
  r<-ceiling(percent*0.01*nrow(df))
  row_candid<-sample(1:nrow(df),r)
  for (i in 1:length(row_candid)) {
    #message(row_candid[i])
    newdf[row_candid[i],]<-insert
  }
  newdf
}