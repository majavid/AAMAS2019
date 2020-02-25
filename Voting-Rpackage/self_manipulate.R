self_manipulate<-function(df){
  top_candid<-c()
  for (i in 1:nrow(df)) {
    top_candid<-c(top_candid,as.character(df[i,1]))
  }
  count_letters<-c()
  for (i in 1:ncol(df)) {
    count_letters[i]<-sum(top_candid==colnames(df)[i])
  }
  non_zero<-min(count_letters[which(count_letters != 0)])
  #message(which(count_letters != 0),"non_zero: ",non_zero)
  minority<-sample(colnames(df)[count_letters==non_zero],1)
  #message("minority: ",minority)
  secend_majority<-"a"
  tempdf<-setNames(data.frame(matrix(ncol = (ncol(df)-1), nrow = count_letters[which(colnames(df)==minority)])), setdiff(colnames(df),minority))
  for (i in 1:nrow(df)) {
    if(as.character(df[i,1])==minority){
      tempdf[i,]<-setdiff(as.character(df[i,]),minority)
    }else{
      tempdf[i,]<-NA
    }
  }
  #print(na.omit(tempdf))
  #message("sec_voting: ",plurality(na.omit(tempdf)))
  secend_majority<-sample(plurality(na.omit(tempdf)),1)
  #message("secend_majority: ",secend_majority)
  for (i in 1:nrow(df)) {
    if(as.character(df[i,1])==minority && as.character(df[i,2])==secend_majority){
      df[i,][c(1,2)]<-df[i,][c(2,1)]
    }
  }
  df
}