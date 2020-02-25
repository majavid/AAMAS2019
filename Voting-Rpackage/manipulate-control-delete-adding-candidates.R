quit_in_favor_of<-function(df){
  top_candid<-c()
  for (i in 1:nrow(df)) {
    top_candid<-c(top_candid,as.character(df[i,1]))
  }
  count_letters<-c()
  for (i in 1:ncol(df)) {
    count_letters[i]<-sum(top_candid==colnames(df)[i])
  }
  m<-colnames(df)
  n<-count_letters
  temp_df<-rbind(m,n)
  #print(temp_df)
  #message(class(temp_df))
  temp_df<-temp_df[,order(temp_df[2,], decreasing = T)]
  #print(temp_df)
  #message(class(temp_df))
  quitter<-as.character(temp_df[1,3])
  #message(class(quitter))
  supported<-as.character(temp_df[1,2])
  #message("quitter :",quitter," & supported :",supported)
  tempdf<-setNames(data.frame(matrix(ncol = (ncol(df)-1), nrow = nrow(df))), setdiff(colnames(df),quitter))
  #print(tempdf)
  for (i in 1:nrow(df)) {
    if(as.character(df[i,1])==quitter){
      tempdf[i,]<-setdiff(as.character(df[i,]),quitter)
      tempdf[i,]<-c(supported,setdiff(as.character(tempdf[i,]),supported))
    }else{
      tempdf[i,]<-setdiff(as.character(df[i,]),quitter)
    }
  }
  tempdf
}