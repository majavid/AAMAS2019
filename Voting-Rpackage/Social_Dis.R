social_dis<-function(df){
  # Social disappointment in voting happens when the outcome of a voting system
  # (for 3 or more alternatives) occurs for those alternatives which are at the end of at least half of
  # the individual preference profiles (ballots).
  agenda<-colnames(df)
  top_candid<-c()
  for (i in 1:nrow(df)) {
    top_candid<-c(top_candid,as.character(df[i,ncol(df)]))
  }
  count_letters<-c()
  for (i in 1:ncol(df)) {
    count_letters[i]<-sum(top_candid==colnames(df)[i])
  }
  if(max(count_letters)<ceiling(nrow(df)/2)){
    return("!!!")
  }else{
   return(colnames(df)[which(count_letters==max(count_letters))]) 
  }
} 

