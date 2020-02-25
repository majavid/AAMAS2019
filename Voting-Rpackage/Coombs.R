Coombs<-function(df){
  # An interesting variant of the Hare procedure was proposed by the
  # psychologist Clyde Coombs. It operates exactly as the Hare system
  # does, but instead of deleting alternatives with the fewest first place
  # votes, it deletes those with the most last place votes. (In all other
  # ways, it operates as does the Hare procedure.)
  
  top_candid<-c()
  for (i in 1:nrow(df)) {
    top_candid<-c(top_candid,as.character(df[i,ncol(df)]))
  }
  count_letters<-c()
  for (i in 1:ncol(df)) {
    count_letters[i]<-sum(top_candid==colnames(df)[i])
  }
  if(length(which(count_letters==max(count_letters)))==ncol(df)){
    #message("*")
    return(colnames(df)[which(count_letters==max(count_letters))])
  }else if(length(which(count_letters==max(count_letters)))==(ncol(df)-1)){
    return(colnames(df)[which(count_letters!=max(count_letters))])
  }else{
    hold<-colnames(df)[which(count_letters!=max(count_letters))]
    #message("&",hold)
    remov<-colnames(df)[which(count_letters==max(count_letters))]
    #message("$",remov," class ",class(remov))
    newdf<-setNames(data.frame(matrix(ncol = length(hold), nrow = nrow(df))), hold)
    #message(colnames(newdf),"*")
    for (i in 1:nrow(df)) {
      newdf[i,]<-setdiff(as.character(df[i,]),remov)
      #message(i,"->",newdf[i,]," when ",setdiff(df[i,],remov), " and ", df[i,])
    }
    Coombs(newdf)
  }
}