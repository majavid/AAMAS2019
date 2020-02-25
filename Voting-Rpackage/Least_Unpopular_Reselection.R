LUR<-function(df){
  # (the Least Unpopular Reselection (L.U.R)) First of all, a set of alternatives
  # appearing less than the others at the bottom of individual preference lists are chosen. If this set
  # has only one member, it would be the social choice. Otherwise, the remaining alternatives are
  # removed and the L.U. procedure is run for the set obtained from the first stage. This procedure
  # is repeated until it cannot be continued. The set obtained in the last repetition would be the set
  # of social choice.
  top_candid<-c()
  for (i in 1:nrow(df)) {
    top_candid<-c(top_candid,as.character(df[i,ncol(df)]))
  }
  count_letters<-c()
  for (i in 1:ncol(df)) {
    count_letters[i]<-sum(top_candid==colnames(df)[i])
  }
  if(length(which(count_letters==min(count_letters)))==1 || length(which(count_letters==min(count_letters)))==ncol(df)){
    return(colnames(df)[which(count_letters==min(count_letters))])
  }else{
    remov<-colnames(df)[which(count_letters!=min(count_letters))]
    #message("&",remov)
    hold<-colnames(df)[which(count_letters==min(count_letters))]
    #message("$",hold," class ",class(hold))
    newdf<-setNames(data.frame(matrix(ncol = length(hold), nrow = nrow(df))), hold)
    #message(colnames(newdf),"*")
    for (i in 1:nrow(df)) {
      newdf[i,]<-setdiff(as.character(df[i,]),remov)
      #message(i,"->",newdf[i,]," when ",setdiff(df[i,],hold), " and ", df[i,])
    }
    LUR(newdf)
  }
}