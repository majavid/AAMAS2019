Hare<-function(df){
  # The social choice procedure known as the Hare procedure was introduced by 
  # Thomas Hare in 1861, and is also known by names such as the
  # "single transferable vote system" or "instant runoff voting." In 1862,
  # John Stuart Mill spoke of it as being "among the greatest improvements 
  # yet made in the theory and practice of government." Today, it
  # is used to elect public officials in Australia, Malta, the Republic of
  # Ireland, and Northern Ireland.
  # The Hare system is based on the idea of arriving at a social choice
  # by successive deletions of less desirable alternatives. More precisely,
  # the procedure is as follows. We begin by deleting the alternative or
  # alternatives occurring on top of the fewest lists. At this stage we have
  # lists that are at least one alternative shorter than that with which we
  # started. Now, we simply repeat this process of deleting the least desirable
  # alternative or alternatives (as measured by the number of lists
  # on top of which it, or they, appear). The alternative(s) deleted last is
  # declared the winner.
  # Notice that if, at any stage, some alternative occurs at the top of
  # more than half the lists, then that alternative will turn out to be the
  # unique winner. However, an alternative occurring at the top of exactly
  # half the lists-even if it is the only one doing so-is not necessarily the
  # unique winner (although it must be among the winners).
  top_candid<-c()
  for (i in 1:nrow(df)) {
    top_candid<-c(top_candid,as.character(df[i,1]))
  }
  count_letters<-c()
  for (i in 1:ncol(df)) {
    count_letters[i]<-sum(top_candid==colnames(df)[i])
  }
  if(length(which(count_letters==min(count_letters)))==ncol(df)){
    #message("*")
    return(colnames(df)[which(count_letters==min(count_letters))])
  }else{
    hold<-colnames(df)[which(count_letters!=min(count_letters))]
    #message("&",hold)
    remov<-colnames(df)[which(count_letters==min(count_letters))]
    #message("$",remov," class ",class(remov))
    newdf<-setNames(data.frame(matrix(ncol = length(hold), nrow = nrow(df))), hold)
    #message(colnames(newdf),"*")
    for (i in 1:nrow(df)) {
      newdf[i,]<-setdiff(as.character(df[i,]),remov)
      #message(i,"->",newdf[i,]," when ",setdiff(df[i,],remov), " and ", df[i,])
    }
    Hare(newdf)
  }
}