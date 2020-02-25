Condorcet<-function(df){
  # The social choice procedure known as Condorcet's method tries to take
  # advantage of the success enjoyed by majority rule when there are only
  # two alternatives. lt does this by seeking an alternative that would, on
  # the basis of the individual preference lists, defeat (or tie) every other
  # alternative if the election had been between these two alternatives.
  # Thus, with Condorcet's method, an alternative x is among the winners
  # if for every other alternative y, at least half the voters rank x over y
  # on their ballots. Although this method is typically attributed to the
  # Marquis de Condorcet (1743-1794), it dates back at least to Ramon
  # Llull in the thirteenth century.
  
  count_letters<-c()
  count<-c()
  for (i in 1:ncol(df)) {
    count_letters[i]<-0
  }
  for (i in 1:(ncol(df)-1)) {
    for (j in (i+1):ncol(df)) {
      count[i]<-0
      count[j]<-0
      for (k in 1:nrow(df)) {
        if(((which(df[k,]==colnames(df)[i]))<(which(df[k,]==colnames(df)[j])))){
          count[i]<-count[i]+1
        }else{
          count[j]<-count[j]+1
        }
      }
      if(count[i]>count[j]){
        count_letters[i]<-count_letters[i]+1
      }else if(count[i]<count[j]){
        count_letters[j]<-count_letters[j]+1
      }else{
        count_letters[i]<-count_letters[i]+1
        count_letters[j]<-count_letters[j]+1
      }
    }
  }
  #message()
  if(max(count_letters)<(ncol(df)-1)){
    return("NW")##NW means No Winner
  }else{
    return(letters[which(count_letters==(ncol(df)-1))])
  }
}