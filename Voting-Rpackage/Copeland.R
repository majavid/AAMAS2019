Copeland<-function(df){
  # The following social choice procedure is due to A.H. Copeland. We
  # define the win-loss record for an alternative to be the number of
  # strict wins against other alternatives in a head-to-head competition
  # minus the number of strict losses. For example, the win-loss record
  # of a Condorcet winner is equal to one less than the total number of
  # alternatives. Under Copeland's procedure, an alternative is a winner
  # if no alternative has a strictly better win-loss record.
  count<-c()
  count_win<-c()
  count_loss<-c()
  total_count<-c()
  for (i in 1:ncol(df)) {
    count_win[i]<-0
    count_loss[i]<-0
    total_count[i]<-0
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
        count_win[i]<-count_win[i]+1
        count_loss[j]<-count_loss[j]+1
      }else if(count[i]<count[j]){
        count_loss[i]<-count_loss[i]+1
        count_win[j]<-count_win[j]+1
      }else{
        count_loss[i]<-count_loss[i]
        count_win[i]<-count_win[i]
        count_loss[j]<-count_loss[j]
        count_win[j]<-count_win[j]
      }
    }
  }
  for (i in 1:ncol(df)) {
    total_count[i]<-count_win[i]-count_loss[i]
  }
  colnames(df)[which(total_count==max(total_count))]
}