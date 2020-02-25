plurality <- function(df){
  # Majority rule is the social choice procedure for two alternatives in which 
  # an alternative is a winner if it appears at the top of at
  # least half of the individual preference lists (equivalently, if at least half
  # of the voters vote for that alternative).
  # Plurality voting is the social choice procedure that most directly generalizes 
  # the idea of simple majority vote from the easy case of two alternatives to the 
  # complicated case of three or more alternatives. The idea
  # is simply to declare as the social choice(s) the alternative(s) with the
  # largest number of first-place rankings in the individual preference lists.
  top_candid<-c()
  for (i in 1:nrow(df)) {
    top_candid<-c(top_candid,as.character(df[i,1]))
  }
  count_letters<-c()
  for (i in 1:ncol(df)) {
    count_letters[i]<-sum(top_candid==colnames(df)[i])
  }
  colnames(df)[which(count_letters==max(count_letters))]
}