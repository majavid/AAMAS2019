seq_pairwise<-function(df,lst){
  # The procedure known as sequential pairwise voting with a fixed
  # agenda runs as follows. We have a fixed ordering of the alternatives
  # a, b, c, . . . called the agenda. The first alternative in the ordering is 
  # pitted against the second in a one-on-one contest. The winning alternative
  # (or both, if there is a tie) is then pitted against the third alternative in
  # the list in a one-on-one contest. An alternative is deleted at the end
  # of any round in which it loses a one-on-one contest. The process is
  # continued along the agenda until the "survivors" have finally met the
  # last alternative in the agenda. Those remaining at the end are declared
  # to be the social choices.
  win_list<-lst[1]
  z<-2
  M<-matrix( rep( 0, len=(length(lst))^2), nrow =length(lst))
  #message(class(win_list)," ",class(M))
  count<-c()
  for (i in 1:(ncol(df)-1)) {
    for (j in (i+1):ncol(df)) {
      count[i]<-0
      count[j]<-0
      for (k in 1:nrow(df)) {
        if(((which(df[k,]==lst[i]))<(which(df[k,]==lst[j])))){
          count[i]<-count[i]+1
        }else{
          count[j]<-count[j]+1
        }
      }
      if(count[i]>count[j]){
        M[i,j]<-1
        M[j,i]<-0
      }else if(count[i]<count[j]){
        M[i,j]<-0
        M[j,i]<-1
      }else{
        M[i,j]<-1
        M[j,i]<-1
      }
    }
  }
  while(z<=length(lst)){
    temp<-win_list
    #message(z,"->",temp)
    counter<-0
    for (j in 1:length(win_list)) {
      r<-which(lst==win_list[j])
      s<-which(lst==lst[z])
      if(M[r,s]==1 && M[s,r]==0){
        counter<-counter
      }else if(M[r,s]==0 && M[s,r]==1){
        temp<-setdiff(temp,win_list[j])
        counter<-counter+1
      }else if(M[r,s]==1 && M[s,r]==1){
        counter<-counter+1
      }else{
        counter<-counter
      }
    }
    if(counter==length(win_list)){
      temp<-c(temp,lst[z])
    }
    win_list<-temp
    z<-z+1
  }
  win_list
}