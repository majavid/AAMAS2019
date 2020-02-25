influencer_noise<-function(df){
  r<-ceiling(0.1*nrow(df))
  top_candid<-c()
  for (i in 1:nrow(df)) {
    top_candid<-c(top_candid,as.character(df[i,1]))
  }
  count_letters<-c()
  for (i in 1:ncol(df)) {
    count_letters[i]<-sum(top_candid==colnames(df)[i])
  }
  influencer_num<-sort(count_letters,decreasing = TRUE)[2]
  influencer<-sample(colnames(df)[count_letters==influencer_num],1)
  #message("influencer: ",influencer)
  potential<-c()
  for (i in 1:nrow(df)) {
    if(top_candid[i]!=influencer){
      potential<-c(potential,i)
    }
  }
  #message("potential: ",potential)
  target<-sample(potential,r)
  #message("target: ",target)
  for (i in 1:r) {
    df[target[i],]<-c(influencer,setdiff(as.character(df[target[i],]),influencer))
  }
  df
}