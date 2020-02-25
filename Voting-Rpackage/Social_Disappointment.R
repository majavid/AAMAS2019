social_disappointment<-function(df,rule=c("plurality","Borda","Hare","Coombs","Condorcet","Copeland","LU","LUR","seq_pairwise")){
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
    return(0)# 0 means "No" and 1 means "Yes"
  }
  sd<-colnames(df)[which(count_letters==max(count_letters))]
    switch(rule,
           "plurality" = ifelse(length(intersect(sd,majority(df)))>0,1,0),
           "Borda"=ifelse(length(intersect(sd,Borda(df)))>0,1,0),
           "Hare"=ifelse(length(intersect(sd,Hare(df)))>0,1,0),
           "Coombs"=ifelse(length(intersect(sd,Coombs(df)))>0,1,0),
           "Condorcet"=ifelse(length(intersect(sd,Condorcet(df)))>0,1,0),
           "Copeland"=ifelse(length(intersect(sd,Copeland(df)))>0,1,0),
           "LU"=ifelse(length(intersect(sd,LU(df)))>0,1,0),
           "LUR"=ifelse(length(intersect(sd,LUR(df)))>0,1,0),
           "seq_pairwise"=ifelse(length(intersect(sd,seq_pairwise(df,agenda)))>0,1,0),
           print("There is no voting rule with this name in this package!")
    )
} 

