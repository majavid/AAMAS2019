Borda<-function(df){
  # First popularized by Jean-Charles de Borda in 1781, the social
  # choice procedure known as the Borda count takes advantage of the
  # information regarding individual intensity of preference provided by
  # looking at how high up in the preference list of an individual a given
  # alternative occurs. More precisely, one uses each preference list to
  # award "points" to each of n alternatives as follows: the alternative at
  # the bottom of the list gets zero points, the alternative at the next to
  # the bottom spot gets one point, the next one up gets two points and
  # so on up to the top alternative on the list which gets n - 1 points. For
  # each alternative, we simply add the points awarded it from each of the
  # individual preference lists. The alternative(s) with the highest "Borda
  # score" is declared to be the social choice.
  count_letters<-c()
  for (i in 1:ncol(df)) {
    count_letters[i]<-0
  }
  for (i in 1:ncol(df)) {
    for (j in 1:nrow(df)) {
      count_letters[i]<-count_letters[i]+(ncol(df)-which(df[j,]==colnames(df)[i]))
    }
  }
  #message(count_letters)
  letters[which(count_letters==max(count_letters))]
}