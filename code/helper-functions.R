Scoring_Word <- function(word, freqs, verbose = TRUE){
  if (verbose == TRUE)
  {}
  #i'm not handling duplicate letters at all right now
  letter_vec <-  unlist(strsplit(word, split = ""))
  value <- 0
  if (verbose == TRUE)
  {message("I'm in Scoring_words message and scoring: ", word)}
  for (i in 1:length(letter_vec)) {
    
    position <- letter_vec[i]== freqs
    value[i] <- y[position]
    # print(i)
    if (i==5) {
      # print("I am here")
      # print(sum(value))
      return(total <- sum(value))
    }
    
  }
}