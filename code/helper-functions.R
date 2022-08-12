Scoring_Word <- function(word){
  #i'm not handling duplicate letters at all right now
  letter_vec <-  unlist(strsplit(word, split = ""))
  value <- 0
  message("I'm in Scoring_words")
  for (i in 1:length(letter_vec)) {
    
    position <- letter_vec[i]== char_frequencies$letters
    value[i] <- y[position]
    # print(i)
    if (i==5) {
      # print("I am here")
      # print(sum(value))
      return(total <- sum(value))
    }
    
  }
}