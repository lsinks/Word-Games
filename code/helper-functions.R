Construct_Freq_Table <- function(word_list) {
# making the frequency table ----

letters <- unlist(strsplit(word_list[,1], split = ""))
char_frequencies <- as.data.frame(table(letters))

#normalized
common <- max(char_frequencies[,2])
y=(char_frequencies[,2]/common)
char_frequencies$normalized <- y
}




Scoring_Word <- function(word, freqs = char_frequencies, verbose = FALSE, debug_detail = FALSE){
  #i'm not handling duplicate letters at all right now
  letter_vec <-  unlist(strsplit(word, split = ""))
  value <- 0
  if (verbose == TRUE)
  {message("I'm in Scoring_words message and scoring: ", word)}
  for (i in 1:length(letter_vec)) {
    #position <- letter_vec[i]== freqs
    #value[i] <- y[position]
    position <- letter_vec[i]== freqs$letters
    value[i] <- freqs$Freq[position]
    if (debug_detail == TRUE)
    {
      print("I am in the scoring loop calculating value: ")
      print(i)
      print(sum(value))
      
    }
    
    if (length(letter_vec)) {
      
      return(total <- sum(value))
    }
    
  }
  }
  

Scoring_Word_Unique <- function(word, freqs = char_frequencies, verbose = FALSE, debug_detail = FALSE){
  # This does only score on unique letters
  letter_vec <-  unlist(strsplit(word, split = ""))
  unique_letter_vec <- unique(letter_vec)
  if (verbose == TRUE)
  {message("I'm in Scoring_words_Unique and scoring: ", word)}
  
  value <- 0
  if (length(unique_letter_vec)== 0) {
    return(value)
  } else{
    for (i in 1:length(unique_letter_vec)) {
      #position <- unique_letter_vec[i] == char_frequencies$letters
      position <- unique_letter_vec[i] == freqs$letters
      #value[i] <- y[position]
      value[i] <- freqs$Freq[position]
      if (debug_detail == TRUE)
      {
        print("I am in the unique scoring loop calculating value: ")
        print(i)
        print(sum(value))
      }
      
      if (i==length(unique_letter_vec)) {
        
        return(total <- sum(value))
      }
      
    }
  }
}


Removing_Letters <- function (word, chosen_word, num_lett, verbose = FALSE, debug_detail = FALSE) {
  if (verbose == TRUE) 
  {
    message("I'm in Removing_Letters working on ", chosen_word)
  }
  ind <- 1
  
  char_vec <- unlist(strsplit(chosen_word, ""))
  test <- word
  for (ind in 1:num_lett) {
    test <- str_replace_all(test, char_vec[ind], "")
    if (debug_detail == TRUE)
    {print(char_vec[ind])
      print(test)}
    
  }
  return(test)
}