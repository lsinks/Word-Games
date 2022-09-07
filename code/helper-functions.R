Construct_Freq_Table <- function(word_list) {
# making the frequency table ----

letters <- unlist(strsplit(word_list[,1], split = ""))
char_frequencies <- as.data.frame(table(letters))

#normalized
common <- max(char_frequencies[,2])
y=(char_frequencies[,2]/common)
char_frequencies$normalized <- y
return(char_frequencies)
}

Scoring_Word <- function(word, freqs = char_frequencies, verbose = FALSE, debug_detail = FALSE){
  letter_vec <-  unlist(strsplit(word, split = ""))
    if (verbose == TRUE)
    {message("I'm in Scoring_words message and scoring: ", word)}
  
  value <- 0
  for (i in 1:length(letter_vec)) {
    position <- letter_vec[i]== freqs$letters
    value[i] <- freqs$normalized[position]
    if (debug_detail == TRUE)
    {
      print("I am in the scoring loop calculating value: ")
      print(i)
      print(sum(value))
      
    }
    
    if (i == length(letter_vec)) {
      
      return(total <- sum(value))
    }
    
  }
  }
  

Scoring_Word_Unique <- function(word, freqs = char_frequencies, verbose = FALSE, debug_detail = FALSE){
  # This does only score on unique letters
  letter_vec <-  unlist(strsplit(word, split = ""))
  unique_letter_vec <- unique(letter_vec)
  #unique_letter_vec <- letter_vec
  if (verbose == TRUE)
  {message("I'm in Scoring_words_Unique and scoring: ", word)}
  
  value <- 0
  if (length(unique_letter_vec)== 0) {
    return(value)
  } else{
    for (i in 1:length(unique_letter_vec)) {
           position <- unique_letter_vec[i] == freqs$letters
          value[i] <- freqs$normalized[position]
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

Removing_Letters <- function (word, chosen_word, verbose = TRUE, debug_detail = TRUE) {
#Removing_Letters <- function (parent_word, word, chosen_word, verbose = TRUE, debug_detail = TRUE) {
  num_lett <- str_length(word)
  #print(original_word)
  #print(num_lett)
  if(num_lett == 0) {return("")} else {if (verbose == TRUE) 
  {
    message("I'm in Removing_Letters working on ", chosen_word, "and ", word)
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
    return(test)}
  
}