Construct_Freq_Table <- function(word_list) {

#scoring code uses the counting code from

#https://www.r-bloggers.com/2018/12/rrrrs-in-r-letter-frequency-in-r-package-names/  
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
  lvec <- gsub(paste0("[", chosen_word, "]"), "", word)  
  return(lvec)}
 