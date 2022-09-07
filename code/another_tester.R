
# Loading libraries and data ----
#load the tidyverse
library("tidyverse")
library("assertive")
library("profvis")

#profvis({
word_list <- read.table("input/sgb-words.txt")

#Functions ----
#scoring code uses the counting code from
#https://www.r-bloggers.com/2018/12/rrrrs-in-r-letter-frequency-in-r-package-names/



Scoring_Word <- function(word, freqs, verbose = FALSE, debug_detail = FALSE){
  #i'm not handling duplicate letters at all right now
  letter_vec <-  unlist(strsplit(word, split = ""))
  value <- 0
  if (verbose == TRUE)
  {message("I'm in Scoring_words message and scoring: ", word)}
  for (i in 1:length(letter_vec)) {
    position <- letter_vec[i]== freqs
    value[i] <- y[position]
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




# making the frequency table ----
letters <- unlist(strsplit(word_list[,1], split = ""))
char_frequencies <- as.data.frame(table(letters))

#normalized
common <- max(char_frequencies[,2])
y=(char_frequencies[,2]/common)



# Bulk of the code is here ----

num_words <- nrow(word_list)
#num_words <- 5
word_scores <- data.frame(word_name = word_list[1:num_words,1],
                    word_length = rep(0, times = num_words),
                    score = rep(0, times = num_words), 
                    score_purrr = rep(0, times=num_words)
                                                )

word_scores$word_length <-  str_length(word_scores$word_name)




# Calculates the initial scores for all words.-----
# This calculates the score for all words, without worrying about duplicate letts.
ind2 <- 0
for (ind2 in 1:num_words){
  #print(word_scores[[ind2,1]])
  score_ind2 <- Scoring_Word(word_scores[[ind2, "word_name"]],
                             freqs = char_frequencies$letters, verbose = TRUE, debug_detail = TRUE)
  word_scores[[ind2,"score"]] <- score_ind2
}

