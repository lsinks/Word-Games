rm(list=ls())
# Loading libraries and data ----
#load the tidyverse
library("tidyverse")
library("assertive")
library("profvis")

source ("code/helper-functions.R")


word_list <- read.table("input/sgb-words.txt", nrows = 10)


# making the frequency table ----
letters <- unlist(strsplit(word_list[,1], split = ""))
char_frequencies <- as.data.frame(table(letters))
common <- max(char_frequencies[,2])
y=(char_frequencies[,2]/common)


# Bulk of the code is here ----

num_words <- nrow(word_list)
#num_words <- 5
word_scores <- data.frame(word_name = word_list[1:num_words,1],
                    word_length = rep(0, times = num_words),
                    word_guess2 = rep(0, times = num_words),
                    word_guess3 = rep(0, times = num_words),
                    score = rep(0, times = num_words))

word_scores$word_length <-  str_length(word_scores$word_name)

# Scoring_Word_purr <- function(word, freqs = char_frequencies, verbose = FALSE, debug_detail = FALSE){
#   #i'm not handling duplicate letters at all right now
#  # print(class(word))
#  # print(word)
#   letter_vec <-  unlist(strsplit(word, split = ""))
#   value <- 0
#   if (verbose == TRUE)
#   {message("I'm in Scoring_words message and scoring: ", word)}
#   for (i in 1:length(letter_vec)) {
#     position <- letter_vec[i]== freqs
#     value[i] <- y[position]
#     if (debug_detail == TRUE)
#     {
#       print("I am in the scoring loop calculating value: ")
#       print(i)
#       print(sum(value))
# 
#     }
# 
#     if (length(letter_vec)) {
# 
#       return(total <- sum(value))
#     }
# 
#   }
# }
# 
# 
# #
# #  ind2 <- 0
# #  for (ind2 in 1:num_words){
# #    #print(word_scores[[ind2,1]])
# #    score_ind2 <- Scoring_Word_purr(word_scores[[ind2, "word_name"]],
# #                               freqs = char_frequencies$letters, verbose = TRUE, debug_detail = TRUE)
# #   word_scores[[ind2,"score"]] <- score_ind2
# #  }
# #
# 
# 
# 
# 
# temp_list <- data.frame(word1= word_scores$word_name)
# 
# 
# 
# temp2_list<- map_dbl(temp_list$word1, Scoring_Word_purr)
# 
# #word_scores <- word_scores %>% mutate (score = map_dbl(temp_list$word1, Scoring_Word_purr)) this works
# word_scores <- word_scores %>% mutate (score = map_dbl(word_name, Scoring_Word_purr))

word_1<- "arose"
#test<- word_scores[[2,"word_name"]]
#word_scores[[2, "word_guess2"]]<- Removing_Letters(test, word_1)
word_scores <- word_scores %>% mutate (word_guess2 = map_chr(word_name, Removing_Letters, chosen_word = word_1))

word_2 <- "until"
#word_scores <- word_scores %>% mutate (word_guess3 = map2_chr(word_name, word_guess2, Removing_Letters, chosen_word = word_2))
#this worked, I think, even though I had not saved the helper function file with the new parameter word_name for the function.
#It isn't doing the correct thing, but it runs without error.


word_scores <- word_scores %>% mutate (word_guess3 = map_chr(word_guess2, Removing_Letters, chosen_word = word_2))

word_3 <- "dumpy"
word_scores <- word_scores %>% mutate (word_guess4 = map_chr(word_guess3, Removing_Letters, chosen_word = word_3))

 #word_scores_temp <- word_scores %>%
 #mutate( purr_scores = map_dbl(word_name, Scoring_Word_purr, freqs = char_frequencies, verbose = TRUE, debug_detail = TRUE))

