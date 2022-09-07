
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

source ("code/helper-functions.R")


# making the frequency table ----
letters <- unlist(strsplit(word_list[,1], split = ""))
char_frequencies <- as.data.frame(table(letters))

#normalized
common <- max(char_frequencies[,2])
y=(char_frequencies[,2]/common)

source("code/data-vis.R")

# Bulk of the code is here ----

num_words <- nrow(word_list)
#num_words <- 5
word_scores <- data.frame(word_name = word_list[1:num_words,1],
                    word_length = rep(0, times = num_words),
                    word_guess1 = rep(0, times = num_words),
                    word_guess2 = rep(0, times = num_words),
                    word_guess3 = rep(0, times = num_words),
                    word_guess4 = rep(0, times = num_words),
                    score = rep(0, times = num_words), 
                    score_guess1 = rep(0, times=num_words),
                    score_guess2 = rep(0, times=num_words),
                    score_guess3 = rep(0, times=num_words),
                    score_guess4 = rep(0, times=num_words)
                                                )

word_scores$word_length <-  str_length(word_scores$word_name)




# Calculates the initial scores for all words.-----
# This calculates the score for all words, without worrying about duplicate letts.
ind2 <- 0
for (ind2 in 1:num_words){
  #print(word_scores[[ind2,1]])
  score_ind2 <- Scoring_Word(word_scores[[ind2, "word_name"]],
                             freqs = char_frequencies, verbose = TRUE, debug_detail = TRUE)
  word_scores[[ind2,"score"]] <- score_ind2
}


ind2 <- 0
for (ind2 in 1:num_words){
 # print(word_scores[[ind2,1]])
#score_ind2 <- Scoring_Word_Unique(word_scores[[ind2,1]])
 # print(score_ind2)
#  word_scores[[ind2,3]] <- score_ind2
score_u_ind2 <- Scoring_Word_Unique(word_scores[[ind2,"word_name"]],
                                    freqs = char_frequencies, verbose = TRUE,  debug_detail = TRUE)
word_scores[[ind2,"score_guess1"]] <- score_u_ind2

}





# Finding the best first word
top_words <- word_scores %>%
 arrange(desc(score_guess1))

word_1 <- top_words$word_name[1]




#now we need a function that sees if a word has the letters of the word_1
#and removes them and then calculates the word score
# this is finding GUESS 2
# Word 1= arose -----

ind3 <- 1
for (ind3 in 1:num_words) {
  test<- word_scores[[ind3,"word_name"]]
  word_scores[[ind3, "word_guess2"]]<- Removing_Letters(test, word_1, word_scores[[ind3, "word_length"]] )
  #print(word_scores[[ind3, "word_guess2"]])
  score_ind3 <- Scoring_Word_Unique( word_scores[[ind3, "word_guess2"]],
                                     freqs = char_frequencies$letters, verbose = FALSE)
  word_scores[[ind3,"score_guess2"]] <- score_ind3
  #print (c("output of small list ", top_words[[ind3,4]]))
}

top_words <- word_scores %>%
  arrange(desc(score_guess2))

word_2 <- top_words$word_name[1]

# This is for GUESS 3
ind4 <- 1
for (ind4 in 1:num_words) {
 
  test<- word_scores[[ind4,"word_guess2"]]
  word_scores[[ind4,"word_guess3"]] <- Removing_Letters(test, word_2, word_scores[[ind4, "word_length"]] )
  #print ( word_scores[[ind4,"word_guess3"]])
  score_ind4 <- Scoring_Word_Unique(word_scores[[ind4,"word_guess3"]], freqs = char_frequencies$letters, verbose = FALSE)
  word_scores[[ind4,"score_guess3"]] <- score_ind4
  
}
top_words <- word_scores %>%
  arrange(desc(score_guess3))


word_3 <- top_words$word_name[1]

#this is for GUESS 4
ind4 <- 1
for (ind4 in 1:num_words) {
  test<- word_scores[[ind4,"word_guess3"]]
  word_scores[[ind4,"word_guess4"]] <- Removing_Letters(test, word_3, word_scores[[ind4, "word_length"]] )
 # print ( word_scores[[ind4,"word_guess4"]])
  score_ind4 <- Scoring_Word_Unique(word_scores[[ind4,"word_guess4"]], freqs = char_frequencies$letters, verbose = FALSE)
  word_scores[[ind4,"score_guess4"]] <- score_ind4
  
}

top_words <- word_scores %>%
  arrange(desc(score_guess4))

word_4 <- top_words$word_name[1]

word_scores2 <- word_scores %>%
  select(word_name, score_guess1, score_guess2, score_guess3, score_guess4)


 
word_scores_reshaped <- pivot_longer(word_scores2, cols = 2:5, names_to = "score_type", values_to = "score")
#levels = c("score_guess1", "score_guess2", "score_guess3")
word_scores_reshaped$score_type <- as.factor(word_scores_reshaped$score_type)

ggplot(word_scores_reshaped, aes(score, fill = score_type))+
  geom_density(alpha= 0.5) +
  theme_classic()

#word_scores_reshaped <- factor(score_type, ordered = TRUE)
# working on letter frequencies graph -----

#ggplot(test_letters, aes(x= fct_reorder(test_letters[,1], test_letters[,2])
#                         +                              , y, fill= lett_fect)) + geom_col()

guess <- rep("not guessed", times = 26)

char_frequencies <- cbind(char_frequencies, guess)






letter_vec <-  unlist(strsplit(word_4, split = ""))
print(letter_vec)
for (i in 1:length(letter_vec)) {
  position <- letter_vec[i]== char_frequencies$letters
  char_frequencies$guess[position] <- "Guess 4"
  
}

letter_vec <-  unlist(strsplit(word_3, split = ""))
print(letter_vec)
for (i in 1:length(letter_vec)) {
  position <- letter_vec[i]== char_frequencies$letters
  
  char_frequencies$guess[position] <- "Guess 3"
  
}

letter_vec <-  unlist(strsplit(word_2, split = ""))
print(letter_vec)
for (i in 1:length(letter_vec)) {
  position <- letter_vec[i]== char_frequencies$letters
  char_frequencies$guess[position] <- "Guess_2"
  
}


letter_vec <-  unlist(strsplit(word_1, split = ""))
print(letter_vec)
for (i in 1:length(letter_vec)) {
  position <- letter_vec[i]== char_frequencies$letters
  char_frequencies$guess[position] <- "Guess 1"
  
}

ggplot(char_frequencies, aes(x= fct_reorder(char_frequencies[,1], char_frequencies[,2])
                             , y , fill = char_frequencies[,3])) +
  geom_col()+
  theme_classic()




letter_order <- fct_reorder(char_frequencies[,1], char_frequencies[,2])
letter_order <- fct_rev(letter_order)
ggplot(char_frequencies, aes(x= letter_order
                             , y , fill = guess)) +
  geom_col()+
  ggtitle("When Letters are Guessed") +
  ylab("Normalized Counts") +
  xlab ("Letter") +
  theme_classic()
#})
