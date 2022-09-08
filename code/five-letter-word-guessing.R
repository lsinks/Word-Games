
# Loading libraries and data ----
#load the tidyverse
library("tidyverse")
library("assertive")
library("profvis")


word_list <- read.table("input/sgb-words.txt") #is this a factor??ge

#Functions ----
#scoring code uses the counting code from
#https://www.r-bloggers.com/2018/12/rrrrs-in-r-letter-frequency-in-r-package-names/

source ("code/helper-functions.R")



char_frequencies <- Construct_Freq_Table(word_list)


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

word_scores <- word_scores %>% mutate(score = map_dbl(word_name, Scoring_Word))
word_scores <- word_scores %>% mutate(score_guess1 = map_dbl(word_name, Scoring_Word_Unique))


# Finding the best first word

top_words <- word_scores %>%
 arrange(desc(score_guess1))
word_1 <- top_words$word_name[1]


word_scores <- word_scores %>% mutate (word_guess2 = map_chr(word_name, Removing_Letters, chosen_word = word_1))
word_scores <- word_scores %>% mutate (score_guess2 = map_dbl(word_guess2, Scoring_Word_Unique))

top_words <- word_scores %>%
  arrange(desc(score_guess2))

word_2 <- top_words$word_name[1]


word_scores <- word_scores %>% mutate (word_guess3 = map_chr(word_guess2, Removing_Letters, chosen_word = word_2))
word_scores <- word_scores %>% mutate (score_guess3 = map_dbl(word_guess3, Scoring_Word_Unique))


top_words <- word_scores %>%
  arrange(desc(score_guess3))
word_3 <- top_words$word_name[1]

word_scores <- word_scores %>% mutate(word_guess4 = map_chr(word_guess3, Removing_Letters, chosen_word = word_3))
word_scores <- word_scores %>% mutate(score_guess4 = map_dbl(word_guess4, Scoring_Word_Unique))


top_words <- word_scores %>%
  arrange(desc(score_guess4))

word_4 <- top_words$word_name[1]

word_scores2 <- word_scores %>%
   select(word_name, score_guess1, score_guess2, score_guess3, score_guess4)

### This is now just visualizing what we've done.

#source("code/Data-Vis.R")  #this doesn't work.  

# I just copied them here for now



#plotting the frequency of the letters in our word_set
ggplot(char_frequencies, aes(x =fct_rev(fct_reorder(letters,  normalized)), y= normalized )) +
  geom_col() +
#ggplot(char_frequencies, aes(x = fct_rev(fct_reorder(letters,  normalized)), y=stat(prop)  )) +
 # geom_bar() +

  theme_classic() +
  theme(legend.position = "none") +
  labs(title = "Frequencies of Letters", caption = "from 5 letter words") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Letter") +
  ylab("Frequency") +
  
  scale_y_continuous( expand = c(0, 0))

## This looks at the distribution of scores as guessing occurs.  Initially, you have a

word_scores_reshaped <- pivot_longer(word_scores2, cols = 2:5, names_to = "score_type", values_to = "score")

word_scores_reshaped$score_type <- as.factor(word_scores_reshaped$score_type)

ggplot(word_scores_reshaped, aes(score, fill = score_type))+
  geom_density(alpha= 0.5) +
  theme_classic()+
  labs(title = "Evolution of Word Scores as Guessing Progresses", caption = "for 5 letter words") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Score") +
  ylab("Density") +
  labs(fill = "") +
  theme(legend.position = c(0.7, 0.8)) +
  scale_x_continuous( expand = c(0, 0)) +
  scale_y_continuous( expand = c(0, 0)) 



## Now we are visualizing what letters are picked in each guess

guess <- rep("not guessed", times = 26)
char_frequencies <- cbind(char_frequencies, guess)
# this is done in reverse order because some letters are guessed in more than
# one word and I'd like them marked at the earliest guess.
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
  char_frequencies$guess[position] <- "Guess 2"

}


letter_vec <-  unlist(strsplit(word_1, split = ""))
print(letter_vec)
for (i in 1:length(letter_vec)) {
  position <- letter_vec[i]== char_frequencies$letters
  char_frequencies$guess[position] <- "Guess 1"

}




ggplot(char_frequencies, aes(
  x = fct_rev(fct_reorder(letters,  normalized)),
  y = normalized,
  fill = guess
)) +
  geom_col() +
  ggtitle("When Letters are Guessed") +
  ylab("Normalized Counts") +
  xlab ("Letter") +
  theme_classic() +
  theme(legend.position = c(0.7, 0.8)) +
  scale_y_continuous(expand = c(0, 0))


