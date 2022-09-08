#char_frequencies

# Creating some visualizations of the frequency table ----
# 
# #raw counts
# ggplot(char_frequencies, aes(x= fct_reorder(char_frequencies[,1], char_frequencies[,2])
#                              , char_frequencies[,2] )) +
#  geom_col() +
#  theme_classic()
# 



#plotting the frequency of the letters in our word_set
ggplot(char_frequencies, aes(x =fct_rev(fct_reorder(letters,  normalized)), y= normalized )) +
  geom_col() +
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

