#char_frequencies

# Creating some visualizations of the frequency table ----

#raw counts
# ggplot(char_frequencies, aes(x= fct_reorder(char_frequencies[,1], char_frequencies[,2])
#                              , char_frequencies[,2] )) +
#   geom_col()+
#   theme_classic()
