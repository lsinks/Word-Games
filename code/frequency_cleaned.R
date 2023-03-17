library(tidyverse)

sgb.words <- 
  read.delim("C:/Users/drsin/OneDrive/Documents/R Projects/wordle/sgb-words.txt",
             sep = "")

#probably want this instead because it assumes no headers
#test6 <- read.table(file.choose())

Scoring_Word <- function(word){
  #i'm not handling duplicate letters at all right now
  letter_vec <-  unlist(strsplit(word, split = ""))
  value <- 0
  for (i in 1:5) {
    position <- letter_vec[i] == char_frequencies$letters
    value[i] <- y[position]
   # print(i)
    if (i == 5) {
     # print("I am here")
     # print(sum(value))
      return(total <- sum(value))
      }
    
  }
} 


Scoring_Word_Unique <- function(word){

 # print(word)
  letter_vec <-  unlist(strsplit(word, split = ""))
  unique_letter_vec <- unique(letter_vec)
  #print(unique_letter_vec)
  #print(length(unique_letter_vec))
  
  value <- 0
  if (length(unique_letter_vec) == 0) {
    return(value)
  } else{
      for (i in 1:length(unique_letter_vec)) {
      position <- unique_letter_vec[i] == char_frequencies$letters
      value[i] <- y[position]
    # print(i)
    # print(value)
    if (i == length(unique_letter_vec)) {
      # print("I am here")
      # print(sum(value))
      return(total <- sum(value))
    }
    
  }
  }
}



start_time <- Sys.time()

letters <- unlist(strsplit(sgb.words[,1], split = ""))
char_frequencies <- as.data.frame(table(letters))
#char_frequencies

ggplot(char_frequencies, 
        aes(x = fct_reorder(char_frequencies[,1], char_frequencies[,2])
                              , char_frequencies[,2] )) +
   geom_col() +
   theme_classic()

common <- max(char_frequencies[,2])
y = (char_frequencies[,2]/common)

ggplot(char_frequencies, 
       aes(x =  fct_reorder(char_frequencies[,1], char_frequencies[,2]), y )) +
  geom_col()+
  theme_classic()

#calculate the score for crone
crone_score <- Scoring_Word("crone")
#might_score <- Scoring_Word ("might")
#sadly_score <- Scoring_Word ("sadly")
num_words <- 5756
#num_words <- 5
small_list <- cbind(word_name = sgb.words[1:num_words,1], 
                    score =rep(0, times = num_words), 
                    unique_score = rep(0, times=num_words),
                    post_word_one_unique = rep(0, times=num_words),
                    post_word_two_unique = rep(0, times=num_words),
                    post_word_three_unique = rep(0, times=num_words)
                                                )
word <- small_list[[1,1]]
Scoring_Word(word)
ind2 <- 0
for (ind2 in 1:num_words){
  #print(small_list[[ind2,1]])
  score_ind2 <- Scoring_Word(small_list[[ind2,1]])
  small_list[[ind2,2]] <- score_ind2
}



#u_crone_score <- Scoring_Word_Unique("crone")
#u_there_core <- Scoring_Word_Unique ("there")
#sadly_score <- Scoring_Word ("sadly")


ind2 <- 0
for (ind2 in 1:num_words){
 # print(small_list[[ind2,1]])
  score_ind2 <- Scoring_Word_Unique(small_list[[ind2,1]])
 # print(score_ind2)
  small_list[[ind2,3]] <- score_ind2
}



small_list1 <- small_list
small_df<- as.data.frame(small_list1)
top_words <- small_df %>%
 arrange(desc(unique_score))

word_1 <- top_words$word_name[1]


#now we need a function that sees if a word has the letters of the word_1
#and removes them and then calculates the word score
#top word is arose
# Word 1= arose -----
ind3 <- 1
for (ind3 in 1:num_words) {
 # print(top_words$word_name[ind3])
  test<- small_list[[ind3,1]]
  lvec <- gsub("[a r o s e]", "", test)  #this actually works.  How do I use the string?
  #lvec <-  unlist(strsplit(word_1, split = ""))
  #lvec<- "t|h|e|i|r" #how do I contruct this automatically

  #new_let <- str_remove_all(pattern= lvec, string= test)
 # print(lvec)
  score_ind3 <- Scoring_Word_Unique(lvec)
 # print("writing score")
 # print(c(ind3, " ", score_ind3, "for the word ", test, "sent as ", lvec))
  
  small_list[[ind3,4]] <- score_ind3
  #print (c("output of small list ", top_words[[ind3,4]]))
}

small_df2 <- as.data.frame(small_list)
top_words2 <- small_df2 %>%
  arrange(desc(post_word_one_unique))


word_2 <- top_words2$word_name[1]

# top word 2 is until

ind4 <- 1
for (ind4 in 1:num_words) {
  # print(top_words$word_name[ind3])
  test<- small_list[[ind4,1]]
  lvec <- gsub("[u n t i l a r o s e]", "", test)  #this actually works.  How do I use the string?
  #lvec <-  unlist(strsplit(word_1, split = ""))
  #lvec<- "t|h|e|i|r" #how do I contruct this automatically
  
  #new_let <- str_remove_all(pattern= lvec, string= test)
  # print(lvec)
  score_ind4 <- Scoring_Word_Unique(lvec)
  # print("writing score")
  # print(c(ind3, " ", score_ind3, "for the word ", test, "sent as ", lvec))
  
  end_time <- Sys.time()
  end_time - start_time
  
  small_list[[ind4,5]] <- score_ind4
  #print (c("output of small list ", top_words[[ind3,4]]))
}

small_df3<- as.data.frame(small_list)
top_words2 <- small_df3 %>%
  arrange(desc(post_word_two_unique))


word_3 <- top_words2$word_name[1]


a= Scoring_Word_Unique("arose")+ Scoring_Word_Unique("until") +Scoring_Word_Unique("dumpy")
b= Scoring_Word_Unique("crone")+ Scoring_Word_Unique("mighty") +Scoring_Word_Unique("sadly")
 