library(dplyr)
##################
######words list
words = read.table("words2.txt", colClasses = "character")
words = words %>% 
  filter(nchar(V1) == 5,
         !grepl("'",V1,fixed=T)) %>% 
  .$V1 %>%
  toupper
################
##########get colors based on guess
compare_guess<-function(guess, answer){
  result = rep("grey", 5)
  
  #vectorize both words
  guess_letters = strsplit(guess,"")[[1]] %>% toupper()
  answer_letters = strsplit(answer,"")[[1]] %>% toupper()
  
  #make a data frame to keep track of how many of each letter are in the answer
  answer_df = answer_letters %>%
    table %>%
    as.data.frame() %>%
    mutate(letter = as.character(`.`)) %>%
    select_at(-1) %>% 
    rbind(data.frame(Freq = 0, letter = setdiff(LETTERS, answer_letters)))
  
  #go through each letter in guess and assign greens
  for(i in 1:5) {
    if(guess_letters[i] == answer_letters[i]) {
      result[i] = "forestgreen"
      answer_df$Freq[which(answer_df$letter == guess_letters[i])] = answer_df$Freq[which(answer_df$letter == guess_letters[i])] - 1
    }
  }
  
  #go through each letter in guess and assign yellows
  for(i in 1:5) {
    if(answer_df$Freq[which(answer_df$letter == guess_letters[i])] > 0) {
      result[i] = "gold1"
      answer_df$Freq[which(answer_df$letter == guess_letters[i])] = answer_df$Freq[which(answer_df$letter == guess_letters[i])] - 1
    }
  }
  
  return(result)
}
#################
#########main program
wordle <- function(word = NULL) {
  plot(NULL, NULL, xlim = c(0,5), ylim = c(0,6),
       xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  if(is.null(word)) {
    answer = sample(words, 1)
  }else{
    answer = word
  }
  
  guess_count = 0
  while(guess_count < 6) {
    guess = readline(prompt="Enter guess: ")
    if(nchar(guess) != 5){
      print("Not a 5 letter word")
    }else{
      if(!(toupper(guess) %in% words)){
        print("Not in dictionary")
      }else{
        guess_count = guess_count + 1
        clue = compare_guess(guess, answer)
        plot_guess(toupper(guess), guess_count, clue)
        if(all(clue == "forestgreen")){
          print("You win!")
          break
        }
        if(guess_count == 6){
          print("You lose")
          print(paste("The word was", answer))
        }
      }
    }
  }
}
#################
########plot each guess
plot_guess<-function(guess, guess_num, clue, sleep = .4) {
  guess_letters = strsplit(guess, "")[[1]]
  for(i in 1:5){
    rect(i-1, 6-guess_num, i, 7-guess_num, col = clue[i])
    text(i-.5, 6.5-guess_num, guess_letters[i], cex = 2)
    Sys.sleep(sleep)
  }
}
################
############test
wordle()