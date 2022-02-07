library(dplyr)
###################
########constants
HEIGHT = 10
WIDTH = 5
N = 5
COLORS = c("grey", "gold1", "forestgreen")
################
##########get colors based on guess
compare_guess<-function(guess, answer){
  n = nchar(answer)
  result = rep(COLORS[1], n)
  
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
  for(i in 1:n) {
    if(guess_letters[i] == answer_letters[i]) {
      result[i] = COLORS[3]
      answer_df$Freq[which(answer_df$letter == guess_letters[i])] = answer_df$Freq[which(answer_df$letter == guess_letters[i])] - 1
    }
  }
  
  #go through each letter in guess and assign yellows
  for(i in 1:n) {
    if(answer_df$Freq[which(answer_df$letter == guess_letters[i])] > 0 & result[i] != COLORS[3]) {
      result[i] = COLORS[2]
      answer_df$Freq[which(answer_df$letter == guess_letters[i])] = answer_df$Freq[which(answer_df$letter == guess_letters[i])] - 1
    }
  }
  
  return(result)
}
#################
#########main program
wordle <- function(word = NULL, n = ifelse(is.null(word), N, nchar(word)), sleep = .3, sound = T, width = WIDTH, height = HEIGHT) {
  if(class(word) == "numeric"){
    n = word
    word = NULL
  }
  
  words = read.table("https://raw.githubusercontent.com/schwartstack/wordle/main/words2.txt", as.is = T) %>%
    filter(nchar(V1) == n) %>%
    filter(!grepl("'", V1, fixed = T)) %>%
    pull(V1) %>%
    toupper
  
  if(is.null(word)) {
    answer = sample(words, 1)
  }else{
    answer = word
    n = nchar(word)
  }
  
  w = width/10
  h = height/16
  KEYBOARD = data.frame(letter = c("Q","W","E","R","T","Y","U","I","O","P",
                                   "A","S","D","F","G","H","J","K","L",
                                   "Z", "X", "C", "V", "B", "N", "M"),
                        row = c(rep(1,10),
                                rep(2,9),
                                rep(3,7)),
                        x = c(seq(0,width-w,by=w),
                              seq(w/2,width-1.5*w,by=w),
                              seq(w,width-2.5*w,by=w)), 
                        y = c(rep(1/8*height, 10),
                              rep(1/16*height, 9),
                              rep(0, 7)),
                        color = "white") %>% mutate_if(is.factor, as.character)
  user_mar = par()$mar
  par(mar = c(1,1,1,1))
  clear(width, height)
  draw_keyboard(KEYBOARD, width, height)
  
  guess_count = 0
  while(guess_count < 6) {
    guess = readline(prompt="Enter guess: ")
    if(nchar(guess) != n){
      print(paste("Not a", n, "letter word"))
    }else{
      if(!(toupper(guess) %in% words)){
        print("Not in dictionary")
      }else{
        guess_count = guess_count + 1
        clue = compare_guess(guess, answer)
        KEYBOARD = update_keyboard(KEYBOARD, clue, guess)
        plot_guess(toupper(guess), guess_count, clue, sleep, sound, width, height)
        draw_keyboard(KEYBOARD, width, height)
        if(all(clue == COLORS[3])){
          beep(5)
          par(mar = user_mar)
          print("You win!")
          break
        }
        if(guess_count == 6){
          beep(9)
          par(mar = user_mar)
          print("You lose")
          print(paste("The word was", answer))
        }
      }
    }
  }
}
#################
########plot each guess
plot_guess<-function(guess, guess_num, clue, sleep, sound, width = WIDTH, height = HEIGHT) {
  require(beepr)
  w = width/nchar(guess)
  h = height/8
  xseq = seq(0,width,by=w)
  guess_letters = strsplit(guess, "")[[1]]
  for(i in 1:length(guess_letters)){
    rect(xseq[i], height-guess_num*h, xseq[i+1], height-(guess_num-1)*h, col = clue[i])
    text(xseq[i]+w/2, height-guess_num*h+h/2, guess_letters[i], cex = 2)
    Sys.sleep(sleep)
    if(sound){
      if(clue[i] == COLORS[3]){
        beep(2)
      }else if(clue[i] == COLORS[1]){
        beep(10)
      }else{
        beep(1)
      }
    }
  }
}
#################
########plot the keyboard
draw_keyboard_letter <- function(letter, keyboard, width, height) {
  w = width/10
  h = height/16
  rownum = which(keyboard$letter == toupper(letter))
  rect(keyboard$x[rownum], 
       keyboard$y[rownum], 
       keyboard$x[rownum]+w, 
       keyboard$y[rownum]+h, 
       col = keyboard$color[rownum])
  text(keyboard$x[rownum]+w/2, keyboard$y[rownum]+h/2, keyboard$letter[rownum], cex = 1)
}
draw_keyboard <- function(keyboard, width, height){
  for(i in 1:nrow(keyboard)){
    draw_keyboard_letter(keyboard$letter[i], keyboard, width, height)
  }
}
update_keyboard <- function(keyboard, clue, guess) {
  temp = keyboard
  guess_letters = strsplit(toupper(guess), "")[[1]]
  for(i in 1:length(guess_letters)){
    if(temp$color[which(temp$letter == guess_letters[i])] != COLORS[3]){
      temp$color[which(temp$letter == guess_letters[i])] = clue[i]
    }
  }
  return(temp)
}
################
######clear the screen
clear<-function(width, height) {
  plot(NULL, NULL, xlim = c(0, width), ylim = c(0, height),
       xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n")
}

###########
###########
##########
wordle(3)
