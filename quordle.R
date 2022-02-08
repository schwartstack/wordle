library(dplyr)
######################################
MIN_WORD = 4
WIDTH = (MIN_WORD+3)*4+3
N_GUESSES = 9
HEIGHT = N_GUESSES + 2
COLORS = c("grey", "gold1", "forestgreen")

quordle <- function(){
  user_mar = par()$mar
  par(mar = c(1,1,1,1))
  
  points = setup_quordle_points(MIN_WORD)
  dictionary = setup_quordle_dictionary(MIN_WORD)
  keyboard = setup_quordle_keyboard(MIN_WORD)
  word = pick_quordle_words(MIN_WORD)
  active = rep(T,4)
  clear_quordle(width = WIDTH, height = HEIGHT)
  plot_quordle_keyboard(keyboard, active, MIN_WORD)
  
  print(word)
  
  guess_count = 0
  win = F
  while(guess_count < N_GUESSES & !win) {
    guess = readline(prompt="Enter guess: ")
    if(!(nchar(guess) %in% (MIN_WORD:(MIN_WORD+3))[active])){
      print(paste("Invalid word length."))
    }else{
      if(!(toupper(guess) %in% unlist(dictionary))){
        print("Not in dictionary.")
      }else{
        guess_count = guess_count + 1
        
        clue = compare_quordle_guess(guess, word, active)
        points = calculate_quordle_points(points, clue, active)
        keyboard = update_quordle_keyboard(keyboard, clue, guess, active, MIN_WORD)
        plot_quordle_guess(toupper(guess), guess_count, clue, 0, active, MIN_WORD)
        plot_quordle_keyboard(keyboard, active, MIN_WORD)
        
        for(i in 1:4){
          if(active[i]){
            if(all(clue[[i]] == COLORS[3])){
              active[i] = F
              print("Nice!")
              if(all(!active)){
                par(mar = user_mar)
                print("You win!!!")
                win = T
                break
              }
            }
          }
        }
        if(guess_count == N_GUESSES){
          par(mar = user_mar)
          print("Game over.")
          print("The words were:")
          print(word)
        }
      }
    }
  }
  score = calculate_quordle_final_score(points, active, MIN_WORD)
  max_score = calculate_quordle_max_score(MIN_WORD)
  score_percent = round(score/max_score, 2)*100
  print(paste("Your score was ", score, "/", max_score, " (", score_percent, "%)", sep = ""))
}



pick_quordle_words <- function(min_word, url = "https://raw.githubusercontent.com/schwartstack/wordle/main/english-common-words.txt") {
  words = c()
  dictionary = setup_quordle_dictionary(min_word, url)
  for(i in 1:4){
    words[i] = sample(dictionary[[i]], 1)
  }
  return(words)
}
setup_quordle_dictionary <- function(min_word, url = "https://raw.githubusercontent.com/schwartstack/wordle/main/words.txt"){
  words = read.table(url, as.is = T) %>%
    filter(!grepl("'", V1, fixed = T)) %>%
    pull(V1) %>%
    toupper
  dictionary = list()
  for(i in 1:4){
    dictionary[[i]] = words[nchar(words) == i+min_word-1]
  }
  return(dictionary)
}
setup_quordle_keyboard <- function(min_word){
  W = (min_word+3)/10
  H = 1/2
  keyboard = list()
  for(i in 1:4){
    keyboard[[i]] = data.frame(letter = c("Q","W","E","R","T","Y","U","I","O","P",
                                          "A","S","D","F","G","H","J","K","L",
                                          "Z", "X", "C", "V", "B", "N", "M"),
                               row = c(rep(1,10),
                                       rep(2,9),
                                       rep(3,7)),
                               x = c(seq((min_word+4)*(i-1), (min_word+4)*(i-1)+(min_word+3)-W, by = W),
                                     seq((min_word+4)*(i-1)+W/2, (min_word+4)*(i-1)+W/2+(min_word+3)-1.5*W, by = W),
                                     seq((min_word+4)*(i-1)+W, (min_word+4)*(i-1)+7*W, by = W)), 
                               y = c(rep(1, 10),
                                     rep(1/2, 9),
                                     rep(0, 7)),
                               color = "white") %>% mutate_if(is.factor, as.character)
  }
  return(keyboard)
}
setup_quordle_words <- function(dictionary) {
  word = c()
  for(i in 1:4){
    word[i] = sample(dictionary[[i]], 1)
  }
  return(word)
}
setup_quordle_points <- function(min_word) {
  points = list()
  for(i in 1:4){
    points[[i]] = rep(0, i+min_word-1)
  }
  return(points)
}

compare_quordle_guess <- function(guess, answers, active){
  result = list()
  guess_letters = strsplit(guess,"")[[1]] %>% toupper()
  for(i in 1:4){
    if(active[i]){
      answer = answers[i]
      n = nchar(answer)
      result[[i]] = rep(COLORS[1], n)
      answer_letters = strsplit(answer,"")[[1]] %>% toupper()
      
      #make a data frame to keep track of how many of each letter are in the answer
      answer_df = answer_letters %>%
        table %>%
        as.data.frame() %>%
        mutate(letter = as.character(`.`)) %>%
        select_at(-1) %>% 
        rbind(data.frame(Freq = 0, letter = setdiff(LETTERS, answer_letters)))
      
      #go through each letter in guess and assign greens
      for(j in 1:min(length(guess_letters), length(answer_letters))) {
        if(guess_letters[j] == answer_letters[j]) {
          result[[i]][j] = COLORS[3]
          answer_df$Freq[which(answer_df$letter == guess_letters[j])] = answer_df$Freq[which(answer_df$letter == guess_letters[j])] - 1
        }
      }
      
      #go through each letter in guess and assign yellows
      for(j in 1:min(length(guess_letters), length(answer_letters))) {
        if(answer_df$Freq[which(answer_df$letter == guess_letters[j])] > 0 & result[[i]][j] != COLORS[3]) {
          result[[i]][j] = COLORS[2]
          answer_df$Freq[which(answer_df$letter == guess_letters[j])] = answer_df$Freq[which(answer_df$letter == guess_letters[j])] - 1
        }
      }
    }
  }
  return(result)
}
calculate_quordle_points <- function(points, clue, active) {
  temp = points
  for(i in 1:4){
    if(active[i]){
      for(j in 1:length(temp[[i]])){
        if(clue[[i]][j] == COLORS[2]){
          temp[[i]][j] = max(temp[[i]][j], 1)
        }else if(clue[[i]][j] == COLORS[3]){
          temp[[i]][j] = max(temp[[i]][j], 3)
        }
      }
    }
  }
  return(temp)
}
calculate_quordle_final_score <- function(points, active, min_word){
  return(sum(unlist(points)) + sum((!active)*(min_word:(min_word+3))))
}
calculate_quordle_max_score <- function(min_word){
  return(sum(min_word:(min_word+3))*4)
}
plot_quordle_guess <- function(guess, guess_num, clue, sleep, active, min_word) {
  W = 1
  H = 1
  guess_letters = strsplit(guess, "")[[1]]
  adjustment = c(1.5, 1, .5, 0)
  for(i in 1:4){
    if(active[i]){
      xstart = (min_word+4)*(i-1)+adjustment[i]
      xstop = (min_word+4)*(i-1)+(min_word+3)+adjustment[i]
      ystart = HEIGHT-guess_num
      ystop = HEIGHT-guess_num+1
      for(j in 1:(i+min_word-1)){
        rect(xstart+(j-1)*W, ystart, xstart+j*W, ystart+H, col = clue[[i]][j])
        if(j <= (i+min_word-1)){
          text(xstart+(j-1)*W+W/2, ystart+H/2, guess_letters[j], cex = 1)
        }
        Sys.sleep(sleep)
      }
    }
  }
}
update_quordle_keyboard <- function(keyboard, clue, guess, active, min_word){
  temp = keyboard
  guess_letters = strsplit(toupper(guess), "")[[1]]
  for(i in 1:4){
    if(active[i]){
      for(j in 1:min(length(guess_letters), i+min_word-1)){
        if(clue[[i]][j] == COLORS[3]){#clue is green, overwrites anything
          temp[[i]]$color[which(temp[[i]]$letter == guess_letters[j])] = clue[[i]][j]
        }else if(clue[[i]][j] == COLORS[2]){#clue is yellow, overwrites grey and white
          if(temp[[i]]$color[which(temp[[i]]$letter == guess_letters[j])] != COLORS[3]){
            temp[[i]]$color[which(temp[[i]]$letter == guess_letters[j])] = clue[[i]][j]
          }
        }else if(clue[[i]][j] == COLORS[1]){#clue is grey, only overwrites white
          if(temp[[i]]$color[which(temp[[i]]$letter == guess_letters[j])] == "white"){
            temp[[i]]$color[which(temp[[i]]$letter == guess_letters[j])] = clue[[i]][j]
          }
        }
      }
    }
  }
  return(temp)
}
clear_quordle <- function(width, height){
  plot(NULL, xlim = c(0, width), ylim = c(0, height), xaxt = "n", yaxt = "n", bty = "n")
}
plot_quordle_keyboard <- function(keyboard, active, min_word){
  W = (min_word+3)/10
  H = 1/2
  for(i in 1:4){
    if(active[i]){
      for(j in 1:26) {
        rect(keyboard[[i]]$x[j], keyboard[[i]]$y[j], keyboard[[i]]$x[j]+W, keyboard[[i]]$y[j]+H, col = keyboard[[i]]$color[j])
        text(keyboard[[i]]$x[j]+W/2, keyboard[[i]]$y[j]+H/2, keyboard[[i]]$letter[j], cex = .8)
      }
    }
  }
}

quordle()