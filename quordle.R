######################################
WIDTH = 27
HEIGHT = 10
W = 6/10
H = 1/2
quordle <- function(){
  dictionary = setup_dictionary()
  keyboard = setup_keyboard()
  clear_quordle()
  plot_quordle_keyboard(keyboard)
}

plot_quordle_keyboard<-function(keyboard){
  for(i in 1:4){
    for(j in 1:26) {
      rect(keyboard[[i]]$x[j], keyboard[[i]]$y[j], keyboard[[i]]$x[j]+W, keyboard[[i]]$y[j]+H, col = keyboard[[i]]$color[j])
      text(keyboard[[i]]$x[j]+W/2, keyboard[[i]]$y[j]+H/2, keyboard[[i]]$letter[j], cex = .8)
    }
  }
}

clear_quordle <- function(){
  plot(NULL, xlim = c(0, WIDTH), ylim = c(0, HEIGHT))
  for(i in 1:4){
    rect(7*(i-1), 2, 7*(i-1)+6, HEIGHT)
  }
}

setup_dictionary <- function(url = "https://raw.githubusercontent.com/schwartstack/wordle/main/words2.txt"){
  words = read.table("https://raw.githubusercontent.com/schwartstack/wordle/main/words2.txt", as.is = T) %>%
    filter(!grepl("'", V1, fixed = T)) %>%
    pull(V1) %>%
    toupper
  dictionary = list()
  for(i in 1:4){
    dictionary[[i]] = words[nchar(words) == i+2]
  }
  return(dictionary)
}

setup_keyboard <- function(){
  keyboard = list()
  for(i in 1:4){
    keyboard[[i]] = data.frame(letter = c("Q","W","E","R","T","Y","U","I","O","P",
                                          "A","S","D","F","G","H","J","K","L",
                                          "Z", "X", "C", "V", "B", "N", "M"),
                               row = c(rep(1,10),
                                       rep(2,9),
                                       rep(3,7)),
                               x = c(seq(7*(i-1), 7*(i-1)+6-W, by = W),
                                     seq(7*(i-1)+W/2, 7*(i-1)+W/2+6-1.5*W, by = W),
                                     seq(7*(i-1)+W, 7*(i-1)+7*W, by = W)), 
                               y = c(rep(1, 10),
                                     rep(1/2, 9),
                                     rep(0, 7)),
                               color = "white") %>% mutate_if(is.factor, as.character)
  }
  return(keyboard)
}


quordle()