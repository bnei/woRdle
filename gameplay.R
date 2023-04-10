library(tidyverse)

#Read in newline-delimited wordlists and separate them into one letter per column
valid <- read_delim("validwords.txt", delim = "\n", 
                    col_names = FALSE, skip = 0)

valid <- valid %>% 
  rename(full_word = X1) 
  
answers <- read_delim("answers.txt", delim = "\n", 
                    col_names = FALSE, skip = 0)

answers <- answers %>% 
  rename(valid_answers = X1)

#Determines if a guess is in the valid word list
validate_guess <- function(guess) {
  
  guess <- toString(guess)

  if(guess %in% valid$full_word) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

#Takes a 5 letter word and returns a 5 column tibble of its letters
split_word <- function(word) {
  split_word <- tibble(word) %>% 
    separate(word, into = paste0("letter_", 1:5), 
             sep = 1:4, remove = TRUE)
  return(split_word)
}

#Randomly picks a word from the answers list
choose_word <- function() {
  n <- floor(runif(1, min = 1, (max = nrow(answers) + 1)))
  ans <- (answers$valid_answers[[n]])
  return(ans)
}

#Prompts user for input until they input a valid 5 letter word
prompt_input <- function() {
  valid <- FALSE
  while(valid == FALSE){
    print("Enter your guess:")
    guess <- readline()
    valid <- validate_guess(guess)
  }
  return(guess)
}

#Takes two words, answer and guess, and returns a list of boolean greens
is_green <- function(answer, guess) {
  answer <- split_word(answer)
  guess <- split_word(guess)
  
  green <- rep(FALSE, 5)
  
  for(i in 1:5) {
    if(answer[[1, i]] == guess[[1, i]]) {
      green[[i]] <- TRUE
    }
  }
  
  return(green)
}

#Takes two words, answer and guess, and returns a list of boolean yellows
is_yellow <- function(answer, guess) {
  green <- is_green(answer, guess)
  
  answer <- split_word(answer)
  guess <- split_word(guess)
  
  non_green <- bind_rows(answer, guess)
  
  for(i in 1:5) {
    if(green[i] == TRUE) {
      non_green[i] <- NA
    }
  }
  
  yellow <- rep(FALSE, 5)

  for(i in 1:5) {
    
    guess_letter <- non_green[[2,i]]
    
    if(is.na(guess_letter) == FALSE) {

      for(j in 1:5) {
        answer_letter <- non_green[[1, j]]
        
        if(is.na(answer_letter) == FALSE) {
          
          if(guess_letter == answer_letter) {
            yellow[[i]] <- TRUE
            non_green[[1, j]] <- NA
            non_green[[2, i]] <- NA
            break
          }
        }
      }
    }
  }
  return(yellow)
}

#Takes two words, answer and guess, and returns a tibble of colours
guess_colour <- function(answer, guess) {
  green <- is_green(answer, guess)
  yellow <- is_yellow(answer, guess)
  grey <- rep("grey", 5)

  for(i in 1:5) {
    if(green[[i]] == TRUE) {
      green[[i]] <- "green"
    }
    else {
      green[[i]] <- NA
    }
  }

  for(i in 1:5) {
    if(yellow[[i]] == TRUE) {
      yellow[[i]] <- "yellow"
    }
    else {
      yellow[[i]] <- NA
    }
  }
  colours <- coalesce(green, yellow, grey)
  return(colours)
}

play <- function() {
  
  answer <- choose_word()
  
  play_tibble <- split_word(answer) %>% 
    filter(row_number() != 1)
  
  colour_tibble <- play_tibble
  
  #Define some colours for plotting
  gg_colours <- c("green" = "green", "grey" = "grey", "yellow" = "yellow")
  
  for(i in 1:6) {
    current_guess <- prompt_input()
    current_colour <- guess_colour(answer, current_guess)
    
    play_tibble <- play_tibble %>%
      add_row(split_word(current_guess))
    
    colour_tibble[i,] <- t(current_colour)
    
    plot_colours <- colour_tibble %>% 
      pivot_longer(cols = everything()) %>% 
      select(value) %>% 
      rename(colour = value)
    
    plot_tibble <- play_tibble %>% 
      pivot_longer(cols = everything()) %>% 
      mutate(row_index = row_number()) %>% 
      mutate(row = ceiling(row_index / 5)) %>% 
      mutate(col = ((row_index - 1))  %% 5 + 1) %>% 
      mutate(y = 10 - row) %>% 
      mutate(x = col) %>% 
      bind_cols(plot_colours)
    
    print(
    ggplot(plot_tibble) +
      facet_grid(vars(row), vars(col)) +
      scale_fill_manual(values = gg_colours) +
      geom_label(aes(label = value, x = 1, y = 1, fill = colour)) +
      theme_test() + 
      theme(axis.text.x = element_blank(), #remove x axis labels
            axis.ticks.x = element_blank(), #remove x axis ticks
            axis.text.y = element_blank(),  #remove y axis labels
            axis.ticks.y = element_blank(),  #remove y axis ticks
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            strip.background = element_blank(),
            strip.text.x = element_blank(),
            strip.text.y = element_blank()) +
      guides(fill = "none")
    )
    
    if(current_guess == answer) {
      break
    }
  }

}

play()

