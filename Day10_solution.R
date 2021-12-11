library(tidyverse)

day10_data <- read_delim("Day10_data.txt", delim = "\n", col_names = FALSE)

data_strings <- day10_data[[1]]

# function to look for an error in the bracket sequence
# if it is an opening bracket, read into a list
# if it is a closing bracket, check most recent entry in opening list. They should match
# If they match then remove that most recent entry from opening list
# If they do not match, this is an error
find_error_point <- function(code_line){
  open_brackets <- c("[", "{", "(", "<")
  openings <- c()
  for(i in 1:code_length){
    this_char <- substring(code_line, i, i)
    if(this_char %in% open_brackets){
      openings <- append(openings, this_char)
    }else if(this_char == "]"){
      last_open <- tail(openings, 1)
      if(!last_open == "["){
        return(this_char)
      }else{
        if(length(openings)==1){
          openings <- c()
        }else{
          openings <- openings[1:(length(openings)-1)]
        }
      }
    }else if(this_char == "}"){
      last_open <- tail(openings, 1)
      if(!last_open == "{"){
        return(this_char)
      }else{
        if(length(openings)==1){
          openings <- c()
        }else{
          openings <- openings[1:(length(openings)-1)]
        }
      }
    }else if(this_char == ")"){
      last_open <- tail(openings, 1)
      if(!last_open == "("){
        return(this_char)
      }else{
        if(length(openings)==1){
          openings <- c()
        }else{
          openings <- openings[1:(length(openings)-1)]
        }
      }
    }else if(this_char == ">"){
      last_open <- tail(openings, 1)
      if(!last_open == "<"){
        return(this_char)
      }else{
        if(length(openings)==1){
          openings <- c()
        }else{
          openings <- openings[1:(length(openings)-1)]
        }
      }
    }else{
      next
    }
  }
  return(openings)
}

find_close <- function(open_list){
  close_str <- c()
  for(i in 1:length(open_list)){
    last_open <- tail(open_list, 1)
    if(last_open == "["){
      next_close <- "]"
    }else if(last_open == "{"){
      next_close <- "}"
    }else if(last_open == "("){
      next_close <- ")"
    }else if(last_open == "<"){
      next_close <- ">"
    }else{
      break
    }
    close_str <- append(close_str, next_close)
    #print(close_str)
    open_list <- open_list[1:(length(open_list)-1)]
  }
  return(close_str)
}

# function to convert the closing string for a line into a value as governed by the question
find_close_value <- function(close_list){
  total_score <- 0
  # create a named list of the values for each possible closing bracket type
  vals <- c(1,2,3,4)
  names(vals) <- c(")", "]", "}", ">")
  # for each closing bracket in the list
  for(i in 1:length(close_list)){
    this_char <- close_list[i]
    total_score <- total_score*5
    this_val <- vals[this_char][[1]] # get the correct value from our named list
    print(total_score)
    total_score <- total_score + this_val
    
  }
  return(total_score)
}

# need a new named list look up table with values for part 2
val_lookup <- c(3,57,1197,25137)
closing <- c(")", "]", "}", ">")
names(val_lookup) <- closing

total_err <- 0
total_errs <- c()
closers <- c()
close_strings <- c()

# loop through all the lines
# for each line find the error point 
# if there is no error point, the function returns the outstanding open brackets
# so in those cases, use the function to find the required closing brackets
# then use our final function to convert the closing bracket list to a value
for(i in  1:length(data_strings)){
  this_err <- find_error_point(data_strings[i])
  if(this_err %in% closing){
    this_val <- val_lookup[this_err][[1]]
    total_err <- total_err + this_val
    total_errs <- append(total_errs,this_val)
  }else{
    this_close <- find_close(this_err)
    close_value <- find_close_value(this_close)
    #print(paste0(this_close, " = ", close_value))
    close_strings <- append(close_strings, str_c(this_close, collapse = ''))
    closers <- append(closers,close_value)
  }
}

answer_part1 <- total_err

closers <- sort(closers)
answer_part2 <- closers[(length(closers)+1)/2] 
# part 2 answer was correct for the practice sample but incorrect for 
closers[29] # was correct somehow - this is one after the middle value (or before, not sure how it should be sorted)
# need to come back to check what is going on. have I missed some edge case?