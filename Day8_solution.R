library(tidyverse)
library(stringr)

day8_data <- read_delim("Day8_data.txt", delim = "|", col_names = FALSE)

# function to count the occurences of digits with 2,3,4,7 characters (unique numbers)
count_1478 <- function(string){
  
  count <- 0
  digits <- str_split(trimws(string), " ")[[1]]
  
  for(digit in digits){
    
    digit_length <- nchar(digit)
    
    if(digit_length %in% c(2,3,4,7)){
      
      count <- count + 1
      
    }
    
  }
  
  return(count)
  
}

# can now just sum up the count for every entry to find the total count of these digits
answer_part1 <- sum(sapply(day8_data$X2, count_1478))


# in part 2, we need to actually assign all of the digits - will need some logic for this

# function to arrange characters in string in alphabetical order, since can appear in different order
# shamelessly taken from https://gist.github.com/martinctc/56b3fb701a182f5b8dffceecd65b6d86
str_arrange <- function(x){
  x %>%
    stringr::str_split("") %>% # Split string into letters
    purrr::map(~sort(.) %>% paste(collapse = "")) %>% # Sort and re-combine
    as_vector() # Convert list into vector
}
# want another function that will check if every letter of one string is contained in another
# cannot simply use str_detect as abd will not be found in abcd
str_check <- function(y, x){
  letters <- str_split(x, "")[[1]]
  contains_all <- TRUE
  for(letter in letters){
    contains_letter <- str_detect(y, letter)
    contains_all <- contains_all * contains_letter
  }
  if(contains_all == 1){
    return(TRUE)
  }else{return(FALSE)}
  
}

# need a function to assign each string/digit combination from the first part of each row in the data
# think this needs to be an iterative process as we gather more information so this function will be quite long and inefficient
assign_digits <- function(digit_list){
  
  digit_list <- str_split(trimws(digit_list), " ")[[1]]
  char_strs <- c()
  digits <- c()
  
  for(digit in digit_list){
    
    digit <- str_arrange(digit)
    chars <- nchar(digit)
    
    if(chars == 2){
      char_strs <- append(char_strs, digit)
      digits <- append(digits, "1")
    }else if(chars == 3){
      char_strs <- append(char_strs, digit)
      digits <- append(digits, "7")
    }else if(chars == 4){
      char_strs <- append(char_strs, digit)
      digits <- append(digits, "4")
    }else if(chars == 7){
      char_strs <- append(char_strs, digit)
      digits <- append(digits, "8")
    }else{ next }
    
  }
  
  # make a named list where the names are the numeric values and the entries are the corresponding strings
  char_strs_now <- char_strs
  names(char_strs_now) <- digits
  
  # now we can assign 3 and 6 using the dictionary so far
  for(digit in digit_list){
    
    digit <- str_arrange(digit)
    chars <- nchar(digit)
    
    if(chars == 5 & str_check(digit, char_strs_now["1"])){
      # '3' is 5 characters and contains all of '1'
      char_strs <- append(char_strs, digit)
      digits <- append(digits, "3")
    }else if(chars == 6 & !str_check(digit, char_strs_now["1"])){
      # '6' is 6 characters and does not contain all of 1
      char_strs <- append(char_strs, digit)
      digits <- append(digits, "6")
    }else{ next }
    
  }
  
  # update the named list
  char_strs_now <- char_strs
  names(char_strs_now) <- digits
  
  # now we can assign 9 and 0
  for(digit in digit_list){
    
    digit <- str_arrange(digit)
    chars <- nchar(digit)
    
    if(chars == 6){
      if(str_check(digit, char_strs_now["6"])){ # we already assigned '6'
        next
      }else if(str_check(digit, char_strs_now["3"])){
        # '9' is 6 characters and contains all of '3'
        char_strs <- append(char_strs, digit)
        digits <- append(digits, "9")
      }else{
        # remaining 6 character value is '0'
        char_strs <- append(char_strs, digit)
        digits <- append(digits, "0")
      }
    }else{ next }
    
  }
  
  # update the named list again
  char_strs_now <- char_strs
  names(char_strs_now) <- digits
  
  # finally can assign 5 and 2
  for(digit in digit_list){
    
    digit <- str_arrange(digit)
    chars <- nchar(digit)
    
    if(chars == 5){
      if(str_check(digit, char_strs_now["3"])){ # we already assigned 3
        next
      }else if(str_check(char_strs_now["9"], digit) & !str_check(digit, char_strs_now["1"])){
        # '5' is contained fully within '9', and does not contain all of '1'
        char_strs <- append(char_strs, digit)
        digits <- append(digits, "5")
      }else{
        # the remaining 5 character value is '2'
        char_strs <- append(char_strs, digit)
        digits <- append(digits, "2")
      }
    }else{ next }
    
  }
  
  # want to reverse the names and entries for the return - will be looking up the strings and converting to number instead
  names(digits) <- char_strs
  return(digits)
  
}

# need one more function to convert each of the final strings to an actual number
convert_nums <- function(digits, convert){
  
  # use the function to create a named list showing the conversion from string to numeric value
  digit_dict <- assign_digits(digits)
  convert_num_list <- str_split(trimws(convert), " ")[[1]]
  output_digits <- c()
  # for each string of characters (a number) look up the numeric value in the named list we created
  for(num in convert_num_list){
    num <- str_arrange(num)
    value <- digit_dict[num]
    output_digits <- append(output_digits, value)
  }
  
  # collapse the list of individual numbers into one number and convert to numeric
  converted_value <- as.numeric(glue_collapse(output_digits))
  
  return(converted_value)
  
}

# now I can just run the function for every row of the data and add up the values
total_output <- 0
for(i in 1:length(day8_data$X1)){
  total_output <- total_output + convert_nums(day8_data$X1[i], day8_data$X2[i])
}

# hopefully this is the answer
answer_part2 <- total_output

# it was correct. phew.
