library(tidyverse)

day19_data <- readLines("Day19_data.txt")

example <- "[[6,[5,[4,[3,2]]]],1]"

# function for performing the 'explode' operation
num_explode <- function(string){
  open_count <- 0
  # look for any point where the total number of [ minus total ] is 5 (implies pair is embedded in 4 other pairs)
  for(char in 1:nchar(string)){
    this_char <- substring(string, char, char)
    if(this_char == "["){
      open_count <- open_count + 1
    }else if(this_char == "]"){
      open_count <- open_count - 1
    }
    if(open_count == 5){
      break
    }
  }
  # if we never find a suitable pair to explode, just return the starting string
  if(open_count < 5){
    return(string)
  }
  # the section we split off is everything after the char we reached
  split_secion <- substring(string, char, nchar(string))
  
  # use regex to look for something matching "[anynumbers,anynumbers]"
  pair_locs <- gregexpr("\\[[0-9]+,[0-9]+\\]", split_secion)[[1]]
  
  # char now modified by the start point of the regex (for cases with [[ where the first bracket triggers the condition)
  char <- char + (pair_locs[1]-1)
  
  # length of the pair is stored as an attribute. use it to find how many characters
  pair_length <- attributes(pair_locs)$match.length[1]
  split_pair <- substring(string, char+1, char+pair_length-2)

  left_val <- as.numeric(str_split(split_pair, ",")[[1]][1])
  right_val <- as.numeric(str_split(split_pair, ",")[[1]][2])
  left_portion <- substring(string, 1, char-1)
  
  # if the portion left of the pair contains a number
  if(str_detect(left_portion, "[0-9]")==TRUE){
    # find positions of numbers, reverse list and take the first element (this is pos of last number)
    nums_left <- gregexpr("[0-9]+", left_portion)[[1]]

    left_num_pos <- nums_left[length(nums_left)]
    left_num_length <- attributes(nums_left)$match.length[length(nums_left)]

    left_num <- as.numeric(substring(left_portion, left_num_pos, (left_num_pos+left_num_length-1)))
    new_left_num <- left_num + left_val
    # construct the new left hand portion
    left_portion <- paste0(substring(left_portion,1,(left_num_pos-1)),
                           new_left_num,
                           substring(left_portion,(left_num_pos+left_num_length),nchar(left_portion)))
  }
  
  # do the same for the portion right of the pair we are exploding
  right_portion <- substring(string, (char+nchar(split_pair)+2), nchar(string))
  if(str_detect(right_portion, "[0-9]")==TRUE){
    # find positions of numbers, reverse list and take the first element (this is pos of last number)
    nums_right <- gregexpr("[0-9]+", right_portion)[[1]]
    right_num_pos <- nums_right[1]
    right_num_length <- attributes(nums_right)$match.length[1]
    right_num <- as.numeric(substring(right_portion, right_num_pos, right_num_pos+right_num_length-1))
   
    new_right_num <- right_num + right_val
    right_portion <- paste0(substring(right_portion,1,(right_num_pos-1)),
                            new_right_num,
                            substring(right_portion,(right_num_pos+right_num_length),nchar(right_portion)))
  }
  output <- paste0(left_portion, "0", right_portion)
  return(output)
}


# function to perform the 'split' operation
num_split <- function(string){
  # first check if there is a double digit number present
  if(str_detect(string, "[0-9][0-9]") == FALSE){
    return(string)
  }
  # find postion of first double digit number
  double_locs <- (gregexpr("[0-9][0-9]+", string)[[1]])
  split_pos <- double_locs[1]
  split_num_length <- attributes(double_locs)$match.length[1]
  split_num <- as.numeric(substring(string, split_pos, split_pos+split_num_length-1))
  # calculate the left and right values of the new pair we are forming
  left_val <- floor(split_num/2)
  right_val <- ceiling(split_num/2)
  new_pair <- paste0("[",left_val,",",right_val,"]")
  output <- paste0(substring(string, 1, split_pos-1 ),
                   new_pair,
                   substring(string, split_pos+split_num_length, nchar(string)))
  return(output)
}

# function to add together the 2 strings 
add_nums <- function(num1, num2){
  answer <- paste0("[",num1,",",num2,"]")
  return(answer)
}

# function to run an addition from start to finish
run_addition <- function(num1, num2){
  value <- add_nums(num1,num2)
  out_val <- ""
  while(!(value == out_val)){
    out_val <- value
    value <- num_explode(value) # find the results of explode operator
    # only run the split operation if explode did nothing
    # since only one operation should run each time (with preference for explode)
    if(value == out_val){
      value <- num_split(value)
    }
    
  }
  return(value)
}

# find the magnitude of a pair
pair_mag <- function(pair){
  left_val <- as.numeric(str_replace(str_split(pair, ",")[[1]][1], "\\[", ""))
  right_val <- as.numeric(str_replace(str_split(pair, ",")[[1]][2], "\\]", ""))
  out_val <- (3*left_val) + (2*right_val)
  return(out_val)
}

# get the full magnitude of a number, by gradually reducing pairs
get_mag <- function(string){
  # get positions of reducible pairs
  pair_locs <- gregexpr("\\[[0-9]+,[0-9]+\\]", string)[[1]]
  new_string <- string
  
  # when we run out of pairs, this function returns -1
  while(!pair_locs[1]==-1){
    string <- new_string
    new_string <- ""
    
    for(pair in 1:length(pair_locs)){
      length_match <- attributes(pair_locs)$match.length[pair]
      start_point <- pair_locs[pair]
      if(pair==1){ # add the start of the string
        new_string <- paste0(new_string, substring(string, 1, start_point-1))
      }
      replacement <- pair_mag(substring(string, start_point, start_point+(length_match-1)))
      new_string <- paste0(new_string, replacement)
      if(is.na(pair_locs[pair+1])){
        next_sec <- substring(string, start_point+length_match, nchar(string))
      }else{
        start_next <- pair_locs[pair+1]
        next_sec <- substring(string, start_point+length_match, start_next-1)
      }
      new_string <- paste0(new_string, next_sec)
    }
    pair_locs <- gregexpr("\\[[0-9]+,[0-9]+\\]", new_string)[[1]]
  }
  return(new_string)
}

# for part 1, loop through the input data and add each additional line until we have an answer
start_val <- day19_data[1]
for(next_val in 1:length(day19_data)){
  if(!next_val==length(day19_data)){
    start_val <- run_addition(start_val, day19_data[next_val+1])
  }
}
answer_part1 <- get_mag(start_val)
print(answer_part1)

# now want to find the largest possible magnitude of any 2 pairs of numbers for part 2
max_mag <- 0
for(i in 1:length(day19_data)){
  for(j in 1:length(day19_data)){
    if(!(i==j)){
      answer <- as.numeric(get_mag(run_addition(day19_data[i],day19_data[j])))
    }
    if(answer > max_mag){
      print(answer)
      max_mag <- answer
    }
    
  }
  
}

print(max_mag)