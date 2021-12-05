library(tidyverse)

text_data <- readLines("Day4_data.txt")

# function to parse a group of lines into a bingo card in data frame form
parse_card <- function(lines){
  col1 <- c()
  col2 <- c()
  col3 <- c()
  col4 <- c()
  col5 <- c()
  for(line in lines){
    split_nums <- str_split(str_squish(line), " ")
    col1 <- append(col1, as.numeric(split_nums[[1]][1]))
    col2 <- append(col2, as.numeric(split_nums[[1]][2]))
    col3 <- append(col3, as.numeric(split_nums[[1]][3]))
    col4 <- append(col4, as.numeric(split_nums[[1]][4]))
    col5 <- append(col5, as.numeric(split_nums[[1]][5]))
  }
  df <- data.frame(col1 = col1, col2 = col2, col3 = col3, col4 = col4, col5 = col5)
  return(df)
}

cards <- list()
tot_rows <- length(text_data)
i <- 1
card_count <- 1
while(i < tot_rows){
  if(i == 1){
    nums_called <- text_data[i] 
    i <- i+1
    next
  }
  row <- text_data[i]
  if(row == ""){
    i <- i+1
  }else{
    card_lines <- text_data[i:(i+4)]
    df <- parse_card(card_lines)
    cards[[card_count]] <- df
    i <- i+5
    card_count <- card_count+1
  }
  
}




# function to look for the number in the dataframe and change it to another value if found
change_val <- function(df, number, value){
    df[df == number] <- value
    return(df)
}

# function to check if any row or column sums to 5000
check_winner <- function(df){
  row_win <- any(rowSums(df, na.rm = TRUE)==5000)
  col_win <- any(colSums(df, na.rm = TRUE)==5000)
  if(row_win == TRUE | col_win == TRUE){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

# function to loop dataframe until it wins, return how many turns it took
# Change marked values to 1000 so we can search for rows/cols summing to 5000 to look for wins
# Considered changing to 0, but 0 is a number used in the game so can't do that
# Considered changing to NA, but rowSum(NA,NA,NA,NA,NA) = rowSum(NA,NA,NA,NA,0) using na.rm=TRUE
# so would risk a false win
find_win <- function(df, num_list){
  
  count <- 0
  for(i in 1:length(num_list[[1]])){
    check_val <- num_list[[1]][i]
    df <- change_val(df, check_val, 1000)
    is_win <- check_winner(df)
    count <- count+1
    if(is_win == TRUE){break}
  }
  
  return(count)
  
}


num_list <- str_split(nums,",") # this is the list of called numbers
current_min <- length(num_list[[1]]) # set the starting winning point (the maximum)

current_max <- 1
for(i in 1:length(cards)){
  this_card <- cards[[i]]
  this_win <- find_win(this_card, num_list)
  if(this_win < current_min){
    winning_card <- this_card
    current_min <- this_win
  }
  if(this_win > current_max){
    losing_card <- this_card
    current_max <- this_win
  }
}

### now we have the winning card, go through the numbers and change to NAs
# change marked vals to NA now instead so we can sum up remaining numbers
for(i in 1:current_min){
  check_val <- num_list[[1]][i]
  winning_card <- change_val(winning_card, check_val, NA)
}

# the answer is the total of all unmarked numbers on the card multipled by the last number called
# get unmarked total by summing rowSums 
unmarked_total <- sum(rowSums(winning_card, na.rm = TRUE))
last_num <- as.numeric(num_list[[1]][current_min])
part1_ans <- unmarked_total*last_num


### part 2 wants us to find the losing card
for(i in 1:current_max){
  check_val <- num_list[[1]][i]
  losing_card <- change_val(losing_card, check_val, NA)
}

unmarked_total <- sum(rowSums(losing_card, na.rm = TRUE))
last_num <- as.numeric(num_list[[1]][current_max])
part2_ans <- unmarked_total*last_num
