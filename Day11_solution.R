library(tidyverse)

day11_data <- read_delim("Day11_data.txt", delim = "\n", col_names = FALSE)

data_strings <- day11_data[[1]] 

data_grid <- list()
for(i in 1:length(data_strings)){
  new_row <- strsplit(as.character(data_strings[[i]]),split='')[[1]]
  data_grid[[i]] <- as.numeric(new_row)
}

# function to add 1 to every octopus
next_step <- function(grid){
  for(i in 1:length(grid)){
    for(j in 1:length(grid[[1]])){
      grid[[i]][j] <- grid[[i]][j] + 1 
    }
  }
  return(grid)
}

# function to count how many are greater than 9
count_over_9 <- function(grid){
  count <- 0
  for(i in 1:length(grid)){
    for(j in 1:length(grid[[1]])){
      this_val <- grid[[i]][j] 
      if(is.na(this_val)){
        next
      }
      if(this_val>9){
        count <- count + 1
      }
    }
  }
  return(count)
}

# function to count total NA values (for part 2)
count_NA <- function(grid){
  count <- 0
  for(i in 1:length(grid)){
    for(j in 1:length(grid[[1]])){
      this_val <- grid[[i]][j] 
      if(is.na(this_val)){
        count <- count + 1
      }else(next)
    }
  }
  return(count)
}

# function to iterate all adjacent to greater than 9
# long and painful to account for edges of the grid (ie i or j = 1 or 10)
iterate_neighbours <- function(grid){
  new_grid <- grid
  for(i in 1:length(grid)){
    for(j in 1:length(grid[[1]])){
      this_val <- grid[[i]][j]
      if(is.na(this_val)){
        next
      }
      if(this_val>9){
        print(paste0(i, ", ",j))
        new_grid[[i]][j] <- NA
        grid[[i]][j] <- NA
        if(between(i,2,9) & between(j,2,9)){
          new_grid[[i-1]][j-1] <- new_grid[[i-1]][j-1] + 1
          new_grid[[i-1]][j] <- new_grid[[i-1]][j] + 1
          new_grid[[i-1]][j+1] <- new_grid[[i-1]][j+1] + 1
          new_grid[[i]][j-1] <- new_grid[[i]][j-1] + 1
          new_grid[[i]][j+1] <- new_grid[[i]][j+1] + 1
          new_grid[[i+1]][j-1] <- new_grid[[i+1]][j-1] + 1
          new_grid[[i+1]][j] <- new_grid[[i+1]][j] + 1
          new_grid[[i+1]][j+1] <- new_grid[[i+1]][j+1] + 1
        }else if(i==1 & between(j,2,9)){
          new_grid[[i]][j-1] <- new_grid[[i]][j-1] + 1
          new_grid[[i]][j+1] <- new_grid[[i]][j+1] + 1
          new_grid[[i+1]][j-1] <- new_grid[[i+1]][j-1] + 1
          new_grid[[i+1]][j] <- new_grid[[i+1]][j] + 1
          new_grid[[i+1]][j+1] <- new_grid[[i+1]][j+1] + 1
        }else if(i==10 & between(j,2,9)){
          new_grid[[i]][j-1] <- new_grid[[i]][j-1] + 1
          new_grid[[i]][j+1] <- new_grid[[i]][j+1] + 1
          new_grid[[i-1]][j-1] <- new_grid[[i-1]][j-1] + 1
          new_grid[[i-1]][j] <- new_grid[[i-1]][j] + 1
          new_grid[[i-1]][j+1] <- new_grid[[i-1]][j+1] + 1
        }else if(between(i,2,9) & j==1){
          new_grid[[i-1]][j] <- new_grid[[i-1]][j] + 1
          new_grid[[i-1]][j+1] <- new_grid[[i-1]][j+1] + 1
          new_grid[[i]][j+1] <- new_grid[[i]][j+1] + 1
          new_grid[[i+1]][j] <- new_grid[[i+1]][j] + 1
          new_grid[[i+1]][j+1] <- new_grid[[i+1]][j+1] + 1
        }else if(between(i,2,9) & j==10){
          new_grid[[i-1]][j] <- new_grid[[i-1]][j] + 1
          new_grid[[i-1]][j-1] <- new_grid[[i-1]][j-1] + 1
          new_grid[[i]][j-1] <- new_grid[[i]][j-1] + 1
          new_grid[[i+1]][j] <- new_grid[[i+1]][j] + 1
          new_grid[[i+1]][j-1] <- new_grid[[i+1]][j-1] + 1
        }else if(i==1 & j==1){
          new_grid[[i+1]][j] <- new_grid[[i+1]][j] + 1
          new_grid[[i+1]][j+1] <- new_grid[[i+1]][j+1] + 1
          new_grid[[i]][j+1] <- new_grid[[i]][j+1] + 1
        }else if(i==1 & j==10){
          new_grid[[i+1]][j] <- new_grid[[i+1]][j] + 1
          new_grid[[i+1]][j-1] <- new_grid[[i+1]][j-1] + 1
          new_grid[[i]][j-1] <- new_grid[[i]][j-1] + 1
        }else if(i==10 & j==1){
          new_grid[[i-1]][j] <- new_grid[[i-1]][j] + 1
          new_grid[[i-1]][j+1] <- new_grid[[i-1]][j+1] + 1
          new_grid[[i]][j+1] <- new_grid[[i]][j+1] + 1
        }else if(i==10 & j==10){
          new_grid[[i-1]][j] <- new_grid[[i-1]][j] + 1
          new_grid[[i-1]][j-1] <- new_grid[[i-1]][j-1] + 1
          new_grid[[i]][j-1] <- new_grid[[i]][j-1] + 1
        }
        #grid[[i]][j] <- NA
      }
    }
    
  }
  return(new_grid)
}

reset_all_NA <- function(grid){
  for(i in 1:length(grid)){
    for(j in 1:length(grid[[1]])){
      this_val <- grid[[i]][j] 
      if(is.na(this_val)){
        grid[[i]][j] <- 0
      }
    }
  }
  return(grid)
}

# now we can loop through 100 steps and apply the required stages
total_flashes <- 0
total_turns <- 100

current_grid <- data_grid
for(i in 1:total_turns){
 current_grid <- next_step(current_grid)
 count_flashes <- count_over_9(current_grid)
 # use a while loop to repeat the iteration until there are no more new flashes
 while(count_flashes >0){
   total_flashes <- total_flashes + count_flashes
   current_grid <- iterate_neighbours(current_grid)
   count_flashes <- count_over_9(current_grid)
 }
 current_grid <- reset_all_NA(current_grid)
}

answer_part1 <- total_flashes

# part 2 wants me to counts how many steps are required before all flash at the same time
# just count the total NAs after each grid
# use a while loop to keep going until there are 100 total NAs 
current_grid <- data_grid
NA_count <- 0
total_steps <- 0
while(NA_count < 100){
  current_grid <- next_step(current_grid)
  total_steps <- total_steps + 1
  count_flashes <- count_over_9(current_grid)
  while(count_flashes >0){
    total_flashes <- total_flashes + count_flashes
    current_grid <- iterate_neighbours(current_grid)
    count_flashes <- count_over_9(current_grid)
  }
  NA_count <- count_NA(current_grid)
  current_grid <- reset_all_NA(current_grid)
}

answer_part2 <- total_steps
