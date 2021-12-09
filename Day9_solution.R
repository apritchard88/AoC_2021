library(tidyverse)

day9_data <- read_delim("Day9_data.txt", delim = "\n", col_names = FALSE)

data_strings <- day9_data[[1]] 

cols <- nchar(data_strings[1]) 
rows <- length(data_strings)

min_points_sum <- 0 # to hold the sum of all the min points
min_points_list <- c() # to hold the grid ref of all the min points

# loop through every point in the grid
# if none of the adjacent points has a lower value, add it to the total sum and record it in the min_points_list
# this is a bit of a mess as I try to account for cells at the extremities of the grid (ie where there are <4 adjacent points)
for(row in 1:rows){
  for(col in 1:cols){
    up <- 9
    down <- 9
    left <- 9
    right <- 9
    this_val <- substring(data_strings[row], col, col)
    if(row == 1){
      if(col == 1){
        right <- substring(data_strings[row], col+1, col+1)
        down <- substring(data_strings[row+1], col, col)
      }else if(col == cols){
        left <- substring(data_strings[row], col-1, col-1)
        down <- substring(data_strings[row+1], col, col)
      }else{
        right <- substring(data_strings[row], col+1, col+1)
        down <- substring(data_strings[row+1], col, col)
        left <- substring(data_strings[row], col-1, col-1)
      }
    }else if(row == rows){
      if(col == 1){
        right <- substring(data_strings[row], col+1, col+1)
        up <- substring(data_strings[row-1], col, col)
      }else if(col == cols){
        left <- substring(data_strings[row], col-1, col-1)
        up <- substring(data_strings[row-1], col, col)
      }else{
        right <- substring(data_strings[row], col+1, col+1)
        up <- substring(data_strings[row-1], col, col)
        left <- substring(data_strings[row], col-1, col-1)
      }
    }else{
      if(col == 1){
        right <- substring(data_strings[row], col+1, col+1)
        up <- substring(data_strings[row-1], col, col)
        down <- substring(data_strings[row+1], col, col)
      }else if(col == cols){
        left <- substring(data_strings[row], col-1, col-1)
        up <- substring(data_strings[row-1], col, col)
        down <- substring(data_strings[row+1], col, col)
      }else{
        right <- substring(data_strings[row], col+1, col+1)
        up <- substring(data_strings[row-1], col, col)
        left <- substring(data_strings[row], col-1, col-1)
        down <- substring(data_strings[row+1], col, col)
      }
      
    }
    right <- as.numeric(right)
    left <- as.numeric(left)
    up <- as.numeric(up)
    down <- as.numeric(down)
    this_val <- as.numeric(this_val)
    if(this_val < right & this_val < up & this_val < left & this_val < down){
      print(paste0("Minimum point of ", this_val," found at row = ", row, " col = ", col))
      min_points_sum <- min_points_sum + as.numeric(this_val) + 1
      new_point <- paste0(row, ", ", col)
      min_points_list <- append(min_points_list, new_point)
    }
  }
}

answer_part1 <- min_points_sum

### part 2 - need to find the minima accessed from each point in the grid
# will try to use the list of min points created previously
# create a named vector with an entry for each minima discovered, and start each entry at 0
point_count <- rep(0, length(min_points_list))
names(point_count) <- min_points_list

# need a function that will take a starting point, and move it to an adjacent square where the value is lower
find_next_point <- function(row, col, grid){
  cols <- nchar(grid[1])
  rows <- length(grid)
  row <- as.numeric(row)
  col <- as.numeric(col)
  if(col == cols){
    right <- 9
  }else{
    right <- as.numeric(substring(data_strings[row], col+1, col+1))
  }
  if(row == 1){
    up <- 9
  }else{
    up <- as.numeric(substring(data_strings[row-1], col, col))
  }
  if(col == 1){
    left <- 9
  }else{
    left <- as.numeric(substring(data_strings[row], col-1, col-1))
  }
  if(row == rows){
    down <- 9
  }else{
    down <- as.numeric(substring(data_strings[row+1], col, col))
  }
  print(paste0(up, ", ",down, ", ", left, ", ", right))
  this_val <- as.numeric(substring(data_strings[row], col, col))
  if(this_val > right){
    next_row <- row
    next_col <- col+1
  }else if(this_val > up){
    next_row <- row-1
    next_col <- col
  }else if(this_val > left){
    next_row <- row
    next_col <- col-1
  }else if(this_val > down){
    next_row <- row+1
    next_col <- col
  }else{
    next_row <- row
    next_col <- col
  }
  new_pos <- paste0(next_row, ", ", next_col)
  return(new_pos)
}

# now I will loop through every point in the grid and use the function and a while loop to find where the next lowest point
# no longer changes - ie we are in a minima
# can then increase the count for that minima by 1 in our named list
for(row in 1:rows){
  for(col in 1:cols){
    this_pos <- paste0(row, ", ", col)
    new_pos <- find_next_point(row, col, data_strings)
    if(as.numeric(substring(data_strings[row], col, col)==9)){next}
    # while the next position continues to change, look for another lower point
    while(!(this_pos == new_pos)){
      this_pos <- new_pos
      this_row <- str_split(this_pos, ", ")[[1]][1]
      this_col <- str_split(this_pos, ", ")[[1]][2]
      new_pos <- find_next_point(this_row, this_col, data_strings)
    }
    # once we find the minima, increment the corresponding value in our named vector
    point_count[this_pos] <- point_count[this_pos] + 1
  }
}

# now point_count contains the count of how many points ended up at each minima
# just need to sort and take the top 3 values
N <- 3
partial <- length(point_count) - N + 1
Nth <- sort(point_count, partial = partial)[partial]
indexes <- which(point_count >= Nth)
top_n <- point_count[indexes]
answer_part2 <- prod(top_n)

