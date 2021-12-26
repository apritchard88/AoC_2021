library(tidyverse)
day25_data <- read_delim("Day25_data.txt", delim = "\n", col_names = FALSE, col_types = 'c')

data_strings <- day25_data[[1]] 

# read all the symbols into a grid (vector of vectors) for easy iteration
grid <- list()
for(i in 1:length(data_strings)){
  new_row <- strsplit(as.character(data_strings[[i]]),split='')[[1]]
  grid[[i]] <- new_row
}

next_iteration <- function(grid){
  any_change <- FALSE
  new_grid <- grid
  row_length <- length(grid[[1]])
  rows <- length(grid)
  
  # for every cell in the grid, look for the > symbol
  # then check if the cell to the right is empty (has a .)
  # if so move the > symbol in the new grid, and replace with a .
  for(i in 1:row_length){
    for(j in 1:rows){
      this_char <- grid[[j]][i]
      if(this_char == ">"){
        next_right <- ifelse(i==row_length, 1, i+1)
        target <- grid[[j]][next_right]
        if(target == "."){
          any_change <- TRUE
          new_grid[[j]][next_right] <- ">"
          new_grid[[j]][i] <- "."
        }
      }
    }
  }
  # now again for the downs
  new_grid_2 <- new_grid
  for(i in 1:row_length){
    for(j in 1:rows){
      this_char <- new_grid[[j]][i]
      if(this_char == "v"){
        next_down <- ifelse(j==rows, 1, j+1)
        target <- new_grid[[next_down]][i]
        if(target == "."){
          any_change <- TRUE
          new_grid_2[[next_down]][i] <- "v"
          new_grid_2[[j]][i] <- "."
        }
      }
    }
  }
  # return a list with TRUE/FALSE if there was a change, and the new grid
  # needed because cannot make a simple comparison of grid 1 vs grid 2
  output <- list(any_change = any_change, new_grid = new_grid_2)
}

g1 <- next_iteration(grid)

i <- 0
any_change <- TRUE
while(any_change == TRUE){
  i <- i + 1
  next_grid_info <- next_iteration(grid)
  new_grid <- next_grid_info$new_grid
  any_change <- next_grid_info$any_change
  grid <- new_grid
}
answer_pt1 <- i
print(answer_pt1)
