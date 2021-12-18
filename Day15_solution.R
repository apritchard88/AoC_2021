library(tidyverse)
library(igraph)

options(digits=100)
day15_data <- read_delim("Day15_data.txt", delim = "\n", col_names = FALSE, col_types = 'c')

data_strings <- day15_data[[1]] 

grid <- list()
for(i in 1:length(data_strings)){
  new_row <- strsplit(as.character(data_strings[[i]]),split='')[[1]]
  grid[[i]] <- as.numeric(new_row)
}

### for part 2, the grid needs expanding 5x in length and width
expand_i <- 5
expand_j <- 5
row_length <- length(grid[[1]])
nrows <- length(grid)

part_2 <- TRUE # to change which part we are answering (controls whether grid is expanded or not)

if(part_2 == TRUE){
  for(i in 1:nrows){
    # i represents the row
    for(j in 1:row_length){
      for(xi in 1:expand_i){
        for(xj in 1:expand_j){
          new_i <- i + ((xi-1) * nrows)
          # if we are adding rows, need to initialise row of 0s first to avoid out of bounds error
          if(new_i > length(grid)){
            grid[[new_i]] <- rep(0,(expand_j*row_length))
          }
          new_j <- j + ((xj-1) * row_length)
          # the weight of the new square is the original grid value + right chunk number + down chunk number
          new_weight <- grid[[i]][j] + (xi - 1) + (xj - 1)
          # if the weight passed 9, subtract 9 from it
          grid[[new_i]][new_j] <- ifelse(new_weight > 9, new_weight - 9, new_weight)
        }
      }
    }
    
  }
}
# at this point grid is the full grid of all values
# to find the shortest path I am going to turn the grid into a network graph, with the values becoming the weights
# then I can use 'shortest path' to find the path through the grid that has the lowest weight

# had to turn the following into a function to run part 2 in a reasonable time
# the function runs through every cell in the grid and creates a row for every adjacent cell and the weight of that move
# ie cell (2,2) will get 4 rows: (1,2), (3,2), (2,1), (2,3)
get_links <- function(grid, process_row_start, process_row_end){
  # empty vectors to store the start, end, weight of each link in the grid
  start_node <- c()
  end_node <- c()
  weight <- c()
  
  imax <- length(grid)
  jmax <- length(grid[[1]])
  
  for(i in process_row_start:process_row_end){
    # i represents the row
    for(j in 1:jmax){
      # j represents the column
      start_point <- str_c(i,",",j, collapse = TRUE)
      if(between(i,2,imax-1) & between(j,2,jmax-1)){
        right_point <-  str_c(i,",",j+1, collapse = TRUE)
        right_weight <- grid[[i]][j+1]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, right_point)
        weight <- append(weight, right_weight)
        
        left_point <-  str_c(i,",",j-1, collapse = TRUE)
        left_weight <- grid[[i]][j-1]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, left_point)
        weight <- append(weight, left_weight)
        
        down_point <-  str_c(i+1,",",j, collapse = TRUE)
        down_weight <- grid[[i+1]][j]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, down_point)
        weight <- append(weight, down_weight)
        
        up_point <-  str_c(i-1,",",j, collapse = TRUE)
        up_weight <- grid[[i-1]][j]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, up_point)
        weight <- append(weight, up_weight)
      }else if(i==1 & between(j,2,jmax-1)){
        # on top row, only have left, down, right
        right_point <-  str_c(i,",",j+1, collapse = TRUE)
        right_weight <- grid[[i]][j+1]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, right_point)
        weight <- append(weight, right_weight)
        
        left_point <-  str_c(i,",",j-1, collapse = TRUE)
        left_weight <- grid[[i]][j-1]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, left_point)
        weight <- append(weight, left_weight)
        
        down_point <-  str_c(i+1,",",j, collapse = TRUE)
        down_weight <- grid[[i+1]][j]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, down_point)
        weight <- append(weight, down_weight)
      }else if(i==imax & between(j,2,jmax-1)){
        # on bottom row, only have left, up, right
        right_point <-  str_c(i,",",j+1, collapse = TRUE)
        right_weight <- grid[[i]][j+1]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, right_point)
        weight <- append(weight, right_weight)
        
        left_point <-  str_c(i,",",j-1, collapse = TRUE)
        left_weight <- grid[[i]][j-1]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, left_point)
        weight <- append(weight, left_weight)
        
        up_point <-  str_c(i-1,",",j, collapse = TRUE)
        up_weight <- grid[[i-1]][j]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, up_point)
        weight <- append(weight, up_weight)
      }else if(between(i,2,imax-1) & j==1){
        # in first column, only have down, up, right
        right_point <-  str_c(i,",",j+1, collapse = TRUE)
        right_weight <- grid[[i]][j+1]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, right_point)
        weight <- append(weight, right_weight)
        
        up_point <-  str_c(i-1,",",j, collapse = TRUE)
        up_weight <- grid[[i-1]][j]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, up_point)
        weight <- append(weight, up_weight)
        
        down_point <-  str_c(i+1,",",j, collapse = TRUE)
        down_weight <- grid[[i+1]][j]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, down_point)
        weight <- append(weight, down_weight)
      }else if(between(i,2,imax-1) & j==jmax){
        # in last column, only have up, down, left
        up_point <-  str_c(i-1,",",j, collapse = TRUE)
        up_weight <- grid[[i-1]][j]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, up_point)
        weight <- append(weight, up_weight)
        
        down_point <-  str_c(i+1,",",j, collapse = TRUE)
        down_weight <- grid[[i+1]][j]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, down_point)
        weight <- append(weight, down_weight)
        
        left_point <-  str_c(i,",",j-1, collapse = TRUE)
        left_weight <- grid[[i]][j-1]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, left_point)
        weight <- append(weight, left_weight)
      }else if(i==1 & j==1){
        # in top left, only have right, down
        right_point <-  str_c(i,",",j+1, collapse = TRUE)
        right_weight <- grid[[i]][j+1]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, right_point)
        weight <- append(weight, right_weight)
        
        down_point <-  str_c(i+1,",",j, collapse = TRUE)
        down_weight <- grid[[i+1]][j]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, down_point)
        weight <- append(weight, down_weight)
      }else if(i==1 & j==jmax){
        # in top right, only left, down
        left_point <-  str_c(i,",",j-1, collapse = TRUE)
        left_weight <- grid[[i]][j-1]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, left_point)
        weight <- append(weight, left_weight)
        
        down_point <-  str_c(i+1,",",j, collapse = TRUE)
        down_weight <- grid[[i+1]][j]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, down_point)
        weight <- append(weight, down_weight)
      }else if(i==imax & j==1){
        # in bottom left, only up and right
        right_point <-  str_c(i,",",j+1, collapse = TRUE)
        right_weight <- grid[[i]][j+1]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, right_point)
        weight <- append(weight, right_weight)
        
        up_point <-  str_c(i-1,",",j, collapse = TRUE)
        up_weight <- grid[[i-1]][j]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, up_point)
        weight <- append(weight, up_weight)
      }else if(i==imax & j==jmax){
        # in bottom right, only left and up
        left_point <-  str_c(i,",",j-1, collapse = TRUE)
        left_weight <- grid[[i]][j-1]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, left_point)
        weight <- append(weight, left_weight)
        
        up_point <-  str_c(i-1,",",j, collapse = TRUE)
        up_weight <- grid[[i-1]][j]
        start_node <- append(start_node, start_point)
        end_node <- append(end_node, up_point)
        weight <- append(weight, up_weight)
      }
    }
  }
  
  
  graph_df <- data.frame(v1 = start_node, v2 = end_node, weight = weight)
  
  return(graph_df)
}

full_df <- data.frame(v1 = c(), v2 = c(), weight = c())
for(i in 1:length(grid)){
  next_row_df <- get_links(grid, i, i)
  full_df <- rbind(full_df, next_row_df)
}

g <- graph_from_data_frame(full_df, directed = TRUE)

# only difference between part 1 and part 2 is the end point of the path
end_part_1 <- "100,100"
bottom_right <- str_c(imax, ",", jmax, collapse = TRUE)

# 
path_part_1 <- shortest_paths(g, from = "1,1", to = end_part_1, mode = "out")
shortest_dist_part_1 <- distances(g, v = "1,1", to = end_part_1, mode = "out")

path_part_2 <- shortest_paths(g, from = "1,1", to = bottom_right, mode = "out")
shortest_dist_part_2 <- distances(g, v = "1,1", to = bottom_right, mode = "out")

answer_part_1 <- shortest_dist_part_1[[1]][1]
answer_part_1
answer_part_2 <- shortest_dist_part_2[[1]][1]
answer_part_2
