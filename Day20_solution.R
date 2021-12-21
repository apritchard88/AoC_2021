library(tidyverse)

day20_data <- readLines("Day20_data.txt")

# separate out the lines containing instructions and paste them to a single line
instructions <- ''
line_count <- 0
for(line in day20_data){
  if(line == ""){
    break
  }else{
    instructions <- paste0(instructions, line)
    line_count <- line_count+1
  }
}

# rest of input is the starting grid
grid_start <- line_count+2 # first line of grid (as there is a blank line between)
grid_start_line_length <- nchar(day20_data[grid_start])

# will pad with 100 pixel thick lines of ... all around starting grid
# since we are told the image actually extends to infinity and need to cover 50 iterations
full_line_length <- grid_start_line_length+200
blank_line <- str_c(rep('.',full_line_length), collapse = '')

# create the full grid with padded area around the actual input grid
full_grid <- c()
for(i in 1:100){
  full_grid <- append(full_grid, blank_line)
}
line_pad <- str_c(rep('.',100), collapse = '')
for(l in grid_start:length(day20_data)){
  this_line <- paste0(line_pad, day20_data[l], line_pad)
  full_grid <- append(full_grid, this_line)
}
for(i in 1:100){
  full_grid <- append(full_grid, blank_line)
}

# function to find the surrounding points and create the required binary number
# then look up the required character from the instructions and return it
change_char <- function(row,col,grid, inst){
  first_3 <- substring(grid[row-1],col-1,col+1)
  middle_3 <- substring(grid[row],col-1,col+1)
  last_3 <- substring(grid[row+1],col-1,col+1)
  full <- paste0(first_3, middle_3, last_3)
  full <- str_replace_all(full, "\\.", "0")
  full <- str_replace_all(full, "#", "1")
  value <- strtoi(full, base=2)+1 # need to add 1 due to R indexing
  new_char <- substring(inst, value, value)
  return(new_char)
}

# function to perform a single iteration of the grid
iterate_grid <- function(grid, instructions){
  out_grid <- c()
  for(i in 1:length(grid)){
    new_line <- ''
    for(j in 1:nchar(grid[1])){
      this_char <- substring(grid[i], j, j)
      # if we are on an edge all surrounding points must be the same
      # so value is either 0 or 512 depending on what the character is
      if(i %in% c(1, length(grid)) | j %in% c(1, nchar(grid[1]))){
        if(this_char=='.'){
          new_char <- substring(instructions, 1, 1)
        }else{
          new_char <- substring(instructions, 512, 512)
        }
      }else{
        new_char <- change_char(i, j, grid, instructions)
      }
      new_line <- paste0(new_line, new_char)
    }
    #print(new_line)
    out_grid <- append(out_grid, new_line)
  }
  return(out_grid)
}

# function to run through all pixels in grid and count total that are #
count_pixels <- function(grid){
  on_count <- 0
  for(i in 1:length(grid)){
    for(j in 1:nchar(grid[1])){
      this_char <- substring(grid[i], j, j)
      if(this_char == '#'){
        on_count <- on_count + 1
      }
    }
  }
  return(on_count)
}

iterations <- 50
out_grid <- full_grid

# perform the stipulated number of transformations
# this will take a while for 50 iterations
for(i in 1:iterations){
  print(paste0("Now on iteration number ",i))
  out_grid <- iterate_grid(out_grid, instructions)
}

answer_part1 <- count_pixels(out_grid)
print(answer_part1)
