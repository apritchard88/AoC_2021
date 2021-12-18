library(tidyverse)

day13_data <- read_delim("Day13_data.txt", delim = ",", col_names = FALSE, col_types = 'cc')
names(day13_data) <- c('x','y')

# hit coords are where y is not NA (ie both x and y are present)
coords <- day13_data %>% 
  filter(!is.na(y)) %>%
  # need to make sure x and y are numeric. also add 1 as r references start from 1, not 0
  mutate(x = as.numeric(x)+1,
         y = as.numeric(y)+1)

# the set of instructions is the rows where y is NA
# following code separates the instruction into an axis (x or y) and the point
instructions <- day13_data %>% 
  filter(is.na(y)) %>%
  select(x) %>%
  rowwise() %>%
  mutate(part = str_split(x, ' ')[[1]][3],
         axis = str_split(part, '=')[[1]][1],
         point = as.numeric(str_split(part, '=')[[1]][2])+1) %>%
  select(axis, point)


# function to change the x or y values of points depending on the reflecting line
# reflection changes coord to r - (x - r) where r is the point we are reflection about
# after changing coords can take distinct rows to remove now overlapping points
reflect_line <- function(df, axis, point){
  if(axis == "y"){
    new_df <- df %>%
      mutate(y = ifelse(y > point, point - (y - point), y),
             x = x) %>%
      select(y, x) %>%
      distinct()
  }else{
    new_df <- df %>%
      mutate(x = ifelse(x > point, point - (x - point), x),
             y = y) %>%
      select(y, x) %>%
      distinct()
  }
  return(new_df)
}

df_first_transform <- reflect_line(coords, instructions$axis[1], instructions$point[1])
answer_part1 <- nrow(df_first_transform)

# part 2 requires us to make all folds
df_updated <- coords
for(i in 1:nrow(instructions)){
    df_updated <- reflect_line(df_updated, instructions$axis[i], instructions$point[i])
}

# need to visualise the remaining points to read off the answer
# get the max x and max y of the remaining points and create a matrix representing this
max_x <- max(df_updated$x)
max_y <- max(df_updated$y)

grid <- matrix(, max_y, max_x)

# fill the matrix with the hits
for(i in 1:nrow(df_updated)){
  row <- df_updated$y[i]
  col <- df_updated$x[i]
  grid[row, col] <- 1
}

# now grid is filled with 1 and NA - the 1s should spell out a series of capital letters, separated by columns of NAs
# print the grid to screen with NAs removed and read off the letters (8 in total)
view(replace_na(grid, ''))
