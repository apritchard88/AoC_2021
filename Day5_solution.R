library(tidyverse)

# following function taken from https://stackoverflow.com/questions/18186357/importing-csv-file-with-multiple-character-separator-to-r
# since our delimiter is a combination of characters
read <- function(fileName, separators) {
  data <- readLines(con <- file(fileName))
  close(con)
  records <- sapply(data, strsplit, split=separators)
  dataFrame <- data.frame(t(sapply(records,c)))
  rownames(dataFrame) <- 1: nrow(dataFrame)
  return(as.data.frame(dataFrame,stringsAsFactors = FALSE))
}

# read the data using our function
day5_data <- read("Day5_data.txt", separators = " -> ")
names(day5_data) <- c("start", "end")

# separate the data into x1,y1,x2,y2
day5_data <- day5_data %>%
  separate(start, into = c('x1', 'y1'), sep = ",") %>%
  separate(end, into = c('x2','y2'), sep = ",") %>%
  # need to add 1 to each values as the data starts at 0 (python style)
  mutate(x1 = as.numeric(x1)+ 1,
         x2 = as.numeric(x2)+ 1,
         y1 = as.numeric(y1)+ 1,
         y2 = as.numeric(y2)+ 1)

# for part 1, want only straight lines - filter for only x1=x2 or y1=y2
part_1 <- FALSE
if(part_1 == TRUE){
  
  day5_data <- day5_data %>%
    filter(x1==x2 | y1==y2)
  
}

# get the max_x and max_y to create a blank grid as a starting point
max_x <- max(max(day5_data$x1), max(day5_data$x2))
max_y <- max(max(day5_data$y1), max(day5_data$y2))

# following function adds 1 to each square the horizontal or vertical lines pass through
add_hit <- function(grid_map, xmin, xmax, ymin, ymax){
  
  # need to calculate start and end of lines (as iterating will go forwards)
  x_min <- min(xmin, xmax)
  x_max <- max(xmin, xmax)
  y_min <- min(ymin, ymax)
  y_max <- max(ymin, ymax)
  
  # if the line is horizontal or vertical
  if(xmin==xmax | ymin==ymax){
    for(x in x_min:x_max){
      for(y in y_min:y_max){
        # add 1 to the relevant cell of the grid
        grid_map[y, x] <- sum(grid_map[y, x], 1, na.rm = TRUE)
      }
    }
  }else{
    # if diagonal line, we instead will make lists of the values and iterate through
    # because diagonal lines are sensitive to the start/end points (unlike horizontal/vertical lines)
    y_list <- ymin:ymax
    x_list <- xmin:xmax
    for(i in 1:length(x_list)){
      # add 1 to the relevant square on the list (as taken from the lists)
      grid_map[y_list[i], x_list[i]] <- sum(grid_map[y_list[i], x_list[i]], 1, na.rm = TRUE)
    }
    
  }
  return(grid_map)
  
}

# make an empty matrix with dimensions given by maximum x and y values
grid <- matrix(nrow = max_y, ncol = max_x)

# loop through all the rows in the data, get the x1,x2 y1,y2 coords, use function to add the line in
for(i in 1:nrow(day5_data)){
  x1 = day5_data[[1]][i]
  x2 = day5_data[[3]][i]
  y1 = day5_data[[2]][i]
  y2 = day5_data[[4]][i]
  grid <- add_hit(grid, x1, x2, y1, y2)  
}

# convert to a data frame as it is easier to view
grid_df <- as.data.frame(grid) 

# change all the NAs to 0
grid_df[is.na(grid_df)] <- 0

# answer should be the number of entries in grid that are >1
answer <- sum(grid > 1, na.rm = TRUE)
