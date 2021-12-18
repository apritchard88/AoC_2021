library(tidyverse)

data_day17 <- readLines("Day17_data.txt")

# parse the input string to get the x range and y range of the target
x_part <- str_remove(str_remove(str_split(data_day17, ' ')[[1]][3], ","), "x=")
y_part <- str_remove(str_split(data_day17, ' ')[[1]][4], "y=")

xmin <- as.numeric(str_split(x_part, "\\.\\.")[[1]][1])
xmax <- as.numeric(str_split(x_part, "\\.\\.")[[1]][2])
ymin <- as.numeric(str_split(y_part, "\\.\\.")[[1]][1])
ymax <- as.numeric(str_split(y_part, "\\.\\.")[[1]][2])

max_allowed_y <- -1000 # for part 1, an initial value showing the highest y achieved by a viable solution
solution_count <- 0 # count how many combinations of x velocity, y velocity land us in the target

# set a big range of starting velocity values to check so we don't miss any solutions
for(xv in 1:10000){
  for(yv in -100:100){
    max_y_achieved <- -1000 # max y achieved by this route
    start_x_vel <- xv
    start_y_vel <- yv
    x_vel <- xv
    y_vel <- yv
    
    x <- 0
    y <- 0
    while(x<xmax & between(y,-10000,10000)){
      x <- x + x_vel
      y <- y + y_vel
      if(y > max_y_achieved){
        max_y_achieved <- y
      }
      if(!x_vel==0){
        x_vel <- ifelse(x_vel>0, x_vel-1, x_vel+1)
      }
      y_vel <- y_vel - 1
      # next section is if we find a solution
      if(between(x, xmin, xmax) & between(y, ymin, ymax)){
        print(paste0(start_x_vel, ", ",start_y_vel))
        solution_count <- solution_count + 1
        # if the max y of this solution is the highest so far, it becomes the new high mark
        if(max_y_achieved > max_allowed_y){
          max_allowed_y <- max_y_achieved
        }
        break
      }
    }
  }
}

answer_part1 <- max_allowed_y
answer_part1

answer_part2 <- solution_count
solution_count
