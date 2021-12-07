library(tidyverse)

day7_data <- strsplit(readLines("Day7_data.txt"), ",")

# vals is the first element of day7_data - need to convert to numeric too
vals <- as.numeric(day7_data[[1]])

# midpoint should be the median of the values?
midpoint <- median(vals)

# now we want the total distance all numbers lie from the midpoint
total_fuel <- sum(abs(vals-midpoint))



get_distance <- function(x, target){
  distance <- abs(target-x)
  new_distance <- sum(1:distance)
  return(new_distance)
}


get_total_distance <- function(data, target){
  new_dist_df <- data %>%
    rowwise() %>%
    mutate(new_dist = get_distance(vals, target))
  tot_dist <- sum(new_dist_df$new_dist)
  return(tot_dist)
}

mean_point <- mean(vals)
vals_df <- as.data.frame(vals)
answer <- get_total_distance(vals_df, round(mean_point))


# part 2 is harder. I thought the optimum point must now be the mean, but this gave the wrong answer
# can't think of any smart way to do it so let's brute force it :)

max_val <- max(vals_df$vals)

for(i in 1:max_val){
  if(i == 1){
    best_dist <- get_total_distance(vals_df, i)
  }else{
    this_dist <- get_total_distance(vals_df, i)
    if(this_dist < best_dist){
      best_dist <- this_dist
      best_i <- i
    }
  }
}

answer <- best_dist


# Turns out that the mean was the correct point to evaluate from, but it should have been rounded down
# I suppose the curve must be asymmetric about the mean - oh well!