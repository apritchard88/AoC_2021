library(tidyverse)
library(ggplot2)

fish_days <- strsplit(readLines("Day6_data.txt"), ",")

# function to iterate the current set of fish by a single day
next_day <- function(day_state){
  
  fish_out <- c()
  for(fish in day_state){
    
    if(fish > 0){
      fish_out <- append(fish_out, fish-1)
    }else{
      fish_out <- append(fish_out, 6)
      fish_out <- append(fish_out, 8)
    }
    
  }
  return(fish_out)
}

tot_days <- 80

# function to find how many total fish result from a single fish with specified start state
fish_after_n_days <- function(fish_state, days){
  
  for(day in 1:days){
    fish_state <- next_day(fish_state)
  }
  
  return(length(fish_state))
  
}

day_map <- c()
fish_state_start <- 1
for(i in 1:tot_days){
  fish_state_start <- next_day(fish_state_start)
  n_fish <- length(fish_state_start)
  day_map <- append(day_map, n_fish)
}



# create a lookup table for each possible starting value so we don't have to do the same thing for each individual fish
possible_fish <- 1:6
total_fish_lookup <- c()
for(fish in possible_fish){
  this_fish_tot_spawn <- fish_after_n_days(fish, tot_days)
  total_fish_lookup <- append(total_fish_lookup, this_fish_tot_spawn)
}

final_fish <- c()
for(fish in fish_days[[1]]){
  fish_pos <- c(as.numeric(fish))
  final_fish <- append(final_fish, total_fish_lookup[fish_pos])
  
}

answer <- sum(final_fish)

# next part wants me to check total numbers after 256 days
# current method won't work as the lists will become absurdly long and memory will die
# need some smarter method

# each day the fish all move down one level
# when a fish is at 0, the next day they all move to 6 and an equivalent number start at 8

# get a starting count of how many fish at each level
start_count <- as.data.frame(fish_days[[1]]) %>% 
  mutate(fish_day_count = fish_days[[1]]) %>%
  group_by(fish_day_count) %>% 
  summarise(n=n()) %>%
  ungroup()

# create a named list to hold the starting positions and number of fish
fish_state <- start_count$n
names(fish_state) <- start_count$fish_day_count
# at the start, no fish occupy 0,6,7,8 - add these manually
fish_state["0"] <- 0
fish_state["6"] <- 0
fish_state["7"] <- 0
fish_state["8"] <- 0

# also need a new state list to fill with the next day positions
new_state <- c(0,0,0,0,0,0,0,0,0)
names(new_state) <- names(fish_state)

tot_days <- 256

# each day, the total fish at each value is whatever the value was one level higher the previous day
# only exception is level 6, which becomes the total of all the fish that were level 7, and all the fish that were level 0
for(i in 1:tot_days){
    new_state["0"] <- fish_state["1"]
    new_state["1"] <- fish_state["2"]
    new_state["2"] <- fish_state["3"]
    new_state["3"] <- fish_state["4"]
    new_state["4"] <- fish_state["5"]
    new_state["5"] <- fish_state["6"]
    new_state["6"] <- fish_state["7"] + fish_state["0"]
    new_state["7"] <- fish_state["8"]
    new_state["8"] <- fish_state["0"]
    
    fish_state <- new_state
    }

options(digits = 20) # need this to print the full precision of the answer

# final answer should be the sum of all the fish at all the levels
sum(fish_state)
