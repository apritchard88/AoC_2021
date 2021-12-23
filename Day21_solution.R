library(tidyverse)

start_p1 <- 5
start_p2 <- 6

squares <- 1:10

dice_rolls <- 1:100

rolls <- 1
turn <- 1
p1_total <- 0
p2_total <- 0
p1_pos <- start_p1
p2_pos <- start_p2

target <- 1000

while(p1_total<target & p2_total<target){
  r1 <- rolls
  rolls <- rolls + 1
  r2 <- rolls
  rolls <- rolls + 1
  r3 <- rolls
  rolls <- rolls + 1
  if(r1 > 100){
    r1 <- (r1%%100)
  }
  if(r2 > 100){
    r2 <- (r2%%100)
  }
  if(r3 > 100){
    r3 <- (r3%%100)
  }
  squares_moved <- sum(r1,r2,r3)
  if(turn%%2 == 1){
    p1_square <- p1_pos + squares_moved
    if(p1_square > 10){
      p1_pos <- p1_square %% 10
      if(p1_pos == 0){
        p1_pos <- 10
      }
    }else{
      p1_pos <- p1_square
    }
    p1_total <- p1_total + p1_pos
    #if(rolls > 980 & rolls < 1000){
    #  print(paste0('Player 1: rolls ', r1,' ', r2, ' ',r3, ' square = ', p1_square, ' total = ',p1_total))
    #}
    
  }else{
    p2_square <- p2_pos + squares_moved
    if(p2_square > 10){
      p2_pos <- p2_square %% 10
      if(p2_pos == 0){
        p2_pos <- 10
      }
    }else{
      p2_pos <- p2_square
    }
    p2_total <- p2_total + p2_pos
    print(paste0('Player 2: ',p2_total))
  }
  
  turn <- turn+1
}

rolls <- rolls - 1 # to find the final roll of the die
if(p1_total >= 1000){
  answer_pt1 <- p2_total*rolls
}else{
  answer_pt1 <- p1_total*rolls
}
print(answer_pt1)



### part 2
# don't know where to start with this...
# had a little look at the reddit thread for some ideas
# led to recursion and memoisation
target <- 21
results <- c()
names(results) <- c()
# memory will become a list of all possible states that we encounter, along with the number of wins from that state
# the idea is that it serves as a cache of the states and outcomes we have already encountered
# therefore if we end up back at that state we don't need to recalculate - just use the previous answer
# this cache idea is called memoisation (not actually sure I did it right, but it works well)
memory <- list()
next_turn <- function(p1_pos, p2_pos, p1_score, p2_score, turn){
  # each state is uniquely identified by this name
  this_state <- paste(p1_pos, p2_pos, p1_score, p2_score, turn, sep = ",")
  # if there is a winner, return 1 or 0 depending on which player we are counting for
  # need to run twice - change which player we are counting the wins of here
  if(p1_score >= target){
    win <- 1
    return(win)
  }
  if(p2_score >= target){
    win <- 0
    return(win)
  }
  key <- this_state
  # this is the point at which we look if we were in this state before - reuse results if so
  if(key %in% names(memory)){
    return(memory[[key]])
  }
  # otherwise we are in an unvisited state and need to calculate wins from scratch
  results <- 0
  # roll the dice 3 times and find the sum
  # need to do it for all combinations
  for(d1 in c(1,2,3)){
    for(d2 in c(1,2,3)){
      for(d3 in c(1,2,3)){
        this_roll <- sum(d1,d2,d3)
        # if it is player 1's turn
        if(turn == 1){
          new_p1_pos <- (p1_pos + this_roll)%%10 # new position after adding the roll
          if(new_p1_pos == 0){new_p1_pos <- 10} # if we have 0, actual space is 10
          new_p1_score <- p1_score + new_p1_pos # calculate new score
          # now we use recursion - call the function from inside the function
          # we call this function with the updates game state
          # this will happen repeatedly until winners are returned
          result <- next_turn(new_p1_pos, p2_pos, new_p1_score, p2_score, 2)
          # after we grab the results, add them up for this state
          results <- results + result
        # repeat the scenario but for player 2's turns
        }else{
          new_p2_pos <- (p2_pos + this_roll)%%10
          if(new_p2_pos == 0){new_p2_pos <- 10}
          new_p2_score <- p2_score + new_p2_pos
          result <- next_turn(p1_pos, new_p2_pos, p1_score, new_p2_score, 1)
          results <- results + result
        }
        
      }
    }
  }
  # now we fill in the cache with the results from this game state
  # use global assignment <<- as the list is outside the function and must be modified each time
  memory[[key]] <<- results
  
  # return the results from this state
  # ultimate return to console will be for the input state
  return(memory[[key]])
}
# just call the function with the desired state, answer will print to screen
next_turn(start_p1, start_p2, 0, 0, 1)

# player 1 wins = 919758187195363
# player 2 wins = 635572886949720