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
