# this one was a total pain and I only got to a solution following some trawling of 
# the relevant reddit threads and further examination of the starting data instructions
# Only some parts of the input are actually relevant

# Of note:
# This is apparently an example of pushing and popping numbers from a stack
# z is basically a base 26 value that allows storing and retrieving other values
# Reading through the thread I realised the easiest solution is to reverse that stack
# and find the relevant combinations that way

library(tidyverse)

data_day24 <- readLines("Day24_data.txt")

inst <- c()
var1 <- c()
var2 <- c()

for(line in data_day24){
  inst <- append(inst, str_split(line," ")[[1]][1])
  var1 <- append(var1, str_split(line," ")[[1]][2])
  var2 <- append(var2, str_split(line," ")[[1]][3])
}

# instructions are split into 14 sections
# in each section x and y are reset to 0
# only 3 differences between each section too - can we use that?
# 5th line is always z/26 or z/1
# 6th line is always x = >10 (if z/26) or x < 0 (if z/1) (call it x)
# 15th line is always y = some value (call it y)
# when z/1, output z from that section is always z + (26+y+w) (except first time it is just y+w)
# when z/26, x becomes what the most recent y for a z/1 line was (due to x = z%%26)
# then x gets modified by the 'x' for that section, and ultimately we need x=w in those z/26 sections
# (otherwise z keeps growing)
# so we need to keep track of previous levels

starts <- which(inst=='inp')
ends <- c(tail(starts, -1)-2, length(inst))
z_insts <- starts+4
x_insts <- starts+5
y_insts <- starts+15

z_inst_list <- var2[z_insts]
x_inst_list <- var2[x_insts]
y_inst_list <- var2[y_insts]

# found this useful data type that has pushing and popping implemented already which is what we need
nums <- collections::stack()
# need a vector of lists to store the possible input digits for each section
possibilities <- vector("list", 14)

for(i in 1:length(z_inst_list)){
  this_z_inst <- z_inst_list[i]
  # if this z inst is 1 then we are storing the y value for this section
  if(this_z_inst == "1"){
    this_y_inst <- y_inst_list[i]
    nums$push(c(i, this_y_inst)) # need to store a vector of the digit number and the y value
  }
  else{
    prev_val <- nums$pop()
    prev_loc <- as.numeric(prev_val[1]) # this is whatever i was when this value was stored
    prev_y <- as.numeric(prev_val[2]) # whatever the y value was then
    this_x_inst <- as.numeric(x_inst_list[i])
    # the x value in this section will be the x + the previous y + the previous w
    # we need to ensure (inp_prev + y_prev + x_this = inp_this)
    # this causes x to reduce to 0 and ultimately z to do the same
    val_mod <- prev_y + this_x_inst
    # the allowed input values for that previous step are the valid values that could give this w
    # ie inp_previous is constrained to be 1-9
    prev_possible_input <- which((1:9+val_mod) %in% 1:9) 
    possibilities[[prev_loc]] <- prev_possible_input
    # the allowed input values for this step are then those previous allowed + value modifier
    # ie this ensures the x = w condition would be true
    this_possible_input <- prev_possible_input + val_mod
    possibilities[[i]] <- this_possible_input
  }
}

out_val <- ""
for(i in 1:length(possibilities)){
  dig_max <- max(possibilities[[i]])
  out_val <- paste0(out_val,dig_max)
}
answer_pt1 <- out_val

# for part 2 we instead want the minimum allowed value
out_val <- ""
for(i in 1:length(possibilities)){
  dig_min <- min(possibilities[[i]])
  out_val <- paste0(out_val,dig_min)
}
answer_pt2 <- out_val
print(answer_pt2)


### the following was my initial attempt that would be trying every possibility
# quickly realised that was unfeasible...

code_val <- as.character(11111111111111)

run_program <- function(code_val){
  w<-0
  x<-0
  y<-0
  z<-0
  inp_char<-1
  for(i in 1:length(inst)){
    
    if(inst[i]=="inp"){
      next_num <- substring(code_val,inp_char,inp_char)
      eval(parse(text=paste0(var1[i],"<-",next_num)))
      inp_char <- inp_char + 1
    }else if(inst[i]=="add"){
      eval(parse(text=paste0(var1[i],"<-",var1[i],"+",var2[i])))
    }else if(inst[i]=="mul"){
      eval(parse(text=paste0(var1[i],"<-",var1[i],"*",var2[i])))
    }else if(inst[i]=="div"){
      eval(parse(text=paste0(var1[i],"<-floor(",var1[i],"/",var2[i],")")))
    }else if(inst[i]=="mod"){
      eval(parse(text=paste0(var1[i],"<-",var1[i],"%%",var2[i])))
    }else if(inst[i]=="eql"){
      eval(parse(text=paste0(var1[i],"<-ifelse(",var1[i],"==",var2[i],",1,0)")))
    }
  }
  return(z)
}


