library(tidyverse)

day19_data <- readLines("Day19_data.txt")

scanner_starts <- c()
scanner_ends <- c()

for(line_num in 1:length(day19_data)){
  if(str_detect(day19_data[line_num], "---") == TRUE){
    scanner_starts <- append(scanner_starts, line_num+1)
  }else if(day19_data[line_num]==""){
    scanner_ends <- append(scanner_ends, line_num-1)
  }else{
    next
  }
}
scanner_ends <- append(scanner_ends, length(day19_data))

all_scanners <- list()
for(i in 1:length(scanner_starts)){
  scanner <- paste0("scanner_",i)
  all_scanners[scanner] <- as.data.frame(day19_data[scanner_starts[i]:scanner_ends[i]], stringsAsFactors = FALSE)
}

scanner_df_list <- list()
for(i in 1:length(all_scanners)){
  x <- c()
  y <- c()
  z <- c()
  scanner <- paste0("scanner_",i)
  this_scanner <- all_scanners[i][[1]]
  for(row in 1:length(this_scanner)){
    split_coords <- as.numeric(str_split(this_scanner[row], ",")[[1]])
    x <- append(x, split_coords[1])
    y <- append(y, split_coords[2])
    z <- append(z, split_coords[3])
    df <- data.frame(x = x, y = y, z = z)
  }
  scanner_df_list[[scanner]] <- df
}

starting_scanners <- scanner_df_list
# xyz, xzy, yxz, yzx, zxy, zyx -> probably only need first 3, others are same but backwards?
# +++, ++-, +--, +-+, ---, --+, -+-, -++
# can use eval(parse(text="this_df[j,1]+next_df[i,1]")) to build command dynamically

# function to correct the beacon coords for a scanner to the reference frame of scanner 1
correct_scanner <- function(scanner_df, xyz, direction, cor_x, cor_y, cor_z){
  print(paste(cor_x,cor_y,cor_z, collapse=","))
  cor_scan <- scanner_df
  # if + is recorded, requires *-1 (ie need to reverse axis orientation)
  sign_1 <- ifelse(substring(direction,1,1)=='+', '-', '+')
  sign_2 <- ifelse(substring(direction,2,2)=='+', '-', '+')
  sign_3 <- ifelse(substring(direction,3,3)=='+', '-', '+')
  # xyx tells us which column is which coord
  col_1 <- substring(xyz,1,1)
  col_2 <- substring(xyz,2,2)
  col_3 <- substring(xyz,3,3)
  eval(parse(text=paste0("cor_scan$x <- (scanner_df$",col_1,"*",sign_1,"1)+",cor_x)))
  eval(parse(text=paste0("cor_scan$y <- (scanner_df$",col_2,"*",sign_2,"1)+",cor_y)))
  eval(parse(text=paste0("cor_scan$z <- (scanner_df$",col_3,"*",sign_3,"1)+",cor_z)))
  return(cor_scan)
}

# function to check all possible frames relative to an input scanner
find_overlap <- function(this_df, next_df, next_scanner_num){
  all_diffs <- c()
  xdiffs <- c()
  ydiffs <- c()
  zdiffs <- c()
  # scanner can have many orientations relative to the other
  possible_xyz <- c('xyz','xzy','yxz','yzx','zxy','zyx')
  possible_dir <- c('+++', '++-', '+--', '+-+', '---', '--+', '-+-', '-++')
  for(xyz in possible_xyz){
    for(dir in possible_dir){
      for(i in 1:nrow(this_df)){
        for(j in 1:nrow(next_df)){
          xd_str <- paste0("this_df[['x']][i]",substring(dir,1,1),"next_df[['",substring(xyz,1,1),"']][j]")
          yd_str <- paste0("this_df[['y']][i]",substring(dir,2,2),"next_df[['",substring(xyz,2,2),"']][j]")
          zd_str <- paste0("this_df[['z']][i]",substring(dir,3,3),"next_df[['",substring(xyz,3,3),"']][j]")
          xdiff <- eval(parse(text=xd_str))
          ydiff <- eval(parse(text=yd_str))
          zdiff <- eval(parse(text=zd_str))
          
          d <- paste0(xdiff, ",", ydiff, ",", zdiff)
          if(!str_detect(d, 'NA')){
            all_diffs <- append(all_diffs, d)
            xdiffs <- append(xdiffs, xdiff)
            ydiffs <- append(ydiffs, ydiff)
            zdiffs <- append(zdiffs, zdiff)
          }
        }
      }
      overlaps <- as.data.frame(all_diffs, stringsAsFactors = FALSE) 
      
      overlaps[['x']] <- xdiffs
      overlaps[['y']] <- ydiffs
      overlaps[['z']] <- zdiffs
      
      overlaps <- overlaps %>% 
        group_by(all_diffs, x, y, z) %>% 
        summarise(n=n()) %>% 
        arrange(-n) %>%
        filter(n >= 12)
      if(nrow(overlaps)>0){
        overlaps <- overlaps %>%
          ungroup %>%
          select(x,y,z)
        overlaps[['xmap']] <- c(substring(xyz,1,1))
        overlaps[['ymap']] <- c(substring(xyz,2,2))
        overlaps[['zmap']] <- c(substring(xyz,3,3))
        
        # lets record the relative postion of each scanner too
        # should be relative to (0,0,0) since the code works in a way that the first df
        # of the two being checked should already have been corrected
        scanner_num <<- append(scanner_num, next_scanner_num)
        scanner_x <<- append(scanner_x, overlaps$x)
        scanner_y <<- append(scanner_y, overlaps$y)
        scanner_z <<- append(scanner_z, overlaps$z)
        
        cor_df <- correct_scanner(next_df, xyz, dir, overlaps$x, overlaps$y, overlaps$z)
        return(cor_df)
      }else{
        all_diffs <- c()
        xdiffs <- c()
        ydiffs <- c()
        zdiffs <- c()
      }
    }
  }
  return(NA)
}

# empty vectors to hold scanner coords from function call
scanner_num <- c()
scanner_x <- c()
scanner_y <- c()
scanner_z <- c()

# function to loop through all scanners, find overlaps, correct coords to same reference frame as scanner 1
correct_scanner_list <- function(scanner_list, block_list){
  print("-----NEXT FUNCTION CALL-----")
  print(paste0("Will now check scanners: ",scanner_list))
  print(paste0("Corrected so far: ",block_list))
  # when we have no more scanners to check, return block list (can ensure none missing)
  if(length(scanner_list)==0){
    return(block_list)
  }
  new_scanner_list <- c()
  for(val in scanner_list){
    # check all of the other scanners that we haven't yet visited to look for an overlap
      this_df <- scanner_df_list[[val]]
      for(i in 1:length(scanner_df_list)){
        if(i != val & !(i %in% block_list)){
          check_df <- scanner_df_list[[i]]
          correct_overlap_df <- find_overlap(this_df, check_df, i)
          if(!is.na(correct_overlap_df)){
            if(!(i %in% block_list)){
              new_scanner_list <- append(new_scanner_list, i) # add this i to the list to check next pass
              block_list <- append(block_list, i) # also add this i to the block list, since it is now corrected
              
            }
            scanner_df_list[[i]] <<- correct_overlap_df
          }
        }
    }
  }
  # now call this function again with updated scanner list and block list
  correct_scanner_list(new_scanner_list, block_list)
}

# initial scanner list is 1
# initial blocker list is just 1 (only scanner not to check)
scanner_list <- c(1)
block_list <- c(1)

# this takes a LONG time to run, so I assume there are much smarter ways 
correct_scanner_list(scanner_list, block_list)

full_scanner_list <- scanner_df_list[[1]]
for(i in 1:(length(scanner_df_list)-1)){
  full_scanner_list <- rbind(full_scanner_list, scanner_df_list[[i+1]])
}
full_scanner_list <- distinct(full_scanner_list)
answer_pt1 <- nrow(full_scanner_list)
answer_pt1
# answer was 467

# create a data frame of the scanner positions relative to (0,0,0)
scanner_pos_df <- data.frame(scanner = scanner_num, x = scanner_x, y = scanner_y, z = scanner_z)

# for part 2 we need to get the maximum separation between scanners
# probably quickest to just loop through and find it now we have all the positions relative to scanner 0
calc_dist <- function(beacon1, beacon2){
  dist <- abs(beacon1$x - beacon2$x) + abs(beacon1$y - beacon2$y) + abs(beacon1$z - beacon2$z)
  return(dist)
}

max_dist <- 0
b1_max <- 0
b2_max <- 0
# loop through the scanner combinations, calculate manhattan distance
# record the highest separation
for(i in 1:nrow(scanner_pos_df)){
  for(j in 1:nrow(scanner_pos_df)){
    if(i != j){
      b1 <- scanner_pos_df[i,]
      b2 <- scanner_pos_df[j,]
      dist_b1b2 <- calc_dist(b1,b2)
      if(dist_b1b2 > max_dist){
        max_dist <- dist_b1b2
        b1_max <- i
        b2_max <- j
      }
    }
  }
}
answer_pt2 <- max_dist
print(answer_pt2)
# answer was 12226