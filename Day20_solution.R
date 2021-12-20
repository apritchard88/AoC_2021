library(tidyverse)

day20_data <- readLines("Day20_practice.txt")

scanner_starts <- c()
scanner_ends <- c()

for(line_num in 1:length(day20_data)){
  if(str_detect(day20_data[line_num], "---") == TRUE){
    scanner_starts <- append(scanner_starts, line_num+1)
  }else if(day20_data[line_num]==""){
    scanner_ends <- append(scanner_ends, line_num-1)
  }else{
    next
  }
}
scanner_ends <- append(scanner_ends, length(day20_data))

all_scanners <- list()
for(i in 1:length(scanner_starts)){
  scanner <- paste0("scanner_",i)
  all_scanners[scanner] <- as.data.frame(day20_data[scanner_starts[i]:scanner_ends[i]], stringsAsFactors = FALSE)
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

this_df <- scanner_df_list[[1]]
next_df <- scanner_df_list[[2]]

#this_df$x <- as.numeric(as.character(this_df$x))
#this_df$y <- as.numeric(as.character(this_df$y))
#this_df$z <- as.numeric(as.character(this_df$z))
#this_df$x2 <- as.numeric(as.character(next_df$x))
#this_df$y2 <- as.numeric(as.character(next_df$y))
#this_df$z2 <- as.numeric(as.character(next_df$z))

#this_df %>% rowwise() %>% mutate(xd = as.numeric(x2)-as.numeric(x),
#                   yd = y2-y,
#                   zd = z2-z)

all_diffs <- c()
for(i in 1:nrow(this_df)){
  for(j in 1:nrow(next_df)){
    xdiff <- this_df[j,1] + next_df[i,1]
    ydiff <- this_df[j,2] - next_df[i,2]
    zdiff <- this_df[j,3] + next_df[i,3]
    
    d <- paste0(xdiff, ",", ydiff, ",", zdiff)
    if(!str_detect(d, 'NA')){
      all_diffs <- append(all_diffs, d)
    }
    
  }
}

as.data.frame(all_diffs, stringsAsFactors = FALSE) %>% 
  group_by(all_diffs) %>% 
  summarise(n=n()) %>% 
  arrange(-n)


# xyz, xzy, yxz, yzx, zxy, zyx -> probably only need first 3, others are same but backwards?
# +++, ++-, +--, +-+, ---, --+, -+-, -++
# can use eval(parse(text="this_df[j,1]+next_df[i,1]")) to build command dynamically

this_df <- scanner_df_list[[1]]
next_df <- scanner_df_list[[2]]

find_overlap <- function(this_df, next_df){
  all_diffs <- c()
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
          }
        }
      }
      overlaps <- as.data.frame(all_diffs, stringsAsFactors = FALSE) %>% 
        group_by(all_diffs) %>% 
        summarise(n=n()) %>% 
        arrange(-n) %>%
        filter(n >= 12)
      if(nrow(overlaps)>0){
        print(xyz)
        print(dir)
        print(overlaps)
        return(overlaps)
      }else{
        all_diffs <- c()
      }
    }
  }
  return(NA)
}

this_df <- scanner_df_list[[5]]
next_df <- scanner_df_list[[1]]
find_overlap(this_df,next_df)


### add individual columns for x y z
# name according to the xyz ordering that found the overlap
# add columns showing the 2 dfs that were compared
# check overlap of 1 df with all others
# only need to do this one way (ie dont compare 1vs2 and 2vs1)