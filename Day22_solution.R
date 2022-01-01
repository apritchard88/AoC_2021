library(tidyverse)

data_day22 <- readLines("Day22_data.txt")

# function to make a data frame with all squares within a defined cube
get_df <- function(line){
  x <- str_split(line, ",")[[1]][1]
  y <- str_split(line, ",")[[1]][2]
  z <- str_split(line, ",")[[1]][3]
        
  xmin <- as.numeric(str_split(str_split(x, "\\.\\.")[[1]][1], "=")[[1]][2])
  xmax <- as.numeric(str_split(x, "\\.\\.")[[1]][2])
  ymin <- as.numeric(str_split(str_split(y, "\\.\\.")[[1]][1], "=")[[1]][2])
  ymax <- as.numeric(str_split(y, "\\.\\.")[[1]][2])
  zmin <- as.numeric(str_split(str_split(z, "\\.\\.")[[1]][1], "=")[[1]][2])
  zmax <- as.numeric(str_split(z, "\\.\\.")[[1]][2])
  
  xrange <- xmin:xmax
  yrange <- ymin:ymax
  zrange <- zmin:zmax
  df <- expand.grid(x = xrange, y = yrange, z = zrange)
  return(df)
}

# loop through the instruction lines for first section (first 20 lines)
# if line instruction is on, full join the resulting df
# if line instruction is off, anti join the resulting df
# remaining rows will be the answer
for(line_num in 1:20){
  inst_line <- data_day22[line_num]
  on_off <- str_split(inst_line, ' ')[[1]][1]
  line <- str_split(inst_line, ' ')[[1]][2]
  this_df <- get_df(line)
  if(line_num == 1){
    out_df <- this_df
  }
  if(on_off == "on"){
    out_df <- out_df %>% 
      full_join(this_df, by = c('x','y','z')) %>%
      select(x,y,z)
  }else{
    out_df <- out_df %>% 
      anti_join(this_df, by = c('x','y','z')) %>%
      select(x,y,z)
  }
}

answer_pt1 <- nrow(out_df)
print(answer_pt1)

# it was correct. but the example part 2 answer is 2e15 so let's assume that method won't work for part 2 :(

# start by getting all the coords into a usable format
xmins <- c()
xmaxs <- c()
xs <- c()
ymins <- c()
ymaxs <- c()
ys <- c()
zmins <- c()
zmaxs <- c()
zs <- c()
on_offs <- c()
for(i in 1:length(data_day22)){
  inst_line <- data_day22[i]
  on_off <- str_split(inst_line, ' ')[[1]][1]
  line <- str_split(inst_line, ' ')[[1]][2]
  x <- str_split(line, ",")[[1]][1]
  y <- str_split(line, ",")[[1]][2]
  z <- str_split(line, ",")[[1]][3]
  
  xmin <- as.numeric(str_split(str_split(x, "\\.\\.")[[1]][1], "=")[[1]][2])
  xmax <- as.numeric(str_split(x, "\\.\\.")[[1]][2])
  ymin <- as.numeric(str_split(str_split(y, "\\.\\.")[[1]][1], "=")[[1]][2])
  ymax <- as.numeric(str_split(y, "\\.\\.")[[1]][2])
  zmin <- as.numeric(str_split(str_split(z, "\\.\\.")[[1]][1], "=")[[1]][2])
  zmax <- as.numeric(str_split(z, "\\.\\.")[[1]][2])
  
  on_offs <- append(on_offs, on_off)
  xmins <- append(xmins, xmin)
  xmaxs <- append(xmaxs, xmax)
  xs <- append(xs, c(xmin, xmax+1)) # +1 to allow distinct cubes (ie no min/max overlap)
  ymins <- append(ymins, ymin)
  ymaxs <- append(ymaxs, ymax)
  ys <- append(ys, c(ymin, ymax+1))
  zmins <- append(zmins, zmin)
  zmaxs <- append(zmaxs, zmax)
  zs <- append(zs, c(zmin, zmax+1))
}

coords_df <- data.frame(on_off = on_offs,
                        xmin = xmins,
                        xmax = xmaxs,
                        ymin = ymins,
                        ymax = ymaxs,
                        zmin = zmins,
                        zmax = zmaxs)

check_overlap <- function(cube_1, cube_2){
  x_overlap <- FALSE
  y_overlap <- FALSE
  z_overlap <- FALSE
  overlap <- FALSE
  cube_overlap <- 0
  if(cube_1$xmin < cube_2$xmax & cube_1$xmax > cube_2$xmin){
    x_overlap <- TRUE
    if(cube_1$xmax <= cube_2$xmax & cube_1$xmin <= cube_2$xmin){
      x_tot <- cube_1$xmax - cube_2$xmin
    }else if(cube_2$xmax <= cube_1$xmax & cube_2$xmin <= cube_1$xmin){
      x_tot <- cube_2$xmax - cube_1$xmin
    }else if(cube_1$xmax <= cube_2$xmax & cube_1$xmin >= cube_2$xmin){ # cube_1 is contained in cube_2
      x_tot <- cube_1$xmax - cube_1$xmin
    }else if(cube_2$xmax <= cube_1$xmax & cube_2$xmin >= cube_1$xmin){ # cube_2 is contained in cube_1
      x_tot <- cube_2$xmax - cube_2$xmin
    }
  }
  if(cube_1$ymin < cube_2$ymax & cube_1$ymax > cube_2$ymin){
    y_overlap <- TRUE
    if(cube_1$ymax <= cube_2$ymax & cube_1$ymin <= cube_2$ymin){
      y_tot <- cube_1$ymax - cube_2$ymin
    }else if(cube_2$ymax <= cube_1$ymax & cube_2$ymin <= cube_1$ymin){
      y_tot <- cube_2$ymax - cube_1$ymin
    }else if(cube_1$ymax <= cube_2$ymax & cube_1$ymin >= cube_2$ymin){ # cube_1 is contained in cube_2
      y_tot <- cube_1$ymax - cube_1$ymin
    }else if(cube_2$ymax <= cube_1$ymax & cube_2$ymin >= cube_1$ymin){ # cube_2 is contained in cube_1
      y_tot <- cube_2$ymax - cube_2$ymin
    }
  }
  if(cube_1$zmin < cube_2$zmax & cube_1$zmax > cube_2$zmin){
    z_overlap <- TRUE
    if(cube_1$zmax <= cube_2$zmax & cube_1$zmin <= cube_2$zmin){
      z_tot <- cube_1$zmax - cube_2$zmin
    }else if(cube_2$zmax <= cube_1$zmax & cube_2$zmin <= cube_1$zmin){
      z_tot <- cube_2$zmax - cube_1$zmin
    }else if(cube_1$zmax <= cube_2$zmax & cube_1$zmin >= cube_2$zmin){ # cube_1 is contained in cube_2
      z_tot <- cube_1$zmax - cube_1$zmin
    }else if(cube_2$zmax <= cube_1$zmax & cube_2$zmin >= cube_1$zmin){ # cube_2 is contained in cube_1
      z_tot <- cube_2$zmax - cube_2$zmin
    }
  }
  if(x_overlap & y_overlap & z_overlap){
    overlap <- TRUE
    cube_overlap <- x_tot*y_tot*z_tot
  }
  return(cube_overlap)
}

count_cubes <- function(cube){
  xspan <- cube$xmax - cube$xmin
  yspan <- cube$ymax - cube$ymin
  zspan <- cube$zmax - cube$zmin
  vol <- xspan*yspan*zspan
  return(vol)
}

overlaps <- c()
cube_contribution <- c()
for(i in 1:nrow(coords_df)){
  this_row <- coords_df[i,]
  if(this_row$on_off == "on"){
    this_row_cubes <- count_cubes(this_row)
  }else{
    this_row_cubes <- 0
  }
  overlap_rows <- ""
  if(i == 1){
    overlaps <- append(overlaps, overlap_rows)
    cube_contribution <- append(cube_contribution, this_row_cubes)
    next
  }
  for(j in 1:(i-1)){
    check_row <- coords_df[j,]
    is_overlap <- check_overlap(this_row, check_row) # this gives the total overlapping cubes
    if(is_overlap > 0){
      overlap_rows <- paste0(overlap_rows, ",", j)
      if(this_row$on_off == "on"){
        if(check_row$on_off == "on"){
          # if the cubes were already on
          this_row_cubes <- this_row_cubes - is_overlap
        }
      }
      if(this_row$on_off == "off"){
        if(check_row$on_off == "on"){
          # can only turn off cubes that were already on
          this_row_cubes <- is_overlap
        }
      }
    }
  }
  if(this_row$on_off == "off"){
    # if this row turns cubes off, total contribution is negative
    this_row_cubes <- -1*this_row_cubes
  }
  overlaps <- append(overlaps, overlap_rows)
  cube_contribution <- append(cube_contribution, this_row_cubes)
}

### problem is for cubes that were on but were already turned off, can't turn off twice
# how to account for this?
# make another df of the cube overlaps that were turned off?
# can we cover every part of the full area by traversing across all xs, ys, zs?

# sort the xs, ys, zs (which contain all min and max for each coord)
# then we need to check which instructions overlap with the regions
# then take the last of the instructions and apply it
xs_sort <- sort(xs)
xs_sort_next <- lead(xs_sort)
ys_sort <- sort(ys)
ys_sort_next <- lead(ys_sort)
zs_sort <- sort(zs)
zs_sort_next <- lead(zs_sort)

section_totals <- c()

# WARNING: this took an absurd amount of time to run
# However this was my final star and I lost any motivation to improve it after completion :)
for(i in 1:(length(xs_sort)-1)){
  total_cube_count <- 0
  x_start <- xs_sort[i]
  x_end <- xs_sort_next[i]
  print(paste0(i,": Now checking for x = ",x_start, " - ",x_end))
  remain_x <- xmins[(xmins <= x_start)&(xmaxs >= x_start)]
  if(length(remain_x)==0){
    next
  }
  for(j in 1:(length(ys_sort)-1)){
    y_start <- ys_sort[j]
    y_end <- ys_sort_next[j]
    remain_y <- ymins[(xmins <= x_start)&(xmaxs >= x_start)&(ymins <= y_start)&(ymaxs >= y_start)]
    if(length(remain_y)==0){
      next 
    }
    for(k in 1:(length(zs_sort)-1)){
      z_start <- zs_sort[k]
      z_end <- zs_sort_next[k]
      remain_z <- zmins[(xmins <= x_start)&(xmaxs >= x_start)&(ymins <= y_start)&(ymaxs >= y_start)&(zmins <= z_start)&(zmaxs >= z_start)]
      
      if(length(remain_z)==0){
        next
      }
      
      remain_on_off <- on_offs[(xmins <= x_start)&(xmaxs >= x_start)&(ymins <= y_start)&(ymaxs >= y_start)&(zmins <= z_start)&(zmaxs >= z_start)]
      on_or_off <- remain_on_off[length(remain_on_off)] # want the latest instruction 
      # if last instruction was 'on', count up the cubes in this section of the overall cube
      if(on_or_off == "on"){
        section_total_cubes <- ((x_end - x_start)*(y_end - y_start)*(z_end - z_start))
        total_cube_count <- total_cube_count + section_total_cubes
      }
      
    }
    
  }
  section_totals[[i]] <- total_cube_count
  
}
print(total_cube_count)
tots_so_far <- section_totals

# answer (eventually) was 1197308251666843
