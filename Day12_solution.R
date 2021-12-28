library(tidyverse)
library(igraph)

day12_data <- read_delim("Day12_data.txt", delim = "-", col_names = FALSE)

g <- graph_from_data_frame(day12_data, directed = FALSE)
plot(g)

all_vertices <- get.vertex.attribute(g, "name")
lower_list <- c()
upper_list <- c()
for(v in all_vertices){
  lower <- tolower(v)
  if(v == lower){
    lower_list <- append(lower_list, v)
  }else{
    upper_list <- append(upper_list, v)
  }
}

create_new_upper_df <- function(df, val, rep){
  df_new <- df %>% 
    filter(X1==val | X2==val) %>% 
    mutate(X1 = if_else(X1==val, paste0(val,rep), X1), 
           X2 = if_else(X2==val, paste0(val,rep), X2))
  return(df_new)
}

new_data <- day12_data
added_vertices <- c()
for(vertex in all_vertices){
  if(!(vertex %in% lower_list)){
    print(vertex)
    v_neighbours <- neighbors(g, vertex)
    count_new_v <- 0
    for(i in 1:length(v_neighbours)){
      if(!(v_neighbours[i]$name == "end")){
        count_new_v <- count_new_v + 1
        if(count_new_v>1){
          new_df <- create_new_upper_df(day12_data, vertex, count_new_v)
          new_data <- rbind(new_data, new_df)
          added_v <- paste0(vertex,count_new_v)
          added_vertices <- append(added_vertices, added_v)
        }
        
      }
    }
  }
}

new_g <- graph_from_data_frame(new_data, directed = FALSE)
#plot(new_g)
paths <- all_simple_paths(new_g,'start','end')
#simple_paths <- all_simple_paths(g,'start','end')

actual_paths <- c()
count <- 1
for(path in paths){
  out_path <- c()
  for(i in 1:length(path)){
    next_step <- path[i]$name
    if(next_step %in% added_vertices){
      next_step <- substring(next_step, 1, nchar(next_step)-1)
    }
    out_path <- append(out_path, next_step)
  }
  collapse_path <- str_c(out_path, collapse = '')
  if(!(collapse_path %in% actual_paths)){
    actual_paths <- append(actual_paths, collapse_path)
    count <- count+1
  }
}

answer_part1 <- length(actual_paths)

#actual_paths_df <- as.data.frame(actual_paths)
#distinct_paths <- actual_paths_df %>%
#  distinct()

# 1) make list of all upper case stages
# 2) add additional section to df for each component of each upper case section that isn't 'end'
# 3) create a list of each added section name (ie HN2, HN3)
# 4) find all simple paths
# 5) loop through paths and change eg HN2 back to derivative
# 6) collapse path to a string and output
# 7) change the output to a data frame and use distinct() to find all distinct paths

# part 2 needs a different approach
# try to record all paths recursively?

# for part 2 we have the option to visit one small cave twice
# this function is recursive, finds next node and counts all those that end in 'end'
count_paths <- function(start_node, visited, doubled){
  # if the node is 'end' we found a path - return 1 for the count
  if(start_node == "end"){
    return(1)
  }
  path_count <- 0 # set the path count to 0 to start for the next loop
  next_nodes <- neighbors(g,start_node) # get a list of adjoining nodes from the graph
  # now loop through the next nodes
  for(i in 1:length(next_nodes)){
    next_node <- next_nodes[i]$name
    # if the next node is 'start' we cannot visit it
    if(next_node == 'start'){
      next
    }else if((next_node %in% visited) & doubled){
      # if the next node is in the lower case visited list and we already went to one twice, we cannot visit it
      next
    }else{
      # track the lower case nodes we have already visited
      if(start_node %in% lower_list){
        visited <- append(visited, start_node)
      }
      # if the next node is lower case and has been visited, change 'doubled' bool to TRUE for rest of path
      # this stops the path visiting any more small caves a second time
      if((next_node %in% visited)){
        path_count <- path_count + count_paths(next_node, visited, TRUE)
      }else{
        # otherwise we keep the doubled variable as it is
        path_count <- path_count + count_paths(next_node, visited, doubled)
      }
    }
  }
  return(path_count)
}

visited_nodes <- c()
answer_pt2 <- count_paths('start',visited_nodes, FALSE)
