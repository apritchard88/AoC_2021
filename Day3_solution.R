library(tidyverse)

Day3_data <- read_delim("Day3_data.txt", delim = " ", col_names = FALSE)

### Part 1

# transform the data so we have a column per bit
data_sep <- Day3_data %>%
  separate(X1,
           into = c("A","B","C","D","E","F","G","H","I","J","K","L"),
           sep = 1:11,
           remove = FALSE) %>%
  # move the full string to the end (will be helpful later)
  select(A:L, X1)

# use pivot_longer and group_by to find the most common value for each bit position
bit_summary <- data_sep %>%
  mutate_all(as.numeric) %>%
  pivot_longer(cols = 1:12, names_to = "bit_num", values_to = "value") %>%
  group_by(bit_num) %>%
  summarise(MCV = round(mean(value))) %>%
  # now use pivot_wider and unite to turn the most common bit values into a string
  pivot_wider(names_from = bit_num, values_from = MCV) %>%
  unite(bit_gamma, 1:12, sep="") %>%
  mutate(bit_epsilon = 111111111111 - as.numeric(bit_gamma), # epsilon is 111111111111 - the most common value bit string
         gamma_val = strtoi(bit_gamma, base = 2), # converts the bit string into decimal
         epsilon_val = strtoi(bit_epsilon, base = 2),
         multiplied = gamma_val*epsilon_val)

# answer to part 1 is this 'multiplied' entry

### Part 2

# function to find either the most common or least common value in the first column
find_MCV_LCV <- function(data, MCV = TRUE){
  
  column_name <- names(data[1])
  counts <- data %>%
    group_by(!!as.name(column_name)) %>%
    summarise(n = n())
  
  ### order to sort counts depends which item we are after
  # if MCV, we prioritise bit val = 1 if count is same, so sort that column descending as secondary sort
  # opposite is true if not MCV
  if(MCV == TRUE){
    
    counts <- counts %>%
      arrange(desc(n), desc(!!as.name(column_name)))
    
  }else{
    
    counts <- counts %>%
      arrange(n, !!as.name(column_name))
    
  }
  
  # the return value is the bit value in position [1,1] in the counts data (the first value in the column being grouped)
  return_val <- counts[[column_name]][1]
  
  return(return_val)
}

# function to systematically reduce the input data
reduce_data <- function(data, MCV = TRUE){
  
  rows <- nrow(data)
  cols <- ncol(data)
  # while there is more than 1 row (so we terminate when left with only a single answer)
  while(rows > 1){
    # if there is more than one column remaining (ie not just the bit string)
    if(cols > 1){
      
      # get the first column of the remaining data, and then find the desired value to keep
      next_col <- names(data[1])
      col_val <- find_MCV_LCV(data, MCV)
      print(col_val)
      # filter out any rows where the first column is not equal to the desired number, then drop the first column
      data <- data %>% 
        filter(!!as.name(next_col) == as.character(col_val)) %>%
        select(-(!!as.name(next_col)))
      rows <- nrow(data)
      cols <- ncol(data)
      
    }else(break)
    
  }
  
  return(data)
  
}

data_reduce <- reduce_data(data_sep, MCV = TRUE)
data_reduce_LCV <- reduce_data(data_sep, MCV = FALSE)

# Now combine the two results and manipulate to get the answer
results <- data.frame(O2_gen = data_reduce[["X1"]], CO2_scrub = data_reduce_LCV[["X1"]])
final_results <- results %>% mutate(O2_val = strtoi(O2_gen, base = 2),
                   CO2_val = strtoi(CO2_scrub, base = 2),
                   multiplied = O2_val*CO2_val)

# answer is 4406844