library(tidyverse)

day14_data <- readLines("Day14_data.txt")

# read through each line in the input
# put it into the instructions list if it contains the "->" characters
instructions <- c()
for(line in day14_data){
  if(str_detect(line, "->")){
    print(line)
    instructions <- append(instructions, line)
  }
}

# the starting polymer chain is the first line of the input file
start_polymer <- day14_data[1]
initial_polymer <- start_polymer

# create a data frame from the insertion instructions by splitting lines at the " -> " characters
inst <- sapply(instructions, strsplit, split=" -> ")
inst_df <- data.frame(t(sapply(inst,c)))
rownames(inst_df) <- 1: nrow(inst_df)

pattern <- inst_df$X1
insertion <- inst_df$X2
# now change the names of the insertions to the corresponding string we are looking for
names(insertion) <- pattern

times <- 8
for(i in 1:times){
  new_string <- ""
  for(c in 1:nchar(start_polymer)){
    if(c < nchar(start_polymer)){
      this_pair <- substr(start_polymer, c, c+1)
      char_1 <- substr(start_polymer, c, c)
      char_2 <- substr(start_polymer, c+1, c+1)
      insert_char <- as.character(insertion[this_pair][[1]])
      # if the out string is currently empty, need to add first character too
      # otherwise only add the new character and second character (due to overlaps)
      if(new_string == ""){
        new_string <- paste0(new_string, char_1, insert_char, char_2)
      }else{
        new_string <- paste0(new_string, insert_char, char_2)
      }
      
    }
    
  }
  start_polymer <- new_string
}

# now we have the final polymer string
# split it into individual characters and count how many times each occurs
polymer_split <- strsplit(start_polymer, "")[[1]]
unique_chars <- unique(polymer_split)
totals <- c()
for(char in unique_chars){
  total <- sum(polymer_split == char)
  totals <- append(totals, total)
}

# answer is just the max count - min count
answer_part1 <- max(totals) - min(totals)

# how long do we expect string to be after 40 iterations?
start_length <- nchar(initial_polymer)
steps <- 10
for(i in 1:steps){
  total_length <- (start_length*2)-1
  start_length <- total_length
}

# it is huge - need a different method to solve part 2 :(
# make a df with the results pair of pairs from each starting pair
# eg CH -> B means CH -> CB BH
new_pairs_df <- inst_df %>%
  rowwise() %>%
  # create a new column for each of the two new pairs that will be created from 'parent' pair
  mutate(pair_1 = paste0(str_split(X1, "")[[1]][1], X2),
         pair_2 = paste0(X2, str_split(X1, "")[[1]][2]))

# lookup tables to get the 2 new pairs corresponding to the start pair
lookup_1 <- new_pairs_df$pair_1
lookup_2 <- new_pairs_df$pair_2
pair_list <- new_pairs_df$X1
names(lookup_1) <- new_pairs_df$X1
names(lookup_2) <- new_pairs_df$X1

# initial pair vector count is all 0
counts <- rep(0, nrow(new_pairs_df))
names(counts) <- new_pairs_df$X1

for(i in 1:(nchar(initial_polymer)-1)){
  this_pair <- substr(initial_polymer, i, i+1)
  print(this_pair)
  counts[this_pair] <- counts[this_pair] + 1
}

steps <- 40 # how many iterations

for(i in 1:steps){
  this_count <- rep(0, nrow(new_pairs_df)) # create a new blank vector each time
  names(this_count) <- new_pairs_df$X1
  # for each possible pair (taken from start list)
  for(pair in pair_list){
    new_pair_1 <- lookup_1[pair][[1]] # get the first pair produced
    new_pair_2 <- lookup_2[pair][[1]] # get the second pair produced
    if(counts[pair][[1]]>0){
      this_count[new_pair_1] <- this_count[new_pair_1][[1]] + counts[pair][[1]] # new count adds on the value for 'parent' pair
      this_count[new_pair_2] <- this_count[new_pair_2][[1]] + counts[pair][[1]]
    }
    
  }
  counts <- this_count # reset counts to be the new version after this stage
}

# now make a data frame from the counts vector for better manipulation and summing
counts_df <- as.data.frame(counts)
counts_df <- tibble::rownames_to_column(counts_df, "Pair")

char_count_df <- counts_df %>%
  rowwise() %>%
  mutate(char_1 = str_split(Pair, "")[[1]][1],
         char_2 = str_split(Pair, "")[[1]][2])

# just need to sum the total count per letter
# just the first character should suffice as otherwise double counting
char_1_count <- char_count_df %>%
  group_by(char_1) %>%
  summarise(total_1 = sum(counts))

# final step is to add on the final character from the start list (since it always lives on the end)
last_char <- substr(initial_polymer, nchar(initial_polymer), nchar(initial_polymer))

# create a named vector of the letter and count
final_count <- char_1_count$total_1
names(final_count) <- char_1_count$char_1
final_count[last_char] <- final_count[last_char] + 1

# need to increase precision before printing the answer
answer <- max(final_count) - min(final_count)
options(digits=20)
answer