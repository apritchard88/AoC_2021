library(tidyverse)

data_day16 <- readLines("Day16_data.txt")

conv_table <- c("0000",
                "0001",
                "0010",
                "0011",
                "0100",
                "0101",
                "0110",
                "0111",
                "1000",
                "1001",
                "1010",
                "1011",
                "1100",
                "1101",
                "1110",
                "1111")

names(conv_table) <- c("0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F")

# for part 2 also need a lookup for type number
conv_type <- c("sum" ,"prod", "min", "max", "greater", "less", "equal")
names(conv_type) <- c("000", "001", "010", "011", "101", "110", "111")

in_length <- nchar(data_day16)
in_string <- ""
for(i in 1:in_length){
  this_char <- substr(data_day16, i, i)
  conv_this_char <- conv_table[this_char][[1]]
  in_string <- paste0(in_string, conv_this_char)
}

length_conv_string <- nchar(conv_string)

# first want to split into packets
packets <- c()

# function to convert binary to decimal
# was using strtoi(x , base=2) but cannot handle the largest values
convert <- function(x) {
  y <- as.numeric(strsplit(x, "")[[1]])
  sum(y * 2^rev((seq_along(y)-1)))
}

# function to parse a literal 
parse_literal <- function(string){
  version <- substr(string, 1, 3)
  type <- substr(string, 4, 6)
  next_bit_start <- 7
  next_bit_end <- 11
  next_bit_flag <- substr(string, next_bit_start, next_bit_start)
  value_str <- substr(string, next_bit_start+1, next_bit_end)
  while(next_bit_flag == 1){
    next_bit_start <- next_bit_start + 5
    next_bit_end <- next_bit_end + 5
    next_bit_flag <- substr(string, next_bit_start, next_bit_start)
    value_str <- paste0(value_str, substr(string, next_bit_start+1, next_bit_end))
  }
  version_d <- strtoi(version, base=2)
  type_d <- strtoi(type, base=2)
  this_literal <- substr(string, 1, next_bit_end)
  value <- convert(value_str)
  string_remaining <- substr(string, next_bit_end+1, nchar(string))
  output <- list("version_val" = version_d,
                 "type_val" = type_d,
                 "this_literal" = this_literal,
                 "literal_value" = value,
                 "string" = string_remaining)
  return(output)
}

conv_len_to_num <- function(string){
  count_sub <- 0
  str_length <- nchar(string)
  while(str_length > 10){
    version_count <- version_count + 1
    version <- substr(conv_string, 1, 3)
    type <- substr(conv_string, 4, 6)
    version_total <- version_total + strtoi(version, base=2)
    if(type=="100"){
      parsed_literal <- parse_literal(string)
      string <- parsed_literal$string
      count_sub <- count_sub + 1
    }else{
      id <- substr(string, 7, 7)
      if(id == "1"){
        string <- substr(string, 19, nchar(string))
      }else{
        string <- substr(string, 23, nchar(string))
      }
      count_sub <- count_sub - 1
    }
    str_length <- nchar(string)
  }
  if(count_sub < 1){
    count_sub <- 1
  }
  return(count_sub)
}

conv_string <- in_string
this_packet <- ""
version_total <- 0
version_count <- 0
interpret_str <- ""
all_packets <- c()

# this while loop will go through the whole string and cut it down based on the instructions
# if type = "100" then use the function above to parse the literal value and return remaining string
# otherwise if id = 0 then cut off the first 18 bits from front of the string 
# if id = 1 cut off the first 22 bits
# add up the value each time
str_length <- nchar(conv_string)
count_to <- 0
start_lit_count <- 1
chunk_counter <- 0
tot_n_sub <- 0

levels_all <- rep(0,40)
names(levels_all) <- c("1","2","3","4","5","6","7","8","9","10"
                       ,"11","12","13","14","15","16","17","18","19","20",
                       "21","22","23","24","25","26","27","28","29","30",
                       "31","32","33","34","35","36","37","38","39","40")

level <- 0

while(str_length > 10){
  version_count <- version_count + 1
  version <- substr(conv_string, 1, 3)
  type <- substr(conv_string, 4, 6)
  version_total <- version_total + strtoi(version, base=2)
  if(type=="100"){
    levels_all[[level]] <- levels_all[level][[1]] - 1
    
    parsed_literal <- parse_literal(conv_string)
    conv_string <- parsed_literal$string
    lit_val <- parsed_literal$literal_value
    packet <- lit_val
    #if(start_lit_count == count_to){
    
    if(levels_all[[level]] == 0){
      #packet <- paste0(packet, ")")
      while(levels_all[[level]] == 0){
        level <- level - 1
        #levels_all[[level]] <- levels_all[level][[1]] - 1
        packet <- paste0(packet, ")")
      }
      packet <- paste0(packet, ",")
    }else{
      packet <- paste0(lit_val, ", ")
    }
    
    start_lit_count <- start_lit_count + 1
    interpret_str <- paste0(interpret_str, packet)
    all_packets[[version_count]] <- packet
    chunk_counter <- chunk_counter - 1
  }else{
    type_val <- strtoi(type,base=2)
    type_str <- conv_type[type]
    id <- substr(conv_string, 7, 7)
    start_lit_count <- 1
    if(id == "1"){
      n_sub <- strtoi(substr(conv_string, 8, 18), base=2)
      conv_string <- substr(conv_string, 19, nchar(conv_string))
      packet <- paste0(type_str,"(")
      interpret_str <- paste0(interpret_str, packet)
      all_packets[[version_count]] <- packet
      count_to <- n_sub
    }else{
      l_sub <- strtoi(substr(conv_string, 8, 22), base=2)
      conv_string <- substr(conv_string, 23, nchar(conv_string))
      chunk_string <- substr(conv_string, 23, (22+l_sub))
      n_sub <- conv_len_to_num(chunk_string)
      packet <- paste0(type_str,"(")
      interpret_str <- paste0(interpret_str,  packet)
      all_packets[[version_count]] <- packet
      count_to <- n_sub
    }
    
    level <- level+1
    levels_all[level] <- levels_all[level][[1]] + n_sub
    
    tot_n_sub <- tot_n_sub + n_sub
    if(!version_count==1){
      if(chunk_counter==0){
        chunk_counter <- chunk_counter + (n_sub)
      }else{
        chunk_counter <- chunk_counter + (n_sub-1)
      }
      
      
    }
  }
  str_length <- nchar(conv_string)
  if(chunk_counter==0){
    packet <- paste0(packet, "\n\n")
  }
}

cat(all_packets)

equal <- function(x,y){
  if(x==y){
    return(1)
  }else{return(0)}
}

greater <- function(x,y){
  if(x>y){
    return(1)
  }else{return(0)}
}

less <- function(x,y){
  if(x<y){
    return(1)
  }else{return(0)}
}

final_str <- str_c(all_packets, collapse='')
