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
  version_total <- 0
  version_count <- 0
  str_length <- nchar(string)
  while(str_length > 10){
    version_count <- version_count + 1
    #count_sub <- count_sub + 1
    version <- substr(string, 1, 3)
    type <- substr(string, 4, 6)
    #print(strtoi(version, base=2))
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
      #count_sub <- count_sub + 1
    }
    str_length <- nchar(string)
  }
  return(version_count)
}

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

deduct_chars <- function(chars){
  for(i in 1:length(levels_char_count)){
    if(levels_char_count[[i]] > 0){
      levels_char_count[[i]] <<- levels_char_count[[i]] - chars
    }
  }
}

### some of previous code is redundant now since I changed method

# not getting very far with this
# coming back to it I will now try using recursion again to solve it
version_count <- 0
version_total <- 0

# this is a recursive function that calculates the value of the next nsub subpackets, 
# or the next l_sub characters
# Need to keep count of the length of a sub packet for those instructions where a length
# is given rather than a number of packets

parse_packets <- function(conv_string, l_sub_packet){
  version_count <<- version_count + 1
  version <- substr(conv_string, 1, 3)
  type <- substr(conv_string, 4, 6)
  l_sub_packet <- l_sub_packet + 6
  version_total <<- version_total + strtoi(version, base=2)
  # if type is 100, this is a literal and we can get the value
  if(type=="100"){
    parsed_literal <- parse_literal(conv_string)
    
    remain_string <- parsed_literal$string
    packet_length <- nchar(parsed_literal$this_literal)
    packet_val <- parsed_literal$literal_value
    print(packet_val)
    l_sub_packet <- l_sub_packet + packet_length - 6 # -6 since 'this_literal' is the whole string including first 6 chars
    
  }else{
    # if type is anything else, need to parse some more
    type_val <- strtoi(type,base=2)
    type_str <- conv_type[type]
    id <- substr(conv_string, 7, 7)
    l_sub_packet <- l_sub_packet + 1
    # if id is 1, we get the number of subpackets to count and use that in a loop
    if(id == "1"){
      n_sub <- strtoi(substr(conv_string, 8, 18), base=2) # how many sub packets
      remain_string <- substr(conv_string, 19, nchar(conv_string)) # remaining string is everything from char 19 onwards
      l_sub_packet <- l_sub_packet + 11
      sub_packet_length <- 0 # to count the length of the next sub packet
      value_list <- c() # list to hold the values of the sub packets
      for(i in 1:n_sub){
        # recursive call to this function with arguments set to whatever remains of the string,
        # and how long the current subpacket is (for l_sub purposes)
        next_packet <- parse_packets(remain_string, sub_packet_length)
        next_value <- next_packet$value
        sub_packet_length <- next_packet$packet_length
        remain_string <- next_packet$remaining
        value_list <- append(value_list, next_value)
      }
      l_sub_packet <- l_sub_packet + sub_packet_length # keep track for any l_sub packets
      
    }else{
      l_sub <- strtoi(substr(conv_string, 8, 22), base=2) # length of sub packet string
      l_sub_packet <- l_sub_packet + 15
      remain_string <- substr(conv_string, 23, nchar(conv_string))
      #char_count <- 0
      sub_packet_length <- 0
      value_list <- c()
      while(sub_packet_length < l_sub){
        # this time we loop through until the subpacket length returned is the l_sub we are aiming for
        next_packet <- parse_packets(remain_string, sub_packet_length)
        next_value <- next_packet$value
        sub_packet_length <- next_packet$packet_length
        remain_string <- next_packet$remaining
        print(paste0("l_sub = ",l_sub, ", sub_length = ", sub_packet_length))
        value_list <- append(value_list, next_value)
      }
      l_sub_packet <- l_sub_packet + sub_packet_length
    }
    # at this point we have the values we need and can make the calculation stipulated by the type
    operation <- type_str[[1]]
    if(operation == "sum"){
      val <- sum(value_list)
    }else if(operation == "prod"){
      val <- prod(value_list)
    }else if(operation == "min"){
      val <- min(value_list)
    }else if(operation == "max"){
      val <- max(value_list)
    }else if(operation == "greater"){
      val <- greater(value_list[1], value_list[2])
    }else if(operation == "less"){
      val <- less(value_list[1], value_list[2])
    }else if(operation == "equal"){
      val <- equal(value_list[1], value_list[2])
    }
    packet_val <- val
    
  }
  # now return the value of this packet, the length of this subpacket, and the remaining string
  return_info <- list(value = packet_val, packet_length = l_sub_packet, remaining = remain_string)
  return(return_info)
}

conv_string <- in_string
version_count <- 0
version_total <- 0
answer_pt2 <- parse_packets(conv_string,0)
print(answer_pt2$value)
