
library(magrittr)

test_input <- c("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")
code_input <- readLines("data/aoc_8_data.txt")

# Part 1

# position_0 <- which(input_to_test == 0)
# position_meta <- position_0 + 1
# num_meta <- input_to_test[position_meta]
# 
# meta_values_at <- data.frame(position_meta = position_meta, num_meta = num_meta)
# meta_values <- c()
# for(i in 1:nrow(meta_values_at)){
#   start_pos <- meta_values_at$position_meta[i] + 1
#   end_pos <- meta_values_at$position_meta[i] + meta_values_at$num_meta[i]
#   # print(input_to_test[c(start_pos:end_pos)])
#   meta_values <-
#     c(meta_values, input_to_test[c(start_pos:end_pos)])
# }
# 
# num_values_to_consider <- (length(input_to_test) - end_pos)
# meta_values <- c(meta_values,tail(input_to_test,num_values_to_consider))
# 
# # position_meta <- c(position_meta, max(position_meta)+1)
# # num_meta <- c(num_meta, length(input_to_test)-max(position_meta))
# 
# print(sum(meta_values))
# 
# 
# 
# ## JBC
# 
# ex <- scan(what = integer(1), text = "
# 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
# ")
# 
# get_metadata <- function(x) {
#   read_node <- function() {
#     if (i > length(x)) return()
#     n_nodes <- x[i]; i <<- i + 1
#     n_meta  <- x[i]; i <<- i + 1
#     for (j in seq_len(n_nodes)) read_node()
#     for (j in seq_len(n_meta)) {
#       meta <<- append(meta, x[i]); i <<- i + 1
#     }
#   }
#   meta <- integer()
#   i <- 1
#   read_node()
#   meta
# }
# 
# (y <- get_metadata(input_to_test))
# sum(y)

## Trying a recursive solution - Reference - https://www.reddit.com/r/adventofcode/comments/a47ubw/2018_day_8_solutions/ebc7t6x

sum_of_meta <- 0
i <<- 0
get_sum_meta <- function(){
  i <<-  i + 1
  count_var <- input_to_test[i]
  i <<-  i + 1
  meta_var <- input_to_test[i]
  
  # browser()
  if(count_var>0) {
    for (j in 1:count_var) {
      sum_of_meta <- sum_of_meta + get_sum_meta()
    }
  } 
    
  for (k in 1:meta_var) {
      i <<- i + 1
      print(input_to_test[[i]])
      sum_of_meta <- sum_of_meta + input_to_test[[i]]
    }
  
    return(sum_of_meta)
  }

# input_to_test <- test_input # To test
input_to_test <- code_input
input_to_test <- stringr::str_split(input_to_test,pattern = " ") %>% unlist() %>% as.numeric()


part_1_answer <- get_sum_meta() %>% clipr::write_clip()
# 41926

# Part 2
read_node <- function(x) {
    if (i > length(x)) return(node_values)
    n_nodes <- x[i]; i <<- i + 1
    n_meta  <- x[i]; i <<- i + 1
    
    # browser()
    node_values <- integer()
    for (j in seq_len(n_nodes)) {
      node_values <- append(node_values, read_node(x))
    }
    
    meta_values <- integer()
    for (j in seq_len(n_meta)) {
      meta_values <- append(meta_values, x[i]); i <<- i + 1
    }
    
    if (n_nodes == 0) {
      sum(meta_values)
    } else {
      sum(node_values[meta_values], na.rm = TRUE)
    }
}

i <<- 1
input_to_test <- code_input
input_to_test <- stringr::str_split(input_to_test,pattern = " ") %>% unlist() %>% as.numeric()
read_node(input_to_test)
