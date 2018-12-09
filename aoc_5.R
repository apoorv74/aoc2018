library(tidyverse)

# Part 1

code_input <- readLines("data/aoc_5_data.txt")

polymer_group_df <- data.frame(small=letters,large=LETTERS) %>% mutate(combine_var_1 = paste0(letters,LETTERS),
                                                                       combine_var_2 = paste0(LETTERS,letters))
pattern_vec <- c(polymer_group_df$combine_var_1,polymer_group_df$combine_var_2)

test_input <- 'dabAcCaCBAcCcaDA'

string_reduce <- function(string_to_reduce){
input_in_loop <- string_to_reduce
loop_flag <- TRUE
while(loop_flag){
  input_fed <- input_in_loop
  for(i in 1:length(pattern_vec)){
    input_in_loop <- stringr::str_replace_all(string = input_in_loop, pattern = pattern_vec[[i]],replacement = "")
  }
  if(nchar(input_fed) == nchar(input_in_loop))
  {
    loop_flag <- FALSE
    value_to_return <- nchar(input_in_loop)
    # print(glue::glue("{input_in_loop} -> {nchar(input_in_loop)} units"))
  }
}
return(value_to_return)
}

string_to_reduce <- test_input
string_reduce(string_to_reduce)

# Part 2
test_input <- 'dabAcCaCBAcCcaDA'
# input_in_loop <- test_input # For test
input_in_loop <- code_input

reduced_vec_length <- c()

# pattern_vec_temp <- pattern_vec[c(1:4,27:30)] # For test
# length(pattern_vec_temp)/2) # For test
for(rem_pat in 1:(length(pattern_vec)/2)){
  pattern_to_remove <- pattern_vec[c(rem_pat,rem_pat+26)]
  updated_input <- stringr::str_replace_all(pattern = glue::glue('[{pattern_to_remove[1]}]'), string = input_in_loop,replacement = '')
  updated_input <- stringr::str_replace_all(pattern = glue::glue('[{pattern_to_remove[2]}]'), string = updated_input,replacement = '')
  reduced_length <- string_reduce(updated_input)
  print(glue::glue('Pattern - {pattern_to_remove} - Reduced Length - {reduced_length}'))
  reduced_vec_length <- c(reduced_vec_length, reduced_length)
}

min(reduced_vec_length) # 5698
pattern_vec[c(which(reduced_vec_length == min(reduced_vec_length)),26+which(reduced_vec_length == min(reduced_vec_length)))]
# uU
