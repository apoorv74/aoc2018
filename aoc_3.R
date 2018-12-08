
library(tidyverse)

# Part 1

code_input <- readLines("data/aoc_3_data.txt")

get_box_id <- function(box_id_string){
  # Finding start positions
  split_string <- strsplit(box_id_string,split = " ") %>% unlist()
  start_pos <- strsplit(split_string[3],split = ",") %>% unlist()
  start_pos_1 <- start_pos[1] %>% as.numeric()
  start_pos_2 <- stringr::str_replace_all(start_pos[2],pattern = ":",replacement = "") %>% as.numeric()
  boxes_to_add <- strsplit(split_string[4],split = "x") %>% unlist() %>% as.numeric()
  box_number <- expand.grid(seq(start_pos_1,start_pos_1+boxes_to_add[1]-1),seq(start_pos_2,start_pos_2+boxes_to_add[2]-1))
  box_number$box_id <- paste0(box_number$Var1,";",box_number$Var2)
  return(box_number)
}

test_vec <- c("#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2")

input_vector <- test_vec
input_vector <- code_input

x <- lapply(input_vector, get_box_id) 

# Number of intersecting Boxes
all_box_ids <- purrr::map(x,`[`,"box_id") %>% unlist()
table(all_box_ids) %>% data.frame() %>% filter(Freq>1) %>% nrow()
# 116920

# Part 2
# Boxes which are not intersecting
no_intersection <- table(all_box_ids) %>% data.frame() %>% filter(Freq==1)

check_for_uniquness <- function(box_details){
  box_ids <- box_details$box_id
  # box_check <- box_ids[!box_ids %in% no_intersection$all_box_ids]
  box_diff_length <- length(base::setdiff(box_ids,no_intersection$all_box_ids))
  return(box_diff_length)
}

y <- lapply(x, check_for_uniquness) %>% unlist()
input_vector[which(y == 0)]
#382 @ 155,316: 28x15


