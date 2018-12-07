
library(testthat)
library(rvest)
library(dplyr)

code_input <- readLines("data/aoc_2_data.txt")

page_link <- "https://adventofcode.com/2018/day/2"

# Part 1

# Test Input
test_vec <- page_link %>% read_html() %>% html_nodes('ul li code') %>% html_text()
test_vec <- test_vec[nchar(test_vec) > 1]


# input_string <- "bababc"
count_logic <- function(input_string){
 x <- strsplit(input_string,"") %>% unlist() %>% table %>% data.frame()
 count_2 <- ifelse(length(which(x$Freq==2)) >= 1,1,0)
 count_3 <- ifelse(length(which(x$Freq==3)) >= 1,1,0)
 return(data.frame(count_2=count_2,count_3=count_3))
}

result_list <- lapply(code_input,count_logic)
result_df <- dplyr::bind_rows(result_list)
checksum <- sum(result_df$count_2) * sum(result_df$count_3)
print(checksum)
# 5000


# Part 2
test_vec <- c("abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")

## Input
input_series <- test_vec
input_series <- code_input

## Logic
string_compare <- lapply(input_series,utf8ToInt)
compare_result <- stringdist::seq_distmatrix(string_compare) %>% as.matrix() %>% data.frame()
stop_flag <- FALSE
for(i in 1:nrow(compare_result)){
  for(j in 1:ncol(compare_result)){
    if(compare_result[i,j] == 1) {
      result_string <- c(input_series[i],input_series[j])
      print(result_string)
      stop_flag <- TRUE
      break
    }
    if(stop_flag){
      break
    }
  }
}

common_code <- lapply(result_string,utf8ToInt) %>% data.frame()
common_code$diff <- common_code[,1] - common_code[,2]
common_code_char <- common_code[common_code$diff == 0,1]
common_code_string <- lapply(common_code_char, intToUtf8) %>% unlist() %>% paste0(collapse = "")
print(common_code_string)
# ymdrchgpvwfloluktajxijsqb
