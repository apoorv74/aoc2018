library(tidyverse)

# Part 1

# Sorting the input dataset
code_input <- readLines("data/aoc_4_data.txt")

code_input_df <- data.frame("status_string" = code_input)
code_input_df$guard_id <- stringr::str_extract(string =code_input_df$status_string, pattern <- "#\\d*")
code_input_df$time_int <-
  stringr::str_extract(string = code_input_df$status_string, pattern <-
                         "\\d+-\\d+-\\d+ \\d+:\\d+") %>% as.POSIXct(format="%Y-%m-%d %H:%M")

code_input_df$guard_date <- stringr::str_extract(string =code_input_df$status_string, pattern <- "\\d+-\\d+-\\d+")
code_input_df$guard_date <- as.Date(code_input_df$guard_date)
code_input_df <- code_input_df[order(code_input_df$time_int),]

code_input_df$row_num <- 1:nrow(code_input_df)

id_fields <- code_input_df$row_num[!is.na(code_input_df$guard_id)] %>% data.frame()
id_fields$lag <- c(id_fields$.[2:nrow(id_fields)],nrow(code_input_df))
id_fields$diff <- id_fields$lag - id_fields$.

code_input_df$id_updated <- c(rep(code_input_df$guard_id[!is.na(code_input_df$guard_id)],id_fields$diff),"#2447")

# Calculate the sleep time for every guard

code_input_df$guard_status <- stringr::str_extract(string = code_input_df$status_string, pattern = "([A-Za-z]+ [A-Za-z]+)")
interval_data <- code_input_df[code_input_df$guard_status %in% c('falls asleep','wakes up'),] %>% select(c("guard_date","id_updated","guard_status","time_int"))
interval_data <- dplyr::bind_cols(interval_data[interval_data$guard_status == "falls asleep",],interval_data[interval_data$guard_status == "wakes up",c("guard_status","time_int")])
interval_data$time_asleep <- interval_data$time_int1 - interval_data$time_int
interval_data$time_asleep <- as.numeric(interval_data$time_asleep)
guard_sleep_mins <- interval_data %>% group_by(id_updated) %>% summarise(total_asleep = sum(time_asleep)) %>% arrange(desc(total_asleep))
# 3371


guard_with_most_sleep <- interval_data[interval_data$id_updated == guard_sleep_mins$id_updated[guard_sleep_mins$total_asleep == max(guard_sleep_mins$total_asleep)],]
guard_with_most_sleep$min1 <- 
  stringr::str_extract(string = guard_with_most_sleep$time_int, pattern = ":\\d+:") %>% gsub(pattern = ":",replacement = "") %>% as.numeric()
guard_with_most_sleep$min2 <- guard_with_most_sleep$min1 + guard_with_most_sleep$time_asleep - 1

mins_vec <- c()
for(i in 1:nrow(guard_with_most_sleep)){
  mins_vec <- c(mins_vec, seq(guard_with_most_sleep$min1[i],guard_with_most_sleep$min2[i]))
}

table(mins_vec) %>% data.frame() %>% arrange(desc(Freq))
# 39

# Output - 3371 * 39 -> 131469

# Part 2
interval_data$min1 <- 
  stringr::str_extract(string = interval_data$time_int, pattern = ":\\d+:") %>% gsub(pattern = ":",replacement = "") %>% as.numeric()
interval_data$min2 <- interval_data$min1 + interval_data$time_asleep - 1

guard_sleep_vec <- c()
for(j in 1:nrow(interval_data)){
  guard_sleep_vec <- c(guard_sleep_vec, paste0(interval_data$id_updated[j],'#',seq(interval_data$min1[j],interval_data$min2[j])))
}

table(guard_sleep_vec) %>% data.frame() %>% arrange(desc(Freq)) %>% head(1)
# 1901*51 -> 96951

