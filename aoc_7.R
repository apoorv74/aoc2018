library(dplyr)

# Part 1

code_input <- readLines("data/aoc_7_data.txt")
test_input <-
  c(
    "Step C must be finished before step A can begin.",
    "Step C must be finished before step F can begin.",
    "Step A must be finished before step B can begin.",
    "Step A must be finished before step D can begin.",
    "Step B must be finished before step E can begin.",
    "Step D must be finished before step E can begin.",
    "Step F must be finished before step E can begin."
  )


# input_to_test <- test_input # For test
input_to_test <- code_input


get_all_steps <- stringr::str_replace_all(string = input_to_test,pattern = "must be finished before step",replacement = "")
get_all_steps <- stringr::str_replace_all(string = get_all_steps,pattern = "Step ",replacement = "")
get_all_steps <-
  stringr::str_replace_all(string = get_all_steps,
                           pattern = "can begin\\.",
                           replacement = "") %>% stringr::str_trim() %>% reshape2::colsplit(pattern = "  ", names = c("s1", "s2"))


get_all_gates <- function(from_gate,input_df){
  return(input_df$s2[input_df$s1 == from_gate])
}

check_prerequisite <- function(node_to_check, nodes_till_now, input_df){
  node_dep <- input_df$s1[input_df$s2 == node_to_check]
  if(length(intersect(node_dep,nodes_till_now)) == length(node_dep)){
    result_var <- TRUE
  } else {
    result_var <- FALSE
  }
}


node_list <- c()
gate_list <- c()
for(i in 1:nrow(get_all_steps)){
current_gate <- get_all_steps$s1[i]
if(!current_gate %in% node_list){
gate_list <- c(gate_list, current_gate)
while(length(gate_list)>0){
  current_step <- gate_list[1]
  all_gates <- get_all_gates(current_step,get_all_steps)
  if(length(all_gates)>0){
    preq_result <- lapply(all_gates, check_prerequisite, c(current_step,node_list),get_all_steps) %>% unlist()  
    gate_list <- c(gate_list, all_gates[preq_result]) %>% sort()
    move_ahead <- which(preq_result == TRUE)
  }
  if(length(move_ahead)>0){
    node_list <- c(node_list, current_step)
    gate_list <- gate_list[(!gate_list %in% node_list)]
  } else {
    gate_list <- c()
  }
  
}
}
}

step_pattern <- paste0(node_list,collapse = '')
print(step_pattern)
