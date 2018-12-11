
library(tidyverse)
library(progress)

code_input <- readLines("data/aoc_6_data.txt")
test_input <- c("1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9")

structure_input <- function(input_pattern){
  structured_input <- reshape2::colsplit(input_pattern,pattern = ",",names = c('x','y'))
  structured_input$point_name <- paste0('A',c(1:nrow(structured_input)))
  return(structured_input)
}

# formatted_input <- structure_input((test_input)) # For test

formatted_input <- structure_input((code_input))

# Part 1

# For Manhattan Distance - https://math.stackexchange.com/questions/139600/how-do-i-calculate-euclidean-and-manhattan-distance-by-hand

# Finaing plane co-ordinates
left_most <- c(0,0)
right_most <- c(max(formatted_input$x), max(formatted_input$y))

points_in_grid <- expand.grid(seq(left_most[[1]], right_most[[1]]), seq(left_most[[2]], right_most[[2]]))

# Defining grid boundary - For detecting area with infinite/finite points

boundary_points <-
  points_in_grid %>% filter (Var1 == min(Var1) |
                               Var1 == max(Var1) | Var2 == min(Var2) | Var2 == max(Var2))
boundary_points$status <- 'boundary'

manhattan_dist <- function(a1,b1,a2,b2){
  dist_vec <- abs(a1-a2) + abs(b1-b2)
  return(dist_vec)
}

points_df <- data.frame(matrix(nrow = 0,ncol = 3))
names(points_df)[] <- c('X','Y','closest_from')

pb <- progress_bar$new(
  format = " [:bar] :percent in :elapsed",
  total = nrow(points_in_grid), clear = FALSE, width= 60)
for(i in 1:nrow(points_in_grid)){
  points_close_by <- c()
  for(j in 1:nrow(formatted_input)){
    a1 <- points_in_grid$Var1[i]
    b1 <- points_in_grid$Var2[i]
    a2 <- formatted_input$x[j]
    b2 <- formatted_input$y[j]
    distance_between <- manhattan_dist(a1,b1,a2,b2)
    points_close_by <- c(points_close_by,distance_between)
  }
  min_distance <- min(points_close_by)
  index_vec <- which(points_close_by == min(points_close_by))
  
  if(length(index_vec) == 1){
    position_vec <- index_vec %% nrow(formatted_input)
    position_vec <- ifelse(position_vec == 0,nrow(formatted_input),position_vec)
    closest_from <- formatted_input$point_name[position_vec]
    points_df <- dplyr::bind_rows(points_df, data.frame('X'=a1,'Y'=b1,'closest_from'=closest_from))
  } else{
    points_df <- dplyr::bind_rows(points_df, data.frame('X'=a1,'Y'=b1,'closest_from'="."))
  }
  pb$tick()
}

points_df <- dplyr::left_join(points_df, boundary_points, by=c("X"="Var1","Y"="Var2"))
infinite_area_points <- unique(points_df$closest_from[points_df$status %in% c('boundary')])
points_df <- points_df[!points_df$closest_from %in% infinite_area_points,]
table(points_df$closest_from) %>% data.frame() %>% filter(Freq == max(Freq))

# [=============================================] 100% in 14m

# Var1 Freq
# A40 4887

# Part 2

# formatted_input <- structure_input((test_input)) # For test
formatted_input <- structure_input((code_input)) # For test

left_most <- c(0,0)
right_most <- c(max(formatted_input$x), max(formatted_input$y))

points_in_grid <- expand.grid(seq(left_most[[1]], right_most[[1]]), seq(left_most[[2]], right_most[[2]]))

# Defining grid boundary - For detecting area with infinite/finite points

boundary_points <-
  points_in_grid %>% filter (Var1 == min(Var1) |
                               Var1 == max(Var1) | Var2 == min(Var2) | Var2 == max(Var2))
boundary_points$status <- 'boundary'

points_df <- data.frame(matrix(nrow = 0,ncol = 3))
names(points_df)[] <- c('X','Y','total_distance')

pb <- progress_bar$new(
  format = " [:bar] :percent in :elapsed",
  total = nrow(points_in_grid), clear = FALSE, width= 60)

for(i in 1:nrow(points_in_grid)){
  points_close_by <- c()
  for(j in 1:nrow(formatted_input)){
    a1 <- points_in_grid$Var1[i]
    b1 <- points_in_grid$Var2[i]
    a2 <- formatted_input$x[j]
    b2 <- formatted_input$y[j]
    distance_between <- manhattan_dist(a1,b1,a2,b2)
    points_close_by <- c(points_close_by,distance_between)
  }
  total_distance <- sum(points_close_by)
  points_df <- dplyr::bind_rows(points_df, data.frame('X'=a1,'Y'=b1,'total_distance'=total_distance))
  pb$tick()
}

under_10k <- points_df[points_df$total_distance < 10000,]
# 34096




