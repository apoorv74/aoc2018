# library(httr)
# library(rvest)


# session_var <- rvest::html_session("https://adventofcode.com/2018")
# page_link <-  "https://adventofcode.com/2018/day/1/input"
# 
# httr::GET(
#   url = page_link,
#   httr::add_headers(
#     Referer = "https://adventofcode.com/2018/auth/login",
#     Host	= "adventofcode.com",
#     TE = "Trailers"
#   ),
#   httr::set_cookies(
#     ru = "53616c7465645f5f7e9cd37bf71be6bede5d0eb57864e1b8612c0c54906b11ec",
#     session = "53616c7465645f5f7e9cd37bf71be6bede5d0eb57864e1b8612c0c54906b11ec"
#   )
# ) -> res

freq_data <- c(4L, 3L, -15L, -8L, 15L, -17L, -16L, 15L, -10L, 1L, 17L, 19L, -10L, 17L, 11L, 3L, -13L, -13L, -20L, 10L, -7L, 2L, -12L, 2L, 2L, 7L, 7L, 19L, -18L, -10L, 16L, 18L, -9L, 15L, -16L, 15L, 15L, 14L, 13L, 15L, 2L, 6L, 10L, 12L, 13L, 8L, 3L, -19L, -12L, 19L, 13L, 4L, 16L, 16L, -10L, 15L, -16L, 17L, 16L, 5L, 1L, 17L, 15L, -16L, 3L, 17L, -9L, -4L, -11L, 2L, 19L, -12L, 9L, 8L, 8L, -7L, 17L, -15L, -17L, 18L, 9L, -11L, 10L, 5L, -18L, 15L, 12L, -3L, -2L, 18L, -1L, 5L, 12L, 3L, -7L, 16L, 3L, 4L, 17L, -13L, 7L, -13L, 3L, 7L, 13L, 4L, -10L, -13L, 4L, 13L, 15L, 14L, -13L, 10L, -9L, -16L, 4L, 18L, -13L, -11L, 13L, 8L, -11L, 18L, 19L, -16L, -6L, -5L, 16L, -10L, -14L, 7L, 10L, 10L, -16L, -19L, 10L, -5L, 13L, 24L, -1L, 9L, 2L, 12L, 11L, -4L, -17L, 2L, -17L, 8L, 18L, -10L, 2L, -6L, -2L, -12L, 21L, 16L, -21L, -7L, -6L, -7L, -3L, 14L, 16L, 3L, 10L, -14L, 2L, 15L, 9L, 13L, 4L, 2L, -9L, -9L, 4L, -2L, -11L, -9L, 16L, 9L, -6L, -17L, 22L, 8L, 16L, 5L, 9L, 18L, -5L, -1L, 13L, -8L, -11L, 10L, 6L, 9L, -1L, 13L, 11L, -3L, -12L, 17L, -19L, -11L, 10L, 17L, 15L, 9L, 9L, 4L, -3L, 9L, -4L, 8L, -7L, 12L, 5L, -2L, -6L, -17L, 12L, 18L, 14L, -1L, 15L, 12L, -19L, -16L, -4L, -4L, 1L, -9L, 5L, -6L, 4L, 16L, -1L, 18L, 5L, 17L, -1L, 5L, 15L, 4L, 3L, -11L, 5L, 18L, -3L, 2L, 7L, -13L, 10L, 21L, -7L, 6L, -2L, -20L, -11L, -12L, -18L, -7L, -15L, 9L, -1L, 18L, -10L, -5L, -15L, 3L, -20L, 22L, -18L, 11L, -6L, 15L, -16L, 17L, 6L, 20L, 19L, -8L, -21L, -8L, 9L, 21L, -15L, 10L, 17L, 16L, -22L, 5L, 5L, -20L, 7L, 22L, -1L, 11L, -17L, -14L, -15L, 2L, -8L, 11L, -4L, -37L, -9L, -10L, 30L, 8L, 50L, 28L, -10L, 6L, 16L, 2L, 11L, 8L, 14L, -17L, -11L, 18L, 1L, 1L, -3L, 17L, 15L, -1L, 13L, 9L, -20L, -7L, 20L, 18L, -16L, -12L, 19L, 2L, 10L, -13L, -4L, -12L, 2L, -11L, -15L, -1L, 22L, -12L, -4L, 1L, -12L, -9L, -1L, 17L, 15L, -6L, -12L, -16L, -5L, 16L, 20L, -18L, -5L, -17L, -1L, -12L, 21L, -13L, -5L, 15L, 36L, -12L, 5L, 30L, 48L, -7L, 2L, 7L, 10L, -14L, 11L, -17L, 34L, 20L, 17L, -16L, -4L, -7L, -6L, 1L, 11L, 11L, 5L, -14L, 35L, 16L, 6L, 28L, -3L, 18L, -4L, -6L, -12L, -13L, 30L, 12L, 16L, -14L, -15L, 14L, -6L, 17L, 2L, 10L, -15L, -19L, -20L, 22L, -20L, 12L, -8L, 9L, -19L, 40L, 17L, -1L, 3L, 7L, 12L, -26L, -3L, -5L, -5L, 12L, 24L, -10L, 3L, -27L, 6L, 13L, -23L, 5L, -27L, -41L, -5L, -41L, 144L, -40L, 155L, 29L, -26L, 16L, 9L, -229L, -70L, 401L, 64265L, -10L, 19L, -3L, -14L, 10L, 2L, 13L, 13L, -19L, -18L, -11L, 16L, -19L, 10L, -2L, -7L, -12L, -13L, -16L, 19L, -12L, 14L, 2L, 15L, 17L, 14L, 19L, 8L, 14L, -7L, -16L, 11L, 15L, 9L, 11L, 5L, -8L, 16L, -11L, -16L, -14L, 10L, -13L, -13L, 8L, -3L, -23L, -19L, -18L, -3L, -7L, 18L, 5L, -2L, 13L, 10L, -8L, -16L, 22L, -9L, 18L, 7L, 22L, -3L, -17L, 11L, 11L, 12L, 11L, 7L, 16L, 12L, 2L, -16L, -8L, 15L, 8L, -4L, 2L, -4L, 12L, -18L, 15L, -17L, -20L, 18L, -1L, 6L, -22L, 23L, 19L, 3L, 15L, -17L, -2L, 7L, 1L, -12L, -7L, -15L, -4L, 15L, -13L, 11L, 14L, 16L, 19L, 7L, -3L, -10L, 12L, -22L, 6L, -2L, -14L, 18L, -1L, 27L, 13L, 15L, 10L
               , 18L, 10L, 11L, -13L, 11L, -1L, 7L, 3L, -2L, 14L, -8L, -2L, 9L, -16L, 13L, 7L, 19L, -15L, 3L, 9L, 14L, -8L, 13L, 19L, -10L, -6L, -4L, 7L, 6L, -1L, -6L, -4L, -16L, 1L, 18L, 17L, 1L, 16L, 17L, -19L, 12L, 21L, 10L, 7L, 3L, -9L, -9L, 11L, 17L, 5L, 13L, 2L, 15L, -6L, -18L, 14L, 17L, -2L, -12L, -14L, 1L, -18L, -1L, -12L, -13L, 7L, 14L, 14L, -18L, 1L, 19L, -9L, -17L, -15L, -14L, -12L, -20L, 12L, -8L, 1L, 8L, 11L, 6L, 8L, 10L, -9L, -6L, 20L, 5L, 14L, -17L, -13L, 22L, 13L, 3L, 11L, 19L, -1L, -9L, -5L, -22L, 19L, 20L, -16L, 6L, 15L, -2L, 31L, 12L, 3L, 4L, 24L, 16L, -6L, 11L, -20L, 18L, 8L, -9L, -9L, 28L, -4L, -3L, -17L, -9L, -11L, -1L, 15L, 13L, 12L, -30L, -16L, 19L, -22L, 2L, -18L, 7L, -8L, -24L, 31L, 5L, 36L, 3L, 48L, 10L, 19L, -7L, -11L, 5L, 3L, 7L, 4L, -15L, 13L, 10L, -7L, 14L, 9L, 7L, 11L, 12L, -3L, 6L, -17L, 7L, -15L, -10L, 17L, -4L, -17L, -13L, 18L, -3L, -9L, 5L, 10L, -12L, -1L, 15L, 20L, -3L, -1L, -1L, -6L, 15L, -2L, 13L, 19L, -1L, 8L, 17L, 4L, 1L, 18L, 1L, 4L, 12L, 3L, -5L, -21L, -15L, -14L, 20L, 5L, 18L, -6L, -5L, -5L, 4L, -10L, 18L, -5L, 23L, 22L, 13L, -5L, 15L, 15L, -14L, -15L, 7L, -11L, -8L, -2L, -6L, -9L, -15L, -3L, 12L, 25L, -9L, -15L, -16L, 20L, 13L, 25L, -6L, 10L, 7L, 15L, -16L, 15L, -9L, -21L, 13L, 14L, 6L, -16L, 17L, -27L, -21L, 12L, 26L, 13L, 5L, -23L, 16L, 9L, 20L, 14L, -32L, 27L, -2L, -16L, 21L, -92L, -12L, -14L, -25L, 4L, -21L, -9L, -14L, -14L, -6L, 8L, 5L, 2L, -1L, -3L, 15L, 13L, 2L, -19L, -15L, 11L, 14L, -4L, -18L, 23L, -24L, -14L, 17L, -10L, 1L, 14L, -19L, -18L, -11L, -10L, -13L, 8L, 21L, -4L, -4L, 11L, -17L, -13L, -23L, -9L, 14L, 14L, -20L, -17L, 11L, -18L, -19L, -64L, 9L, -24L, -16L, -10L, 196L, 16L, -5L, 21L, -7L, 28L, 5L, 5L, -19L, 38L, -33L, 25L, 209L, 391L, -559L, 64522L, -2L, -19L, 7L, 17L, 5L, -6L, 18L, -2L, 5L, 4L, -11L, -17L, 14L, 12L, -19L, -9L, 14L, 20L, -16L, -6L, 12L, 15L, 11L, 15L, 24L, -37L, 12L, -15L, -4L, 3L, -21L, -17L, -19L, -16L, 21L, 12L, 9L, -13L, -20L, -30L, -7L, 10L, 14L, -3L, 20L, 11L, -1L, 9L, -22L, -19L, -3L, -8L, 3L, -18L, 8L, 3L, -7L, -2L, -7L, 6L, 15L, -19L, 13L, -26L, 7L, -18L, -13L, 21L, -9L, 10L, -130255L)

# Prob 1
sum(freq_data)

# test_data <- c(3L, 3L, 4L, -2L, -4L)

# Prob 2
sum_var <- 0
result_vec <- c()
loop_run <- TRUE
j <- 0
while(j < 1000){
for(i in 1:length(freq_data)){
  sum_var <- sum_var + freq_data[[i]]
  # if(sum_var %in% result_vec){
  #   loop_run <- FALSE
  #   print(glue::glue('First repeated freq is: {sum_var}'))
  #   break
  # }
  result_vec <- c(result_vec, sum_var)
  
}
  j <- j+1
}

# First repeated freq is: 65474



dupfreq <- function(deltas = 0, initial = 0, n = 10) {
  initial <- 0
  deltas <- freq_data
  x <- c(initial, rep(deltas, times = 1000))
  y <- cumsum(x)
  y[min(which(duplicated(y)))]
}

expect_identical(dupfreq(c(1, -2, 3, 1)), 2)



















