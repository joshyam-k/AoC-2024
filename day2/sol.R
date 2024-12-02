library(tidyverse)

input_raw <- data.frame(x = readLines("day2/input.txt"))

input <- input_raw |> 
  mutate(nums_raw = str_extract_all(x, "\\d+"),
         nums = map(nums_raw, as.numeric)) |> 
  select(nums)


is_valid <- function(vec) {
  
  if (!all(vec == sort(vec)) && !all(vec == sort(vec, decreasing = TRUE))) {
    return(FALSE)
  } else {
    diffs <- abs(diff(vec))
    if (all(diffs <= 3 & diffs >= 1)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
}

## part 1
p1 <- input |> 
  mutate(valid = map(nums, is_valid)) |> 
  filter(valid == TRUE) |> 
  nrow()
 

is_valid_p2 <- function(vec) {
  
  for (i in 1:length(vec)) {
    new_vec <- vec[-i]
    if (is_valid(new_vec)) {
      return(TRUE)
    }
  }
  
  return(FALSE)
  
}


p2 <- input |> 
  mutate(is_valid_p1 = map(nums, is_valid)) |> 
  filter(is_valid_p1 == FALSE) |> 
  mutate(is_valid_p2 = map(nums, is_valid_p2)) |> 
  filter(is_valid_p2 == TRUE) |> 
  nrow()

p2 + p1
