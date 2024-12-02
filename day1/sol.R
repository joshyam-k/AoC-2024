library(tidyverse)

input_raw <- data.frame(x = readLines("day1/input.txt"))

input <- input_raw |> 
  separate(x, into = c("left", "right"), sep = "\\s+") |> 
  mutate(left = as.numeric(left), right = as.numeric(right))

# part 1
input |> 
  mutate(left = sort(left),
         right = sort(right),
         diff = abs(left - right)) |> 
  summarise(res = sum(diff))


# part 2
right_counts <- table(input$right)

lut <- setNames(as.vector(right_counts), names(right_counts))

input |> 
  mutate(multiplier = lut[as.character(left)],
         multiplier = replace_na(multiplier, 0),
         sim_score = left * multiplier) |> 
  summarise(res = sum(sim_score))
