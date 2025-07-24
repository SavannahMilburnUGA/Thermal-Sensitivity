# Reordering the site from North to South (y coordinate) and adding site index
library(tidyverse)
# Load data
data <- read.csv("results/2021/TSAndEVs2021.csv")
data <- data %>%
    arrange(desc(y)) %>%
    mutate(index = row_number()) %>%
    select(index, everything())
View(data)
# Save sorted TSAndEVs2021.csv
write_csv(data, "results/2021/SortedTSAndEVs2021.csv")
