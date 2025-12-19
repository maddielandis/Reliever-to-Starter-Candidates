library(dplyr)
library(tidyr)

my_data <- read.csv(file.choose())

stuff_cols <- c("Stf..FA", "Stf..SI", "Stf..FC", "Stf..FS", 
                "Stf..SL", "Stf..CU", "Stf..CH", "Stf..KC", "Stf..FO")

results <- my_data %>%
  rowwise() %>%
  mutate(
    elite_pitch_count = sum(c_across(all_of(stuff_cols)) > 100, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(
    elite_pitch_count >= 2,
    Location. > 100,
    F.Strike. >= 0.62,
    K.BB. > 0.20,
    Age <= 32
  ) %>%
  mutate(
    across(all_of(stuff_cols), ~ifelse(. > 100, ., NA))
  ) %>%
  select(Name, Team, Age, elite_pitch_count, everything())

print(results)

write.csv(results, "Reliever_Starter_Candidates.csv", row.names = FALSE, na = "")
