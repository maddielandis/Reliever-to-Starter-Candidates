library(dplyr)
library(tidyr)

# Load your data
my_data <- read.csv(file.choose())

# Define Stuff+ columns
stuff_cols <- c("Stf..FA", "Stf..SI", "Stf..FC", "Stf..FS", 
                "Stf..SL", "Stf..CU", "Stf..CH", "Stf..KC", "Stf..FO")

# Filter
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
    # Replace values <= 100 with NA for Stuff+ columns
    across(all_of(stuff_cols), ~ifelse(. > 100, ., NA))
  ) %>%
  select(Name, Team, Age, elite_pitch_count, everything())

# Print results
print(results)

# Save to CSV (na = "" will handle the NAs in the CSV output)
write.csv(results, "Reliever_Starter_Candidates.csv", row.names = FALSE, na = "")