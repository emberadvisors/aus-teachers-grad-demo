# Some recommended libraries
library(tidyverse)
library(readxl)
library(janitor)

# Some useful functions
?read_rds()    # Read in your transformed datasets
?inner_join()  # Methods for joining datasets together based on key columns
?left_join()   
?summarise()   # Helps with summarising data into smaller sets


# Suggested linkage steps ----
# Read in transformed data from step 1
student_data <- readRDS("~/aus-teachers-grad-demo/data/cleaned_data/clean_students_data.rds")
staff_data <- readRDS("~/aus-teachers-grad-demo/data/cleaned_data/clean_staff_data.rds")

# Summarise datasets to have the same grain of information
student_data_agg <- student_data %>% 
  group_by(year, state_territory, government_flag, catholic_flag, school_level, sex) %>% 
  summarise(student_count = sum(student_count, na.rm = TRUE))

check_2023_student <- student_data_agg %>% filter(year == 2023)
sum(check_2023_student$student_count)

staff_data_agg <- staff_data %>% 
  group_by(year, state_territory, government_flag, catholic_flag, school_level, sex) %>% 
  summarise(staff_count = sum(staff_count, na.rm = TRUE))

check_2023_staff <- staff_data_agg %>% filter(year == 2023)
sum(check_2023_staff$staff_count)

# Join data together
student_staff_data <- full_join(student_data_agg, staff_data_agg, by = c("year", "state_territory", "government_flag", "catholic_flag", "school_level", "sex"))

check_2023 <- student_staff_data %>% filter(year == 2023)
sum(check_2023$student_count)
sum(check_2023$staff_count)

# Save linked data as a single set for ease of future use
saveRDS(student_staff_data, "~/aus-teachers-grad-demo/data/cleaned_data/clean_student_staff_data.rds")


