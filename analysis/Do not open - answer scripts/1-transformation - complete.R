
## COMPLETED VERSION TO SUPPORT SESSION FACILITATION

# Some recommended libraries
library(tidyverse)
library(readxl)
library(janitor)

# Some useful functions
?read_excel()   # Reads Excel files - hint, there is a parameter that can help you get rid of the junk rows at top
?clean_names()  # Removes white space from columns,  making them easier to program with
?mutate()       # Helps with modifying columns in a dataset
?coalesce()     # Replaces missing values with a chosen value
?write_rds()    # Saves datasets in "R" format (make sure the filename extension is .rds)
?pivot_longer() # Lengthens data, turning multiple columns into a single column 

# Suggested transformation steps ----

# Read in data
students_data_in <- read_excel("~/aus-teachers-grad-demo/data/Table 43a Full-Time Equivalent Students, 2006-2023.xlsx"
                               , col_types = c("text"), sheet = "Table 1", skip = 7) %>% 
  clean_names() %>% 
  mutate(sum_of_fte_aboriginal_and_torres_strait_islander_students = as.numeric(sum_of_fte_aboriginal_and_torres_strait_islander_students)
         , sum_of_fte_non_indigenous_students = as.numeric(sum_of_fte_non_indigenous_students)
         , sum_of_fte_all_students = as.numeric(sum_of_fte_all_students))

# Remove duplicate rows
students_data_filt <- students_data_in %>% 
  filter(if_all(everything(), ~ !grepl("total", ., ignore.case = TRUE))) %>% 
  mutate(year = as.numeric(year))

# Pivot non-tidy columns
students_data_pivot <- students_data_filt %>% 
  select(-c(sum_of_fte_all_students)) %>% 
  pivot_longer(cols = c("sum_of_fte_aboriginal_and_torres_strait_islander_students", "sum_of_fte_non_indigenous_students")
               , names_to = "indigenous_flag", values_to = "student_count") %>% 
  mutate(indigenous_flag = as.numeric(ifelse(indigenous_flag == "sum_of_fte_aboriginal_and_torres_strait_islander_students", 1, 0)))

# Clean up column names and values
unique(students_data_pivot$affiliation_gov_non_gov)
unique(students_data_pivot$affiliation_gov_cath_ind)

students_data_clean <- students_data_pivot %>%
  rename("government_flag" = "affiliation_gov_non_gov"
         , "catholic_flag" = "affiliation_gov_cath_ind"
         , "anr_school_level" = "national_report_on_schooling_anr_school_level")

students_data_clean <- students_data_clean %>% 
  mutate(across(c(state_territory, government_flag, catholic_flag, ft_pt, sex, school_level, anr_school_level, year_grade), ~substr(., 3, nchar(.))))

# Check for, and clean missing data
sum(is.na(students_data_clean)) ## many missing values (422876)

students_data_filled <- students_data_clean %>%
  fill(c("year", "state_territory", "government_flag", "catholic_flag", "ft_pt", "sex", "school_level"
         , "anr_school_level", "year_grade"), .direction = "down")

sum(is.na(students_data_filled)) ## 0 missing values

# Perform sense checks
check_2023 <- students_data_filled %>% filter(year == 2023)
sum(check_2023$student_count)
check_2023_nsw <- check_2023 %>% filter(state_territory == "NSW")
sum(check_2023_nsw$student_count)
check_2023_nsw_male <- check_2023 %>% filter(sex == "Male" & state_territory == "NSW")
sum(check_2023_nsw_male$student_count)

# Save data as RDS to simplify future steps
saveRDS(students_data_filled, "~/aus-teachers-grad-demo/data/cleaned_data/clean_students_data.rds")


#### Replicate code for the staff data

# Read in data
staff_data_in <- read_excel("~/aus-teachers-grad-demo/data/Table 51a In-school Staff (FTE), 2006-2023.xlsx", sheet = "Table 1", skip = 7) %>% 
  clean_names()

# Remove duplicate rows
staff_data_filt <- staff_data_in %>% 
  filter(if_all(everything(), ~ !grepl("total", ., ignore.case = TRUE))) %>% 
  mutate(year = as.numeric(year))

# Pivot non-tidy columns
staff_data_pivot <- staff_data_filt %>% 
  select(-c(total)) %>% 
  pivot_longer(cols = c("a_male", "b_female")
               , names_to = "sex", values_to = "staff_count") %>% 
  mutate(sex = ifelse(sex == "a_male", "Male", "Female"))

# Clean up column names and values
unique(staff_data_pivot$affiliation_1)
unique(staff_data_pivot$affiliation_2)

staff_data_clean <- staff_data_pivot %>% 
  rename("government_flag" = "affiliation_1"
         , "catholic_flag" = "affiliation_2"
         , "staff_function" = `function`)

staff_data_clean <- staff_data_clean %>% 
  mutate(across(c(state_territory, government_flag, catholic_flag, school_level, staff_function), ~substr(., 3, nchar(.))))

# Check for, and clean missing data
sum(is.na(staff_data_clean)) ## many missing values (48544)

staff_data_filled <- staff_data_clean %>%
  fill(c("year", "state_territory", "government_flag", "catholic_flag", "school_level", "staff_function"), .direction = "down")

sum(is.na(staff_data_filled)) ## many missing values (48544)

# staff_data_filled <- staff_data_filled %>% 
#   filter(!is.na(staff_count))

# Perform sense checks
check_2023_staff <- staff_data_filled %>% filter(year == 2023)
sum(check_2023_staff$staff_count)
check_2023_staff_nsw <- check_2023_staff %>% filter(state_territory == "NSW")
sum(check_2023_staff_nsw$staff_count)

# Save data as RDS to simplify future steps
saveRDS(staff_data_filled, "~/aus-teachers-grad-demo/data/cleaned_data/clean_staff_data.rds")




