
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

## Unstructured excel table with additional breakdowns
students_data_in <- read_excel("~/aus-teachers-grad-demo/data/Table 43a Full-Time Equivalent Students, 2006-2023.xlsx", sheet = "Table 1", skip = 7) %>% 
  clean_names()

staff_data_in <- read_excel("~/aus-teachers-grad-demo/data/Table 51a In-school Staff (FTE), 2006-2023.xlsx", sheet = "Table 1", skip = 7) %>% 
  clean_names()

# Pivot non-tidy columns

## pivot number columns longer and create a column for indigenous status
students_data_pivot <- students_data_in %>% 
  select(-c(sum_of_fte_all_students)) %>% 
  pivot_longer(cols = c("sum_of_fte_aboriginal_and_torres_strait_islander_students", "sum_of_fte_non_indigenous_students")
               , names_to = "indigenous_flag", values_to = "student_count") %>% 
  mutate(indigenous_flag = as.numeric(ifelse(indigenous_flag == "sum_of_fte_aboriginal_and_torres_strait_islander_students", 1, 0)))

## pivot number columns longer and create a column for gender
staff_data_pivot <- staff_data_in %>% 
  select(-c(total)) %>% 
  pivot_longer(cols = c("a_male", "b_female")
               , names_to = "gender", values_to = "staff_count") %>% 
  mutate(gender = ifelse(gender == "a_male", "male", "female"))

# Clean up column names
unique(students_data_pivot$affiliation_gov_non_gov)
unique(students_data_pivot$affiliation_gov_cath_ind)

students_data_clean <- students_data_pivot %>% 
  rename("government_flag" = "affiliation_gov_non_gov"
         , "catholic_flag" = "affiliation_gov_cath_ind"
         , "anr_school_level" = "national_report_on_schooling_anr_school_level")

unique(staff_data_pivot$affiliation_1)
unique(staff_data_pivot$affiliation_2)

staff_data_clean <- staff_data_pivot %>% 
  rename("government_flag" = "affiliation_1"
         , "catholic_flag" = "affiliation_2"
         , "staff_function" = `function`)

# Check for, and clean missing data

## count NAs across whole students data set
sum(is.na(students_data_clean)) ## many missing values (422876)

students_data_clean <- students_data_clean %>%
  fill(c("year", "state_territory", "government_flag", "catholic_flag", "ft_pt", "sex", "school_level"
         , "anr_school_level", "year_grade"), .direction = "down")

sum(is.na(students_data_clean)) ## 0 missing values

## count NAs in the total students column
check_na <- students_data_clean %>% 
  filter(is.na(student_count)) ## 0 missing values

## count NAs across whole staff data set
sum(is.na(staff_data_clean)) ## many missing values (48544)

staff_data_clean <- staff_data_clean %>%
  fill(c("year", "state_territory", "government_flag", "catholic_flag", "school_level"), .direction = "down")

sum(is.na(staff_data_clean)) ## many missing values (48544)


# Save data as RDS to simplify future steps





## Other options
# students_data_fill_check <- students_data_in %>% 
#   fill(all_of(names(students_data_in)), .direction = "down")

## Structured in excel
# students_data_in <- read_excel("~/aus-teachers/data/Table 43a Full-Time Equivalent Students, 2006-2023.xlsx", sheet = "Table 2", skip = 5) %>% 
#   clean_names()
# 
# students_data <- students_data_in %>% 
#   rename_with(~ sub(".*_", "", .), .cols = c(a_nsw, b_vic, c_qld, d_sa, e_wa, f_tas, g_nt, h_act))


