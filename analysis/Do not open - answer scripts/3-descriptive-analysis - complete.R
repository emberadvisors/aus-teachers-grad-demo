# Some recommended libraries
library(tidyverse)
library(ggplot2)

# Some useful functions
?ggplot()      # Read in your transformed datasets
?filter()      # Help filter out rows of data based on a logical condition
?mutate()      # Helps with modifying columns in a dataset
?summarise()   # Helps with summarising data into smaller sets
?count()       # Quick way to get the count of a dataset by one of the columns
?View()        # Lets you review the dataset in RStudio

# Example ggplot
example <- tibble(
  year = 2001:2020,
  huge_numbers = runif(20),
  class = sample(c("No cap", "Drip", "Hits different", "OK boomer"), replace = TRUE, 20)
)

example %>%
  ggplot(aes(x = year, y = huge_numbers, fill = class)) +
  geom_bar(stat = 'identity') + 
  labs(
    x = "Year",
    y = "Huge! Numbers",
    fill = "GenZ chat",
    title = "Boomers keep showing up in the data",
    subtitle = "Huge numbers by year and GenZ chat"
  ) + 
  theme_minimal()

# Suggested modelling steps ----
# Read in linked data from step 2
student_staff_data <- readRDS("~/aus-teachers-grad-demo/data/cleaned_data/clean_student_staff_data.rds")

# Explore different cuts of the data

# Example 1: filter 10 years back (2013 to 2023), count the number of staff and students by government_flag column at a national level
staff_students_by_gov_flag <- student_staff_data %>% 
  filter(year >= 2013) %>% 
  group_by(year, government_flag) %>% 
  summarise(student_count = sum(student_count, na.rm = TRUE)
            , staff_count = sum(staff_count, na.rm = TRUE))

# Example 2: find the proportion of students at catholic schools by jurisdiction across the full date range
prop_catholic_students <- student_staff_data %>% 
  group_by(state_territory) %>% 
  summarise(catholic_students = sum(if_else(catholic_flag == "Catholic", student_count, 0), na.rm = TRUE)
            , all_students = sum(student_count, na.rm = TRUE)) %>% 
  mutate(prop_catholic = catholic_students / all_students)

# Example 3: find the proportion of students and teachers who are female over time in NSW
prop_female_students_staff <- student_staff_data %>% 
  group_by(year) %>% 
  summarise(female_students = sum(if_else(sex == "Female", student_count, 0), na.rm = TRUE)
            , all_students = sum(student_count, na.rm = TRUE)
            , female_staff = sum(if_else(sex == "Female", staff_count, 0), na.rm = TRUE)
            , all_staff = sum(staff_count, na.rm = TRUE)) %>% 
  mutate(prop_female_students = female_students / all_students
         , prop_female_staff = female_staff / all_staff) %>% 
  select(year, prop_female_students, prop_female_staff)



# Identify an interesting insight related to the problem
## Create a new column which is the number of students per teacher in each jurisdiction
## Problem statement: Investigate the claim "NSW private schools are impacted less by the teacher shortage because they have been able
## to hire more teachers relative to students."

# Example insight 1: Ratio of students to staff members in private schools by jurisdiction
# Understand how the ratio varies across states, how does NSW compare (slightly tangential)

# Find the number of students per staff member in private schools by jurisdiction over the past 5 years
private_student_rate_by_jurisdiction <- student_staff_data %>% 
  filter(government_flag == "Non-government" & year >= 2018) %>% 
  group_by(year, state_territory) %>% 
  summarise(student_count = sum(student_count, na.rm = TRUE)
            , staff_count = sum(staff_count, na.rm = TRUE)) %>% 
  mutate(student_rate = student_count / staff_count) %>% 
  select(year, state_territory, student_rate)

# Graph the insight
student_rate_plot_1 <- private_student_rate_by_jurisdiction %>% 
  ggplot(aes(x = year, y = student_rate, colour = state_territory, group = state_territory)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Number of students per staff member in private schools by jusrisdiction, 2018 - 2023"
       , x = "Calendar year"
       , y = "Students per staff member"
       , colour = "Jurisdiction") +
  theme_minimal()

# Example insight 2: Ratio of students to staff members across government, non-government schools
# Understand if this media report/claim is true, are there more teachers relative to students in private schools?

student_rate_by_school_type <- student_staff_data %>% 
  filter(state_territory == 'NSW' & year >= 2015) %>% 
  group_by(year, government_flag) %>% 
  summarise(student_count = sum(student_count, na.rm = TRUE)
            , staff_count = sum(staff_count, na.rm = TRUE)) %>% 
  mutate(student_rate = student_count / staff_count) %>% 
  select(year, government_flag, student_rate)

student_rate_plot_2 <- student_rate_by_school_type %>% 
  ggplot(aes(x = year, y = student_rate, colour = government_flag, group = government_flag)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Number of students per staff member in NSW by school type, 2018 - 2023"
       , x = "Calendar year"
       , y = "Students per staff member"
       , colour = "School type") +
  theme_minimal()

# Example insight 2: Ratio of students to staff members across catholic, non-catholic
# Idea: Dis-aggregate the types of private schools to see if there is a particular school type driving this difference

student_rate_by_catholic_type <- student_staff_data %>% 
  filter(state_territory == 'NSW' & year >= 2015) %>% 
  group_by(year, catholic_flag) %>% 
  summarise(student_count = sum(student_count, na.rm = TRUE)
            , staff_count = sum(staff_count, na.rm = TRUE)) %>% 
  mutate(student_rate = student_count / staff_count) %>% 
  select(year, catholic_flag, student_rate)

student_rate_plot_3 <- student_rate_by_catholic_type %>% 
  ggplot(aes(x = year, y = student_rate, colour = catholic_flag, group = catholic_flag)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Number of students per staff member in NSW by catholic school type, 2018 - 2023"
       , x = "Calendar year"
       , y = "Students per staff member"
       , colour = "Catholic type") +
  theme_minimal()


# Example insight 3:
# Looking to see what is driving the change in rate, is it increase in students, decrease in staff, etc.

student_staff_count <- student_staff_data %>% 
  filter(state_territory == 'NSW' & year >= 2015) %>%
  group_by(year, government_flag) %>% 
  summarise(student_count = sum(student_count, na.rm = TRUE)
            , staff_count = sum(staff_count, na.rm = TRUE)) %>% 
  pivot_longer(
    cols = c(student_count, staff_count),
    names_pattern = "^(.*)_count$",
    names_to = 'type',
    values_to = 'count'
  )

# For example, just looking at staff count
staff_count_plot_4 <- student_staff_count %>% 
  filter(type == 'staff') %>% 
  ggplot(aes(x = year, y = count, colour = government_flag, group = government_flag)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Number of staff in NSW by school type, 2018 - 2023"
       , x = "Calendar year"
       , y = "Number of staff"
       , colour = "Catholic type") +
  theme_minimal()

# For example, just looking at student count
student_count_plot_4 <- student_staff_count %>% 
  filter(type == 'student') %>% 
  ggplot(aes(x = year, y = count, colour = government_flag, group = government_flag)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Number of students in NSW by school type, 2018 - 2023"
       , x = "Calendar year"
       , y = "Number of students"
       , colour = "Catholic type") +
  theme_minimal()

