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


# Remove duplicate rows


# Pivot non-tidy columns


# Clean up column names and values


# Check for, and clean missing data


# Perform sense checks


# Save data as RDS to simplify future steps