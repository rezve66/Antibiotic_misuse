
# ----- Install Required Packages -----
# Install necessary packages for data manipulation, statistical analysis, and table generation
install.packages("gtsummary")   # For creating summary tables
install.packages("psych")       # For statistical functions
install.packages("MASS")        # For additional statistical methods
install.packages("gt")          # For generating tables in R
install.packages("flextable")   # For creating customizable tables
install.packages("report")      # For creating statistical model reports
install.packages("writexl")     # For exporting data as Excel files

# ----- Load Required Packages -----
library(gtsummary)    # Package for generating tables summarizing data
library(psych)        # For statistical analysis functions
library(readxl)       # To read Excel files
library(gt)           # For table generation and export
library(flextable)    # For flexible table creation
library(report)       # For generating readable reports from statistical models
library(tidyverse)    # A collection of packages for data manipulation and visualization

# ----- Data Import -----
# Import the data from an Excel file
Data <- read_rds("Data/AMR_KAP_Processed.rds")
View(Data)    # View the data in a new tab to explore the structure and contents

# ----- Table 1: Demographic Characteristics of Study Participants -----
# Create a summary table of the first 11 columns (assumed to be demographic characteristics)
Data |> 
  select(1:11) |>                      # Select the first 11 columns from the dataset
  tbl_summary() |>                     # Create a summary table
  as_gt() |>                           # Convert the table to gt format
  gtsave("table/Table1.Demographic characteristics of study participants (N=704).docx")  # Save the table as a Word document

# ----- Table 2: Major Sources of Information about Antibiotics among Parents -----
# Create a summary table for the columns that contain information about the sources of antibiotic information (assumed to be columns 46 to 50)

Data |> 
  select(47:55) |>               # Select columns 46 to 50, assumed to contain information about sources of antibiotic information
  tbl_summary() |>             # Create a summary table using the tbl_summary function
  as_gt() |>                     # Convert the summary table into a gt table format
  gtsave("table/Table 2.Major sources of information about antibiotic parents.docx")     # Save the table as a Word document

# ----- Table 3: Level of Knowledge, Attitudes, and Practices Towards Antibiotic Resistance -----
# Create groups for KnowledgePCT, AttitudePCT, and PracticePCT and add them as new variables

# KnowledgePCT grouping into Knowledge_Level
Data <- Data |> 
  mutate(Knowledge_Level = case_when(
    KnowledgePCT < 39 ~ "Poor",                      # 'Poor' if KnowledgePCT is less than 39
    KnowledgePCT >= 39 & KnowledgePCT <= 59 ~ "Moderate",  # 'Moderate' if KnowledgePCT is between 39 and 59
    KnowledgePCT > 59 ~ "Good"                       # 'Good' if KnowledgePCT is greater than 59
  ))

# AttitudePCT grouping into Attitude_Level
Data <- Data |> 
  mutate(Attitude_Level = case_when(
    AttitudePCT <= 49 ~ "Negative",                  # 'Negative' if AttitudePCT is 49 or less
    AttitudePCT > 49 & AttitudePCT < 79 ~ "Uncertain",  # 'Uncertain' if AttitudePCT is between 49 and 79
    AttitudePCT >= 79 ~ "Positive"                   # 'Positive' if AttitudePCT is 79 or greater
  ))

# PracticePCT grouping into Practice_Level
Data <- Data |> 
  mutate(Practice_Level = case_when(
    PracticePCT <= 79 ~ "Misuse",                    # 'Misuse' if PracticePCT is 79 or less
    PracticePCT > 79 ~ "Good"                        # 'Good' if PracticePCT is greater than 79
  ))

# ----- Save Grouped Data for Further Analysis -----
# Export the modified dataset (with new groupings) to an Excel file
library(writexl)    # Load the package to write Excel files
write_xlsx(Data, "Data/AMP_KAP Group data.xlsx")    # Save the data as an Excel file

# ----- Summary Table of Knowledge, Attitude, and Practice Levels -----
# Create a summary table for the Knowledge_Level, Attitude_Level, and Practice_Level
Data |> 
  select(Knowledge_Level, Attitude_Level, Practice_Level) |>  # Select the columns for Knowledge, Attitude, and Practice
  tbl_summary() |>                                            # Create a summary table
  as_gt() |>                                                  # Convert the table to gt format
  gtsave("table/Table 3. Level of knowledge, attitudes, and practices.docx")  # Save the table as a Word document

