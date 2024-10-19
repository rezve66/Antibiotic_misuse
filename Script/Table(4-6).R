# Load the required libraries for data manipulation, modeling, and table creation
library(gtsummary)    # For creating summary tables of model results
library(psych)        # For various statistical functions
library(readxl)       # To import Excel files
library(gt)           # For creating and exporting tables
library(flextable)    # To create customizable tables
library(report)       # To generate reports of statistical models
library(tidyverse)    # Collection of packages for data manipulation and visualization

# ----- Data Import and Preparation -----

# Import data from an Excel file
Data <- read_excel("Data/AMP_KAP Group data.xlsx")

# Create a new variable 'Knowledge_Level_Coded' by recoding the percentage values for knowledge
# Categories: 0 = below 50%, 1 = between 50% and 80%, 2 = above 80%
data <- Data |> 
  mutate(Knowledge_Level_Coded = case_when(
    KnowledgePCT < 50 ~ 0,                         # Code as '0' if KnowledgePCT is less than 50%
    KnowledgePCT >= 50 & KnowledgePCT < 80 ~ 1,    # Code as '1' if KnowledgePCT is between 50% and 80%
    KnowledgePCT >= 80 ~ 2                         # Code as '2' if KnowledgePCT is greater than or equal to 80%
  ))

# ----- Multivariable Model for Knowledge -----

# Run a logistic regression model to assess factors associated with knowledge level
# Independent variables: Parent’s age, sex, education, employment status, family type, income, child’s sex, age, number of children
mv_model <- glm(Knowledge_Level_Coded ~ `Parent’s age (years)` + `Parent’s sex` + 
                  `Parent’s education level` + `Employment status` + `Family type` + 
                  `Your average household income per month (BDT)` + `Child’s sex` +
                  `Child’s age (years)` + `Number of children`, 
                data = data)

# Display a summary of the model
summary(mv_model)

# Generate a report of the model
report(mv_model)

# ----- Create Regression Table for Knowledge Model -----

# Create a regression table for the model with odds ratios (ORs), bold p-values, and custom headers
mv_model |>  
  tbl_regression(exponentiate = TRUE) |>          # Display odds ratios (OR) by exponentiating the coefficients
  modify_header(estimate = "**OR**") |>           # Modify the header of the OR column
  bold_p(t = 0.05) |>                             # Bold p-values less than 0.05
  as_gt() |>                                      # Convert to a gt table format
  gtsave("table/Table 4. Factors associated with the level of knowledge among parents of school-going children.docx")  # Save the table as a Word document


# ----- Data Preparation for Attitude -----

# Create a new variable 'Attitude_Level_Coded' by recoding the percentage values for attitude
# Categories: 0 = below 50%, 1 = between 50% and 80%, 2 = above 80%
data <- Data |> 
  mutate(Attitude_Level_Coded = case_when(
    AttitudePCT < 50 ~ 0,                         # Code as '0' if AttitudePCT is less than 50%
    AttitudePCT >= 50 & AttitudePCT < 80 ~ 1,     # Code as '1' if AttitudePCT is between 50% and 80%
    AttitudePCT >= 80 ~ 2                         # Code as '2' if AttitudePCT is greater than or equal to 80%
  ))

# ----- Multivariable Model for Attitude -----

# Run a logistic regression model to assess factors associated with attitude level
mv_model <- glm(Attitude_Level_Coded ~ `Parent’s age (years)` + `Parent’s sex` + 
                  `Parent’s education level` + `Employment status` + `Family type` + 
                  `Your average household income per month (BDT)` + `Child’s sex` + 
                  `Child’s age (years)` + `Number of children`, 
                data = data)

# Display a summary of the model
summary(mv_model)

# Generate a report of the model
report(mv_model)

# ----- Create Regression Table for Attitude Model -----

# Create a regression table for the model with odds ratios (ORs), bold p-values, and custom headers
mv_model |> 
  tbl_regression(exponentiate = TRUE) |>          # Display odds ratios (OR) by exponentiating the coefficients
  modify_header(estimate = "**OR**") |>           # Modify the header of the OR column
  bold_p(t = 0.05) |>                             # Bold p-values less than 0.05
  as_gt() |>                                      # Convert to a gt table format
  gtsave("table/Table 5. Factors associated with the level of attitudes towards antibiotic resistance among parents.docx")  # Save the table as a Word document


# ----- Data Preparation for Practice -----

# Create a new variable 'Practice_Level_Coded' by recoding the percentage values for practice
# Categories: 0 = below 50%, 1 = between 50% and 80%, 2 = above 80%
data <- Data |> 
  mutate(Practice_Level_Coded = case_when(
    PracticePCT < 50 ~ 0,                         # Code as '0' if PracticePCT is less than 50%
    PracticePCT >= 50 & PracticePCT < 80 ~ 1,     # Code as '1' if PracticePCT is between 50% and 80%
    PracticePCT >= 80 ~ 2                         # Code as '2' if PracticePCT is greater than or equal to 80%
  ))

# ----- Multivariable Model for Practice -----

# Run a logistic regression model to assess factors associated with practice level
mv_model <- glm(Practice_Level_Coded ~ `Parent’s age (years)` + `Parent’s sex` + 
                  `Parent’s education level` + `Employment status` + `Family type` + 
                  `Your average household income per month (BDT)` + `Child’s sex` + 
                  `Child’s age (years)` + `Number of children`, 
                data = data)

# Display a summary of the model
summary(mv_model)

# Generate a report of the model
report(mv_model)

# ----- Create Regression Table for Practice Model -----

# Create a regression table for the model with odds ratios (ORs), bold p-values, and custom headers
mv_model |> 
  tbl_regression(exponentiate = TRUE) |>          # Display odds ratios (OR) by exponentiating the coefficients
  modify_header(estimate = "**OR**") |>           # Modify the header of the OR column
  bold_p(t = 0.05) |>                             # Bold p-values less than 0.05
  as_gt() |>                                      # Convert to a gt table format
  gtsave("table/Table 6. Factors associated with the level of practices regarding antibiotic resistance among parents.docx")  # Save the table as a Word document








