
# Install necessary R packages for plotting, Likert scale analysis, and visualization
install.packages("RColorBrewer")  # Provides color palettes for R
install.packages("likert")        # For Likert scale visualization
install.packages("ggthemes")      # Additional themes for ggplot
install.packages("ggpubr")        # Publication-ready ggplot themes and functions

# Load required libraries
library(tidyverse)       # Collection of packages for data manipulation and visualization
library(likert)          # Functions for working with Likert scale data
library(ggpubr)          # Functions for publication-ready plots
library(ggthemes)        # Additional themes for ggplot
library(RColorBrewer)    # Color palettes
library(readxl)          # For reading Excel files

# Import the dataset from the Excel file
data <- read_excel("Data/AMR_KAP_Data.xlsx")

# ----- Visualize Knowledge Responses -----

# Select columns 12 to 23 that contain knowledge-related questions
# Convert character data into factors (necessary for Likert scale plotting)
Knowledge <- data |> 
  select(12:23) |>                # Select columns 12 to 23 (Knowledge section)
  mutate_if(is.character,as.factor) |>  # Convert character columns to factors
  as.data.frame()                 # Convert the tibble to a data frame

# Create a Likert plot for the knowledge-related questions
# Disable ordered responses and define the center at '2'
p1 <- plot(likert(Knowledge),
           ordered = FALSE,        # Do not order the responses
           group.order = names(Knowledge),  # Grouping order by question names
           center = 2)             # Center the responses at value '2'

# Add a publication-ready theme to the plot
p1 + theme_pubr()

# Display the plot
p1

# Export the plot as a TIFF file with high resolution
ggsave("figures/Figure 1. Distribution of knowledge of antibiotic resistance among parents of school-going children.tiff",
       units = "in", width = 12, height = 6, dpi = 300, compression = 'lzw')


# ----- Visualize Attitude Responses -----

# Re-import the dataset (this step can be skipped if 'data' is already loaded)
data <- read_excel("Data/AMR_KAP_Data.xlsx")

# Select columns 24 to 33 that contain attitude-related questions
# Convert character data into factors for Likert scale plotting
Attitude <- data |> 
  select(24:33) |>                # Select columns 24 to 33 (Attitude section)
  mutate_if(is.character,as.factor) |>  # Convert character columns to factors
  as.data.frame()                 # Convert the tibble to a data frame

# Create a Likert plot for the attitude-related questions
# Disable ordered responses and define the center at '2'
p1 <- plot(likert(Attitude),
           ordered = FALSE,        # Do not order the responses
           group.order = names(Attitude),  # Grouping order by question names
           center = 2)             # Center the responses at value '2'

# Add a publication-ready theme to the plot
p1 + theme_pubr()

# Display the plot
p1

# Export the plot as a TIFF file with high resolution
ggsave("figures/Figure 2.Attitude towards antibiotic resistance.tiff",
       units = "in", width = 12, height = 6, dpi = 300, compression = 'lzw')


# ----- Visualize Practice Responses -----

# Select columns 34 to 39 that contain practice-related questions
# Convert character data into factors for Likert scale plotting
Practice <- data |> 
  select(34:39) |>                # Select columns 34 to 39 (Practice section)
  mutate_if(is.character,as.factor) |>  # Convert character columns to factors
  as.data.frame()                 # Convert the tibble to a data frame

# Create a Likert plot for the practice-related questions
# Disable ordered responses and do not define a specific center
p1 <- plot(likert(Practice),
           ordered = FALSE,        # Do not order the responses
           group.order = names(Practice))  # Grouping order by question names

# Add a publication-ready theme to the plot
p1 + theme_pubr()

# Display the plot
p1

# Export the plot as a TIFF file with high resolution
ggsave("figures/Figure 3. Practices among parents of school-going children regarding antibiotic resistance.tiff",
       units = "in", width = 12, height = 6, dpi = 300, compression = 'lzw')






