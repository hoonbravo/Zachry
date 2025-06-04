# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)

# Load the data
data <- read_excel("0_Nov7JN.xlsx", sheet = "Initial")
student_info <- read_excel("0_Current Student Info for Dr. Yoon - gender and majors.xlsx", sheet = 1)

# Combine "First Name" and "Last Name" into a single "Full_Name" column
student_info <- student_info %>%
  mutate(Full_Name = paste(`First Name`, `Last Name`))

# Specify the range for columns E to BP (5 to 68)
name_columns <- colnames(data)[5:68]

# Extract names from the second row of each column (text after the last hyphen)
extracted_names <- sapply(data[1, name_columns], function(cell) {
  sub(".*You can also skip the row if it is you\\. -\\s*", "", cell)
})

# Rename columns 5 to 68 with the extracted names
colnames(data)[5:68] <- rep(extracted_names[1:32], times = 2)

# Define the student identifier column and select columns 5 to 36 for ties (adjusted for 32 students)
student_column <- "Q2"
ties_data <- data %>% select(all_of(student_column), all_of(colnames(data)[5:36]))

# Remove the first row from `ties_data`
ties_data <- ties_data[-1, ]

# Initialize empty data frames for each type of tie (adjusted for 32 students, 19x32 tables)
problem_solving <- matrix(NA, nrow = nrow(ties_data), ncol = 32)
advice_seeking <- matrix(NA, nrow = nrow(ties_data), ncol = 32)
socialization <- matrix(NA, nrow = nrow(ties_data), ncol = 32)
receiving_energy <- matrix(NA, nrow = nrow(ties_data), ncol = 32)

# Use extracted column names as target names
target_names <- colnames(ties_data)[2:33]  # Adjusted to include all 32 students
colnames(problem_solving) <- target_names
colnames(advice_seeking) <- target_names
colnames(socialization) <- target_names
colnames(receiving_energy) <- target_names

# Populate each tie type table
for (i in 1:nrow(ties_data)) {
  for (j in 2:33) {  # Adjusted to process columns 2 to 33 for 32 students
    tie_response <- ties_data[i, j]  # Current response in Q9 columns
    
    # Check if the response is not NA
    if (!is.na(tie_response)) {
      # Split the responses if there are multiple ties in a single cell (e.g., "1,2,3,4")
      tie_types <- unlist(strsplit(as.character(tie_response), ","))
      
      # Populate each matrix based on the identified tie types
      if ("1" %in% tie_types) { problem_solving[i, j - 1] <- 1 }
      if ("2" %in% tie_types) { advice_seeking[i, j - 1] <- 1 }
      if ("3" %in% tie_types) { socialization[i, j - 1] <- 1 }
      if ("4" %in% tie_types) { receiving_energy[i, j - 1] <- 1 }
    }
  }
}

# Convert matrices to data frames for readability and naming
problem_solving <- as.data.frame(problem_solving)
advice_seeking <- as.data.frame(advice_seeking)
socialization <- as.data.frame(socialization)
receiving_energy <- as.data.frame(receiving_energy)

# Add "Student" column explicitly instead of setting row names
problem_solving <- cbind(Student = ties_data[[student_column]], problem_solving)
advice_seeking <- cbind(Student = ties_data[[student_column]], advice_seeking)
socialization <- cbind(Student = ties_data[[student_column]], socialization)
receiving_energy <- cbind(Student = ties_data[[student_column]], receiving_energy)

# Replace NA with 0 in each tie table
problem_solving[is.na(problem_solving)] <- 0
advice_seeking[is.na(advice_seeking)] <- 0
socialization[is.na(socialization)] <- 0
receiving_energy[is.na(receiving_energy)] <- 0

# Replace UINs with full names in each tie-type table
problem_solving$Student <- student_info$Full_Name[match(problem_solving$Student, student_info$UIN)]
advice_seeking$Student <- student_info$Full_Name[match(advice_seeking$Student, student_info$UIN)]
socialization$Student <- student_info$Full_Name[match(socialization$Student, student_info$UIN)]
receiving_energy$Student <- student_info$Full_Name[match(receiving_energy$Student, student_info$UIN)]

# Define desired order based on target names
desired_order <- target_names

# Check whther they are the same !!
all(desired_order %in% extracted_names[1:32])

# Function to rearrange matrix to match desired order
rearrange_matrix <- function(data, desired_order) {
  data <- data %>%
    filter(Student %in% desired_order) %>%
    arrange(match(Student, desired_order))
  data <- data %>% select(Student, all_of(desired_order))
  missing_students <- setdiff(desired_order, data$Student)
  for (student in missing_students) {
    data <- add_row(data, Student = student, .before = match(student, desired_order))
  }
  data[is.na(data)] <- 0
  return(data)
}

# Rearrange each matrix
problem_solving <- rearrange_matrix(problem_solving, desired_order)
advice_seeking <- rearrange_matrix(advice_seeking, desired_order)
socialization <- rearrange_matrix(socialization, desired_order)
receiving_energy <- rearrange_matrix(receiving_energy, desired_order)

# Initialize workbook
wb <- createWorkbook()

# Define the student identifier column and select columns for "Leadership Emergence"
ties_data_leadership <- data %>% select(student_column, 37:68)
ties_data_leadership <- ties_data_leadership[-1, ]
leadership_emergence <- as.data.frame(ties_data_leadership)
leadership_emergence <- cbind(Student = ties_data[[student_column]], leadership_emergence)
leadership_emergence[is.na(leadership_emergence)] <- 0
leadership_emergence$Student <- student_info$Full_Name[match(leadership_emergence$Student, student_info$UIN)]
leadership_emergence <- leadership_emergence[,-2]
leadership_emergence <- rearrange_matrix(leadership_emergence, desired_order)
leadership_emergence[-1] <- lapply(leadership_emergence[-1], as.numeric)

# Define a function to remove self-designations
remove_self_designations <- function(data) {
  for (i in 1:nrow(data)) {
    student_name <- data$Student[i]
    if (student_name %in% colnames(data)) {
      data[i, student_name] <- 0
    }
  }
  return(data)
}

# Apply the function to each tie-type table and leadership emergence table
problem_solving <- remove_self_designations(problem_solving)
advice_seeking <- remove_self_designations(advice_seeking)
socialization <- remove_self_designations(socialization)
receiving_energy <- remove_self_designations(receiving_energy)
leadership_emergence <- remove_self_designations(leadership_emergence)

# Prepare the Attribute table
questionnaire_data <- data %>% select(Q2, Q34_1:Q34_16, Q35_1:Q35_5)
questionnaire_data <- questionnaire_data[-1,]
colnames(questionnaire_data)[colnames(questionnaire_data) == "Q2"] <- "Student"
questionnaire_data[-1] <- lapply(questionnaire_data[-1], as.numeric)
questionnaire_data <- questionnaire_data %>%
  rowwise() %>%
  mutate(
    Sum_Q34 = sum(c_across(Q34_1:Q34_16), na.rm = TRUE),
    Sum_Q35 = sum(c_across(Q35_1:Q35_5), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  rename(
    Growth_Mindset = Sum_Q35,
    Emotional_Intelligence = Sum_Q34
  )

questionnaire_data$Student <- student_info$Full_Name[match(questionnaire_data$Student, student_info$UIN)]
questionnaire_data <- as.data.frame(questionnaire_data)
questionnaire_data <- data.frame(Student = desired_order) %>%
  left_join(questionnaire_data, by = "Student") %>%
  mutate(across(c(Growth_Mindset, Emotional_Intelligence), ~ replace_na(., 0)))
questionnaire_data$Gender <- ifelse(student_info$Gender[31:62] == "M", 1, 0)

# Generate anonymous IDs
set.seed(123)
anonymized_ids <- sprintf("%09d", sample(1e8:1e9 - 1, length(desired_order), replace = FALSE))
colnames(problem_solving)[-1] <- anonymized_ids
colnames(advice_seeking)[-1] <- anonymized_ids
colnames(socialization)[-1] <- anonymized_ids
colnames(receiving_energy)[-1] <- anonymized_ids
colnames(leadership_emergence)[-1] <- anonymized_ids
# colnames(questionnaire_data) <- c("Student", "Growth_Mindset", "Emotional_Intelligence", "Gender")
questionnaire_data$Student <- anonymized_ids
problem_solving$Student <- anonymized_ids
advice_seeking$Student <- anonymized_ids
socialization$Student <- anonymized_ids
receiving_energy$Student <- anonymized_ids
leadership_emergence$Student <- anonymized_ids

# Add each table to the workbook
addWorksheet(wb, "problem_solving")
writeData(wb, "problem_solving", problem_solving)

addWorksheet(wb, "advice_seeking")
writeData(wb, "advice_seeking", advice_seeking)

addWorksheet(wb, "socialization")
writeData(wb, "socialization", socialization)

addWorksheet(wb, "receiving_energy")
writeData(wb, "receiving_energy", receiving_energy)

addWorksheet(wb, "leadership_emergence")
writeData(wb, "leadership_emergence", leadership_emergence)

addWorksheet(wb, "Attribute")
writeData(wb, "Attribute", questionnaire_data)

## SAVE AND FINISH!!!!!!
saveWorkbook(wb, "Responses_Final.xlsx", overwrite = TRUE)

