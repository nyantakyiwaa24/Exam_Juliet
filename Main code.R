# Install required packages
install.packages ("dplyr")
install.packages ("ggplot2")
install.packages ("lubridate")
install.packages ("stringr")
install.packages ("tools")
install.packages ("shinydashboard")
install.packages ("plotly")
install.packages ("scales")
install.packages ("DT")
install.packages ("shiny")
install.packages ("readr")
install.packages ("ggrepel")

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tools)
library(shinydashboard)
library(plotly)
library(scales)
library(DT)
library(shiny)
library(readr)
library(ggrepel)

# Get the working directory
getwd()

# Load the dataset
data <- read.csv("healthcare_dataset.csv")
View(data)



# INSPECTING THE DATA FRAME STRUCTURE:

# Concise summary of the data frame
str(data)

# Check for the data type of each column
sapply(data, class)

# Check the head of the data frame
head(data)

# Check the summary statistics for each column
summary(data)

# Lists all the column names
colnames(data)

# Check for null values in the entire data frame
is.na(data)

# Count the missing values in the entire data frame
sum(is.na(data))

# Get all the values in a single column
data$Billing.Amount


print(data$Billing.Amount, max = 60000) # To get all the values without limits

# Get the unique values of specific columns in the data 
columns_of_interest <- c("Gender", "Blood.Type", "Medical.Condition", 
                         "Insurance.Provider", "Admission.Type", 
                         "Medication", "Test.Results")

unique_values <- lapply(data[columns_of_interest], unique)

unique_values

# Check the items in the Name Column
head(data$Name, 100)  # View the first 100 entries
tail(data$Name, 100)  # View the last 100 entries

# Check for the duplicates and the sum of duplicates in the data frame
duplicated(data)
sum(duplicated(data))



# DATA CLEANING: 

data <- data %>% distinct() # Remove duplicates in the entire data frame
data$Name <- str_to_title(trimws(data$Name)) # To Standardize the names in the Name columns
data$Date.of.Admission <- as.Date(data$Date.of.Admission, format = "%Y-%m-%d") # Change to Date format
data$Discharge.Date <- as.Date(data$Discharge.Date, format = "%Y-%m-%d") # Change to Date format
data$Gender <- factor(data$Gender, levels = c("Male", "Female"))  # Gender as a factor

# Recheck to see that all changes has been made:
# Sum(duplicated(data))
# data$Name
# summary(data)


# Data cleaning of the Hospital Column:

data$Hospital <- data$Hospital %>%
  tolower() %>%                        # Convert to lowercase
  trimws() %>%                         # Remove leading and trailing whitespace
  gsub("\\s+", " ", .) %>%             # Replace multiple spaces with a single space
  gsub("\\bhospital\\b", "Hospital", .) %>%  # Standardize the word 'hospital'
  tools::toTitleCase() %>%             # Capitalize the first letter of each word
  gsub("^(llc|plc|inc|ltd)\\s+(.*)", "\\2 \\1", ., ignore.case = TRUE) %>%  # Move LLC, PLC, Inc, Ltd to the end
  gsub("\\band\\b", "And", ., ignore.case = TRUE) %>%  # Standardize 'and' to proper capitalization
  gsub("^And\\s+", "", .) %>%                   # Remove 'And' if it is at the start
  gsub("\\s+And$", "", .) %>%                   # Remove 'And' if it is at the end
  gsub("^Ltd\\s+", "", ., ignore.case = TRUE) %>% # Remove 'Ltd' if it is at the start
  gsub("\\s+Ltd$", " Ltd", ., ignore.case = TRUE) %>% # Ensure 'Ltd' is correctly placed at the end
  gsub("\\s+And\\s+", " and ", .) %>%           # Ensure correct placement of 'and' within the names
  gsub(",\\s*$", "", .)                         # Remove trailing commas


# Verify unique hospital names after cleaning
print(unique(data$Hospital))

print(data$Hospital, max = 60000)

# Proper capitalization of the names in the 'Name', 'Doctor' and 'Hospital' columns
exceptions <- c("and", "or", "of", "the", "a", "in", "for", "to", "by")

# Helper function to apply title case while preserving exceptions
title_case_with_exceptions <- function(column) {
  sapply(column, function(x) {
    words <- unlist(strsplit(tolower(x), " "))  # Split the text into words
    words <- ifelse(words %in% exceptions, words, tools::toTitleCase(words))  # Title case if not in exceptions
    paste(words, collapse = " ")  # Rejoin the words
  })
}

# Clean and modify the columns
data$Name <- title_case_with_exceptions(data$Name)         # Apply to Name column
data$Doctor <- title_case_with_exceptions(data$Doctor)     # Apply to Doctor column
data$Hospital <- title_case_with_exceptions(data$Hospital) # Apply to Hospital column

# Check the result
head(data)


# To add spacing to the 'UnitedHealthcare' in the Insurance.Provider column
data$Insurance.Provider <- gsub("UnitedHealthcare", "United Healthcare", data$Insurance.Provider)


# Check for outliers using boxplot in the Billing Amount
boxplot(data$Billing.Amount, main = "Boxplot to Identify Outliers")
boxplot.stats(data$Billing.Amount)$out



# FEATURE CREATION:


# Going further, I want to group patient ages to better understand patterns among age groups.

# Define age bins and labels
age_bins <- c(0, 18, 35, 50, 65, 100)
age_labels <- c("0-18", "19-35", "36-50", "51-65", "66+")

# Use cut() to create the Age Group column
data$Age.Group <- cut(data$Age, breaks = age_bins, labels = age_labels, right = FALSE)

head(data)


# Create new columns for year, month, day, and quarter of admission
data$Date.of.Admission <- as.Date(data$Date.of.Admission, format = "%Y-%m-%d")

# Extract year, month, quarter, and day
data$Admission.Day <- format(data$Date.of.Admission, "%d")
data$Admission.Month <- format(data$Date.of.Admission, "%m")
data$Admission.Year <- format(data$Date.of.Admission, "%Y")
data$Admission.Quarter <- quarters(data$Date.of.Admission)

head(data)


# Create new columns for year, month, day, and quarter of discharge
data$Discharge.Date <- as.Date(data$Discharge.Date, format = "%Y-%m-%d")

# Extract year, month, quarter, and day
data$Discharge.Day <- format(data$Discharge.Date, "%d")
data$Discharge.Month <- format(data$Discharge.Date, "%m")
data$Discharge.Year <- format(data$Discharge.Date, "%Y")
data$Discharge.Quarter <- quarters(data$Discharge.Date)

head(data)


# Calculate the duration of stay

data$Date.of.Admission <- as.Date(data$Date.of.Admission, format = "%Y-%m-%d")
data$Discharge.Date <- as.Date(data$Discharge.Date, format = "%Y-%m-%d")

# Calculate the duration of stay in days
data$Duration.of.Stay <- as.numeric(difftime(data$Discharge.Date, data$Date.of.Admission, units = "days"))


# To change the data types
data$Name <- as.character(data$Name)  # String
data$Age <- as.integer(data$Age)  # Integer
data$Gender <- as.factor(data$Gender)  # Categorical
data$Blood.Type <- as.factor(data$Blood.Type)  # Categorical
data$Medical.Condition <- as.character(data$Medical.Condition)  # String
data$Date.of.Admission <- as.Date(data$Date.of.Admission, format = "%Y-%m-%d")  # Date
data$Doctor <- as.character(data$Doctor)  # String
data$Hospital <- as.character(data$Hospital)  # String
data$Insurance.Provider <- as.character(data$Insurance.Provider)  # String
data$Billing.Amount <- as.numeric(data$Billing.Amount)  # Numeric
data$Room.Number <- as.integer(data$Room.Number)  # Integer
data$Admission.Type <- as.factor(data$Admission.Type)  # Categorical
data$Discharge.Date <- as.Date(data$Discharge.Date, format = "%Y-%m-%d")  # Date
data$Medication <- as.character(data$Medication)  # String
data$Test.Results <- as.character(data$Test.Results)  # String
data$Age.Group <- as.factor(data$Age.Group)  # Categorical
data$Admission.Day <- as.integer(data$Admission.Day)  # Integer
data$Admission.Month <- as.integer(data$Admission.Month)  # Integer
data$Admission.Year <- as.integer(data$Admission.Year)  # Integer
data$Admission.Quarter <- as.factor(data$Admission.Quarter)  # Categorical
data$Discharge.Day <- as.integer(data$Discharge.Day)  # Integer
data$Discharge.Month <- as.integer(data$Discharge.Month)  # Integer
data$Discharge.Year <- as.integer(data$Discharge.Year)  # Integer
data$Discharge.Quarter <- as.factor(data$Discharge.Quarter)  # Categorical
data$Duration.of.Stay <- as.integer(data$Duration.of.Stay)  # Integer



#Save cleaned data as new file 
write.csv(data, "cleaned_data.csv", row.names = FALSE)
print("Cleaned dataset saved as 'cleaned_data.csv'.")




# VISUALIZATION


# Plot 1:  Total Billing Amount by Age Group
# Aggregate billing amount by Age Group
billing_by_age <- aggregate(Billing.Amount ~ Age.Group, data = data, sum)

# Plot total billing amount by Age Group
ggplot(billing_by_age, aes(x = Age.Group, y = Billing.Amount, fill = Age.Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Billing Amount by Age Group", x = "Age Group", y = "Total Billing Amount") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +  # Use scale and suffix for millions
  theme_minimal()

# Plot 2:  Average Billing Amount by Admission Type
#Aggregate average billing amount by Admission Type
billing_by_admission <- aggregate(Billing.Amount ~ Admission.Type, data = data, mean)

# Plot average billing amount by Admission Type
ggplot(billing_by_admission, aes(x = Admission.Type, y = Billing.Amount, fill = Admission.Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Billing Amount by Admission Type", x = "Admission Type", y = "Average Billing Amount") +
  theme_minimal()


# Plot 3: Billing Amount Distribution
# Histogram of Billing Amount
ggplot(data, aes(x = Billing.Amount)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "white") +
  labs(title = "Distribution of Billing Amount", x = "Billing Amount", y = "Frequency") +
  theme_minimal()


# Plot 4: Total Billing Amount by Insurance Provider
# Aggregate billing amount by Insurance Provider
billing_by_insurance <- aggregate(Billing.Amount ~ Insurance.Provider, data = data, sum)

# Plot total billing amount by Insurance Provider
ggplot(billing_by_insurance, aes(x = reorder(Insurance.Provider, Billing.Amount), y = Billing.Amount, fill = Insurance.Provider)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Billing Amount by Insurance Provider", x = "Insurance Provider", y = "Total Billing Amount") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +  # Format y-axis in millions
  theme_minimal() +
  coord_flip()


# Plot 5: Proportion of Billing Amount by Gender

# Aggregate billing amount by Gender
billing_by_gender <- aggregate(Billing.Amount ~ Gender, data = data, sum)

billing_by_gender <- data %>%
  group_by(Gender) %>%
  summarise(Billing.Amount = sum(Billing.Amount)) %>%
  mutate(Percentage = Billing.Amount / sum(Billing.Amount) * 100)

# Create a pie chart with percentage
pie_chart <- ggplot(billing_by_gender, aes(x = "", y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  labs(title = "Proportion of Billing Amount by Gender (Pie Chart)", x = "", y = "") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")  # Keeping the pie chart by using polar coordinates

pie_chart


# Plot 6: Billing Amount by Admission Year

# Aggregate billing amount by Admission Year
billing_by_year <- aggregate(Billing.Amount ~ Admission.Year, data = data, sum)

# Convert Billing Amount to millions
billing_by_year$Billing.Amount <- billing_by_year$Billing.Amount / 1e6  # Convert to millions

# Plot total billing amount by Admission Year
ggplot(billing_by_year, aes(x = Admission.Year, y = Billing.Amount, fill = Admission.Year)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::label_number(scale = 1, suffix = "M")) +  # Format y-axis as millions (M)
  labs(title = "Total Billing Amount by Admission Year", x = "Admission Year", y = "Total Billing Amount (in M)") +
  theme_minimal()


# Plot 7: Gender distribution in medical conditions
condition_gender <- data %>% 
  group_by(Medical.Condition, Gender) %>% 
  summarise(Count = n(), .groups = "drop")

ggplot(condition_gender, aes(x = Medical.Condition, y = Count, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Medical Condition by Gender", x = "Medical Condition", y = "Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Plot 8: Medical Condition by Age Group
condition_gender <- data %>% 
  group_by(Medical.Condition, Age.Group) %>% 
  summarise(Count = n(), .groups = "drop")

ggplot(condition_gender, aes(x = Medical.Condition, y = Count, fill = Age.Group)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Medical Condition by Age Group", x = "Medical Condition", y = "Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot 9:  Most diagnosed sickness
diagnosed_conditions <- data %>%
  group_by(Medical.Condition) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))
print(diagnosed_conditions[1, ])


count_of_conditions <- diagnosed_conditions %>%
  slice_max(order_by = Count, n = 10)

ggplot(count_of_conditions, aes(x = reorder(Medical.Condition, -Count), y = Count, fill = Medical.Condition)) +
  geom_bar(stat = "identity") +
  labs(title = "Diagnosed Sicknesses", x = "Medical Condition", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Plot 10: Top 5 Hospitals by Admission in zear 2023
admissions_2023 <- data %>%
  filter(year(Date.of.Admission) == 2023)

# Count the number of admissions per hospital
hospital_admissions_2023 <- admissions_2023 %>%
  group_by(Hospital) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))

# Get the top 5 hospitals by admissions in 2023
top_5_hospitals <- hospital_admissions_2023 %>%
  slice_max(order_by = Count, n = 5)

# Plot the top 5 hospitals by admissions
ggplot(top_5_hospitals, aes(x = reorder(Hospital, -Count), y = Count, fill = Hospital)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Hospitals by Admissions in 2023", x = "Hospital", y = "Number of Admissions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# COST ANALYSIS FOR ELDERLY PEOPLE (66+):

#1. Most Common Medical Conditions by Elderly People


# Filter data for elderly people
data_66_plus <- subset(data, Age.Group == "66+")

# Count the most common medical conditions
common_conditions <- data_66_plus %>%
  group_by(Medical.Condition) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Plot
ggplot(common_conditions, aes(x = reorder(Medical.Condition, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "gold4") +
  labs(title = "Most Common Medical Conditions Among Elderly People",
       x = "Medical Condition",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 2. Frequency of Duration of Stay Among Elderly People

# Plot duration of stay distribution
ggplot(data_66_plus, aes(x = Duration.of.Stay)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "honeydew4") +
  labs(title = "Frequency of Duration of Stay Among Elderly People",
       x = "Duration of Stay (Days)",
       y = "Frequency") +
  theme_minimal()



# 3. Billing Amount Distribution by Medical Condition Among Elderly People

# Plot billing amount distribution by medical condition
ggplot(data_66_plus, aes(x = Medical.Condition, y = Billing.Amount)) +
  geom_boxplot(fill = "orange", outlier.color = "red") +
  labs(title = "Billing Amount Distribution by Medical Condition (Elderly)",
       x = "Medical Condition",
       y = "Billing Amount") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 4. Which Medication Cost Elderly People the Most

# Calculate total cost by medication
medication_costs <- data_66_plus %>%
  group_by(Medication) %>%
  summarise(Total.Cost = sum(Billing.Amount, na.rm = TRUE)) %>%
  arrange(desc(Total.Cost))

# Convert Total.Cost to millions (M)
medication_costs$Total.Cost <- medication_costs$Total.Cost / 1e6

# Plot
ggplot(medication_costs, aes(x = reorder(Medication, -Total.Cost), y = Total.Cost)) +
  geom_bar(stat = "identity", fill = "cyan4") +
  labs(title = "Total Medication Costs for Elderly",
       x = "Medication",
       y = "Total Cost in million (M)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 5. Which Disease Cost Elderly People the Most

# Calculate total cost by medical condition
disease_costs <- data_66_plus %>%
  group_by(Medical.Condition) %>%
  summarise(Total.Cost = sum(Billing.Amount, na.rm = TRUE)) %>%
  arrange(desc(Total.Cost))

# Convert Total.Cost to millions (M)
disease_costs$Total.Cost <- disease_costs$Total.Cost / 1e6

# Plot
ggplot(disease_costs, aes(x = reorder(Medical.Condition, -Total.Cost), y = Total.Cost)) +
  geom_bar(stat = "identity", fill = "firebrick4") +
  labs(title = "Total Disease Costs for Elderly",
       x = "Medical Condition",
       y = "Total Cost in million (M)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 6. Which Disease Did Elderly People Suffer the Most

# Count occurrences by disease
disease_frequency <- data_66_plus %>%
  group_by(Medical.Condition) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Plot
ggplot(disease_frequency, aes(x = reorder(Medical.Condition, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "sienna3") +
  labs(title = "Most Common Diseases Among Elderly People",
       x = "Medical Condition",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

