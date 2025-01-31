# Visualization with R shiny

ui <- fluidPage(
  titlePanel("Healthcare Financial and Demographic Insights"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Select Visualization:",
                  choices = c(
                    "Total Billing Amount by Age Group",
                    "Average Billing Amount by Admission Type",
                    "Distribution of Billing Amount",
                    "Total Billing Amount by Insurance Provider",
                    "Proportion of Billing Amount by Gender",
                    "Total Billing Amount by Admission Year",
                    "Medical Condition by Gender",
                    "Medical Condition by Age Group",
                    "Diagnosed Sicknesses",
                    "Top 5 Hospitals by Admissions in 2023",
                    "Most Common Medical Conditions Among Elderly People",
                    "Frequency of Duration of Stay Among Elderly People",
                    "Billing Amount Distribution by Medical Condition (Elderly)",
                    "Total Medication Costs for Elderly",
                    "Total Disease Costs for Elderly",
                    "Most Common Diseases Among Elderly People"
                  ))
    ),
    
    mainPanel(
      plotOutput("selected_plot")
    )
  )
)

server <- function(input, output) {
  # Load your cleaned data
  data2 <- read.csv("cleaned_data.csv")
  
  
  output$selected_plot <- renderPlot({
    if (input$plot_type == "Total Billing Amount by Age Group") {
      billing_by_age <- aggregate(Billing.Amount ~ Age.Group, data = data2, sum)
      ggplot(billing_by_age, aes(x = Age.Group, y = Billing.Amount, fill = Age.Group)) +
        geom_bar(stat = "identity") +
        labs(title = "Total Billing Amount by Age Group", x = "Age Group", y = "Total Billing Amount") +
        scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
        theme_minimal()
    } else if (input$plot_type == "Average Billing Amount by Admission Type") {
      billing_by_admission <- aggregate(Billing.Amount ~ Admission.Type, data = data2, mean)
      ggplot(billing_by_admission, aes(x = Admission.Type, y = Billing.Amount, fill = Admission.Type)) +
        geom_bar(stat = "identity") +
        labs(title = "Average Billing Amount by Admission Type", x = "Admission Type", y = "Average Billing Amount") +
        theme_minimal()
    } else if (input$plot_type == "Distribution of Billing Amount") {
      ggplot(data2, aes(x = Billing.Amount)) +
        geom_histogram(binwidth = 5000, fill = "blue", color = "white") +
        labs(title = "Distribution of Billing Amount", x = "Billing Amount", y = "Frequency") +
        theme_minimal()
    } else if (input$plot_type == "Total Billing Amount by Insurance Provider") {
      billing_by_insurance <- aggregate(Billing.Amount ~ Insurance.Provider, data = data2, sum)
      ggplot(billing_by_insurance, aes(x = reorder(Insurance.Provider, Billing.Amount), y = Billing.Amount, fill = Insurance.Provider)) +
        geom_bar(stat = "identity") +
        labs(title = "Total Billing Amount by Insurance Provider", x = "Insurance Provider", y = "Total Billing Amount") +
        scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
        theme_minimal() +
        coord_flip()
    } else if (input$plot_type == "Proportion of Billing Amount by Gender") {
      billing_by_gender <- data2 %>%
        group_by(Gender) %>%
        summarise(Billing.Amount = sum(Billing.Amount)) %>%
        mutate(Percentage = Billing.Amount / sum(Billing.Amount) * 100)
      ggplot(billing_by_gender, aes(x = "", y = Percentage, fill = Gender)) +
        geom_bar(stat = "identity", width = 1) +
        labs(title = "Proportion of Billing Amount by Gender (Pie Chart)", x = "", y = "") +
        theme_minimal() +
        geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                  position = position_stack(vjust = 0.5)) +
        coord_polar(theta = "y")
    } else if (input$plot_type == "Total Billing Amount by Admission Year") {
      billing_by_year <- aggregate(Billing.Amount ~ Admission.Year, data = data2, sum)
      billing_by_year$Billing.Amount <- billing_by_year$Billing.Amount / 1e6
      ggplot(billing_by_year, aes(x = Admission.Year, y = Billing.Amount, fill = Admission.Year)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = scales::label_number(scale = 1, suffix = "M")) +
        labs(title = "Total Billing Amount by Admission Year", x = "Admission Year", y = "Total Billing Amount (in M)") +
        theme_minimal()
    } else if (input$plot_type == "Medical Condition by Gender") {
      condition_gender <- data2 %>% 
        group_by(Medical.Condition, Gender) %>% 
        summarise(Count = n(), .groups = "drop")
      ggplot(condition_gender, aes(x = Medical.Condition, y = Count, fill = Gender)) + 
        geom_bar(stat = "identity", position = "dodge") + 
        labs(title = "Medical Condition by Gender", x = "Medical Condition", y = "Count") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$plot_type == "Medical Condition by Age Group") {
      condition_age <- data2 %>% 
        group_by(Medical.Condition, Age.Group) %>% 
        summarise(Count = n(), .groups = "drop")
      ggplot(condition_age, aes(x = Medical.Condition, y = Count, fill = Age.Group)) + 
        geom_bar(stat = "identity", position = "dodge") + 
        labs(title = "Medical Condition by Age Group", x = "Medical Condition", y = "Count") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$plot_type == "Diagnosed Sicknesses") {
      diagnosed_conditions <- data2 %>%
        group_by(Medical.Condition) %>%
        summarise(Count = n(), .groups = "drop") %>%
        arrange(desc(Count))
      count_of_conditions <- diagnosed_conditions %>%
        slice_max(order_by = Count, n = 10)
      ggplot(count_of_conditions, aes(x = reorder(Medical.Condition, -Count), y = Count, fill = Medical.Condition)) +
        geom_bar(stat = "identity") +
        labs(title = "Diagnosed Sicknesses", x = "Medical Condition", y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$plot_type == "Top 5 Hospitals by Admissions in 2023") {
      admissions_2023 <- data2 %>%
        filter(year(Date.of.Admission) == 2023)
      hospital_admissions_2023 <- admissions_2023 %>%
        group_by(Hospital) %>%
        summarise(Count = n(), .groups = "drop") %>%
        arrange(desc(Count))
      top_5_hospitals <- hospital_admissions_2023 %>%
        slice_max(order_by = Count, n = 5)
      ggplot(top_5_hospitals, aes(x = reorder(Hospital, -Count), y = Count, fill = Hospital)) +
        geom_bar(stat = "identity") +
        labs(title = "Top 5 Hospitals by Admissions in 2023", x = "Hospital", y = "Number of Admissions") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$plot_type == "Most Common Medical Conditions Among Elderly People") {
      data_66_plus <- subset(data2, Age.Group == "66+")
      common_conditions <- data_66_plus %>%
        group_by(Medical.Condition) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count))
      ggplot(common_conditions, aes(x = reorder(Medical.Condition, -Count), y = Count)) +
        geom_bar(stat = "identity", fill = "gold4") +
        labs(title = "Most Common Medical Conditions Among Elderly People",
             x = "Medical Condition",
             y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$plot_type == "Frequency of Duration of Stay Among Elderly People") {
      data_66_plus <- subset(data2, Age.Group == "66+")
      ggplot(data_66_plus, aes(x = Duration.of.Stay)) +
        geom_histogram(binwidth = 1, fill = "lightgreen", color = "honeydew4") +
        labs(title = "Frequency of Duration of Stay Among Elderly People",
             x = "Duration of Stay (Days)",
             y = "Frequency") +
        theme_minimal()
    } else if (input$plot_type == "Billing Amount Distribution by Medical Condition (Elderly)") {
      data_66_plus <- subset(data2, Age.Group == "66+")
      ggplot(data_66_plus, aes(x = Medical.Condition, y = Billing.Amount)) +
        geom_boxplot(fill = "orange", outlier.color = "red") +
        labs(title = "Billing Amount Distribution by Medical Condition (Elderly)",
             x = "Medical Condition",
             y = "Billing Amount") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$plot_type == "Total Medication Costs for Elderly") {
      data_66_plus <- subset(data2, Age.Group == "66+")
      medication_costs <- data_66_plus %>%
        group_by(Medication) %>%
        summarise(Total.Cost = sum(Billing.Amount, na.rm = TRUE)) %>%
        arrange(desc(Total.Cost))
      medication_costs$Total.Cost <- medication_costs$Total.Cost / 1e6
      ggplot(medication_costs, aes(x = reorder(Medication, -Total.Cost), y = Total.Cost)) +
        geom_bar(stat = "identity", fill = "cyan4") +
        labs(title = "Total Medication Costs for Elderly",
             x = "Medication",
             y = "Total Cost in million (M)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$plot_type == "Total Disease Costs for Elderly") {
      data_66_plus <- subset(data2, Age.Group == "66+")
      disease_costs <- data_66_plus %>%
        group_by(Medical.Condition) %>%
        summarise(Total.Cost = sum(Billing.Amount, na.rm = TRUE)) %>%
        arrange(desc(Total.Cost))
      disease_costs$Total.Cost <- disease_costs$Total.Cost / 1e6
      ggplot(disease_costs, aes(x = reorder(Medical.Condition, -Total.Cost), y = Total.Cost)) +
        geom_bar(stat = "identity", fill = "firebrick4") +
        labs(title = "Total Disease Costs for Elderly",
             x = "Medical Condition",
             y = "Total Cost in million (M)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$plot_type == "Most Common Diseases Among Elderly People") {
      data_66_plus <- subset(data2, Age.Group == "66+")
      disease_frequency <- data_66_plus %>%
        group_by(Medical.Condition) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count))
      ggplot(disease_frequency, aes(x = reorder(Medical.Condition, -Count), y = Count)) +
        geom_bar(stat = "identity", fill = "sienna3") +
        labs(title = "Most Common Diseases Among Elderly People",
             x = "Medical Condition",
             y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

shinyApp(ui = ui, server = server)
         