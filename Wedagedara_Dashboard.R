library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(ggplot2)

# Load your data
data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")


# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Employee Attrition Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Visualizations", tabName = "visualizations", icon = icon("plot")),
      menuItem("Logistic Regression", tabName = "logistic", icon = icon("dspace")),
      menuItem("Summary", tabName = "summary", icon = icon("file"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        #overview .content-wrapper {
          background-image: url('https://traqq.com/blog/wp-content/uploads/2020/11/shutterstock_1696263217_edited-1024x512.jpg');
          background-size: cover;
          background-position: center center;
          background-repeat: no-repeat;
        }
        .content-wrapper {
          background-color: #e7f4ff; /* Default background color for other tabs */
        }
        .box, .small-box, .navbar, .sidebar {
          background-color: rgba(255, 255, 255, 0.8) !important;
          color: #000 !important;
          font-weight: bold;
          border-radius: 15px; /* Rounded corners for boxes */
          box-shadow: 0px 0px 10px 0px rgba(0,0,0,0.15); /* Soft shadow for depth */
        }
        .big-title {
          font-size: 36px;
          font-weight: bold;
          text-align: center;
          margin-top: 20px;
          margin-bottom: 20px;
        }
        .key-point-box {
          background: rgba(0, 0, 0, 0.5); /* Semi-transparent black for text readability */
          padding: 10px;
          border-radius: 10px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
          font-size: 16px;
          text-align: left; /* Aligns text to the left */
          color: #fff;
          margin: 10px 0; /* Vertical spacing */
          width: 100%; /* Full width to spread across the container */
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                div(class = "big-title", "Employee Attrition Dashboard"),
                box(title = "Welcome", status = "primary", solidHeader = TRUE,
                    "Explore insights into factors contributing to employee attrition.",
                    width = 12),
                box(title = "Dataset Snapshot", status = "info", solidHeader = TRUE,
                    DTOutput("dataHead"), width = 12)
              )
      ),
      tabItem(tabName = "visualizations",
              fluidRow(
                selectInput("department", "Choose a Department:", choices = unique(data$Department)),
                selectInput("ageGroup", "Select Age Group:", choices = c("Under 30", "30-50", "Over 50")),
                box(title = "Demographic Breakdown", plotlyOutput("demographicPlot"), width = 6),
                box(title = "Salary and Compensation", plotlyOutput("salaryPlot"), width = 6),
                box(title = "Attrition by Job Role", plotlyOutput("attritionByRolePlot"), width = 6),
                box(title = "Heat Map of Correlations", plotlyOutput("correlationHeatmap"), width = 6),
                box(title = "3D Work Environment Factors", plotlyOutput("environmentPlot3D"), width = 12)
              )
      ),
      tabItem(tabName = "logistic",
              fluidRow(
                box(title = "Logistic Regression Model Explanation", plotlyOutput("logisticPlot"), width = 12)
              )
      ),
      tabItem(tabName = "summary",
              fluidRow(
                div(class="key-point-box", "1. Age and Retention: Older employees show lower attrition probability, indicating experience and tenure boost retention."),
                div(class="key-point-box", "2. Job Satisfaction's Impact: Higher satisfaction reduces attrition likelihood, highlighting its significance for retention strategies."),
                div(class="key-point-box", "3. Financial Stability and Tenure: Monthly income and tenure negatively correlate with attrition, emphasizing their importance for employee commitment."),
                div(class="key-point-box", "4. Model Accuracy: Achieved 84.5% and 82.4% accuracy on training and validation sets, indicating strong generalization capability."),
                div(class="key-point-box", "5. Effective Visualization: Visualizations aided data exploration and model validation.")
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$dataHead <- renderDataTable({
    datatable(data)
  })
  output$demographicPlot <- renderPlotly({
    req(input$department)
    df <- data %>% filter(Department == input$department)
    plot_ly(df, x = ~Department, y = ~Age, color = ~Attrition, type = 'bar', colors = c('#1f77b4'))
  })
  output$salaryPlot <- renderPlotly({
    req(input$ageGroup)
    df <- data %>% filter(Age %in% getAgeRange(input$ageGroup))
    plot_ly(df, x = ~Attrition, y = ~MonthlyIncome, type = 'box', colors = c('#1f77b4'))
  })
  output$attritionByRolePlot <- renderPlotly({
    req(input$department, input$ageGroup)
    df <- data %>% filter(Department == input$department, Age %in% getAgeRange(input$ageGroup))
    plot_ly(df, x = ~JobRole, y = ~MonthlyIncome, color = ~Attrition, type = 'scatter', mode = 'markers', marker = list(color = '#1f77b4'))
  })
  output$correlationHeatmap <- renderPlotly({
    df <- data %>% filter(Department == input$department)
    cor_data <- cor(df[,sapply(df, is.numeric)], use = "complete.obs")
    plot_ly(x = colnames(cor_data), y = colnames(cor_data), z = cor_data, type = 'heatmap', colorscale = 'Blues')
  })
  output$environmentPlot3D <- renderPlotly({
    df <- data %>% filter(Department == input$department)
    plot_ly(df, x = ~JobSatisfaction, y = ~MonthlyIncome, z = ~Age, color = ~Attrition, type = 'scatter3d', mode = 'markers')
  })
  output$logisticPlot <- renderPlotly({
    df <- data %>% filter(Department == input$department)
    model <- glm(Attrition ~ Age + MonthlyIncome + JobSatisfaction, data = df, family = binomial())
    effect <- seq(min(df$Age), max(df$Age), length.out = 100)
    pred <- predict(model, newdata = data.frame(Age = effect, MonthlyIncome = median(df$MonthlyIncome), JobSatisfaction = median(df$JobSatisfaction)), type = "response")
    plot_ly(x = ~effect, y = ~pred, type = 'scatter', mode = 'lines', name = 'Attrition Probability', 
            marker = list(color = '#1f77b4'), line = list(color = '#1f77b4')) %>%
      layout(title = 'Predicted Probability of Attrition by Age',
             xaxis = list(title = 'Age'),
             yaxis = list(title = 'Probability of Attrition'))
  })
}

# Helper function to determine age range for filtering
getAgeRange <- function(ageGroup) {
  if (ageGroup == "Under 30") {
    return(18:29)
  } else if (ageGroup == "30-50") {
    return(30:50)
  } else if (ageGroup == "Over 50") {
    return(51:65)
  } else {
    return(NULL)  # Safety return
  }
}

shinyApp(ui = ui, server = server)

