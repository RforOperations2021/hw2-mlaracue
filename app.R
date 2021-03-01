# for the app itself
library("shiny")
library("shinydashboard")
library("dashboardthemes")
library("DT")

# for data loading and transformation
library("readr")
library("tidyr")
library("dplyr")
library("stringr")

# for plotting
library("ggplot2")
library("plotly")
library("scales")

# --- split the components expected by the app for readability

header <- dashboardHeader(
    title = "NYC Leading Causes of Death",
    titleWidth = 350
)

sidebar <- dashboardSidebar(
    # the app has three main pages: EDA, hypothesis testing and forecasting
    
    sidebarMenu(
        menuItem(
            "Exploratory Data Analysis", 
            tabName = "EDA", 
            icon = icon("chart-bar")
        ),
        menuItem(
            "Hypothesis testing", 
            tabName = "testing", 
            icon = icon("vials"), 
            badgeLabel = "new", 
            badgeColor = "green"
        ),
        
        menuItem(
            "Forecasting", 
            tabName = "forecasting", 
            icon = icon("diagnoses")
        )
    )
)

body <- dashboardBody(
    shinyDashboardThemes(
        theme = "purple_gradient"
    ),
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session){
    
}

shinyApp(ui = ui, server = server)