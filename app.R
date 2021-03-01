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

# --- data loading
deaths <- read_csv(
    file = "./data/NYC-death-causes.csv",
    col_types = cols(
        Year = col_double(),
        .default = col_character()
    )
)

names(deaths) <- str_replace_all(names(deaths), pattern = " ", "")

deaths <- deaths %>% 
    filter(!is.na(Deaths)) %>% 
    mutate(
        LeadingCause = str_replace_all(
            LeadingCause, 
            pattern = "Posioning",
            replacement = "Poisoning"),
        LeadingCause = gsub("\\(..*", "", LeadingCause),
        LeadingCause = str_trim(LeadingCause),
        Sex = case_when(Sex == "F" ~ "Female",
                        Sex == "M" ~ "Male",
                        TRUE ~ Sex)
    ) %>% 
    mutate_at(
        .vars = c("Deaths", 'DeathRate', "AgeAdjustedDeathRate"),
        .funs = function(x) as.numeric(str_replace_all(x, ".", ""))
    )

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
    
    # first row
    fluidRow(
        # these are the main plots: trend comparison for different leading causes
        column(
            width = 3,
            box(status = "warning")
        ),
        
        # this column includes the global inputs for the page (leading case) and a pie-chart
        column(
            width = 3,
            box(width = 12, status = "primary", title = "Global Inputs"),
            box(width = 12, status = "primary", title = "Pie chart")
        ),
        
        # Descriptive information for total number of deaths
        column(
            width = 6,
            fluidRow(
                # for dynamic info boxes:infoBoxOutput
                infoBox("total_deaths", color = "yellow"),
                infoBox("average_deaths", color = "fuchsia"),
                infoBox("average_rate", color = "aqua")
            ),
            box(status = "primary", title = "Total Deaths")
        )
    ),
    
    # second row
    fluidRow(
        column(
            width = 6,
            box(width = 12),
            box(width = 12)
        ),
        
        column(
            width = 6,
            box()
        )
    )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session){
    
}

shinyApp(ui = ui, server = server)