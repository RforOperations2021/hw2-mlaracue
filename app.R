# for the app itself
library("shiny")
library("shinydashboard")
library("dashboardthemes")
library("DT")
library("rhandsontable")

# for data loading and transformation
library("readr")
library("tidyr")
library("dplyr")
library("stringr")
library("comprehenr")

# for plotting
library("ggplot2")
library("plotly")
library("scales")

my_pal <- c("#7400b8", "#6930c3", "#5e60ce", "#5390d9", "#4ea8de",
            "#48bfe3", "#56cfe1", "#64dfdf", "#72efdd", "#80ffdb")

# --- data loading
deaths <- read_csv(
    file = "./data/NYC-death-causes.csv",
    col_types = cols(
        Year = col_double(),
        .default = col_character()
    )
)

names(deaths) <- str_replace_all(names(deaths), pattern = " ", "")

fix_numbers <- function(x) {
    if (x == ".") {
        return(NA_real_)
    } else {
        return(as.numeric(x))
    }
}

deaths <- deaths %>% 
    rename(Ethnicity = RaceEthnicity) %>% 
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
        .funs = fix_numbers
    ) %>% 
    filter(!is.na(Deaths), Deaths > 5)

leading_causes <- deaths %>% 
    group_by(LeadingCause) %>% 
    summarise(n = sum(Deaths)) %>% 
    arrange(-n) %>%
    ungroup() %>% 
    mutate(wt = n / sum(n)) %>% 
    filter(n > quantile(n, probs = .5))

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
        # this column includes the global filter for the page (leading case) and a pie-chart
        column(
            width = 3,
            box(
                width = 12, 
                status = "primary", 
                title = "Global Filter",
                selectInput(
                    inputId = "cause", 
                    label = "Select a leading cause (optional)", 
                    choices = c("All", leading_causes$LeadingCause),
                    selected = 'All', 
                    multiple = FALSE
                )
            ),
            
            # pie-chart with leading causes' relative frequencies
            box(
                width = 12, 
                status = "primary", 
                title = "Leading Cause Pie-Chart",
                plotlyOutput(outputId = "pie", height = "80%")
            )
        ),
        
        # Descriptive information for total number of deaths
        column(
            width = 9,
            fluidRow(
                # for dynamic info boxes:infoBoxOutput
                infoBox("total_deaths", color = "purple"),
                infoBox("average_deaths", color = "purple"),
                infoBox("average_rate", color = "purple")
            ),
            
            tabBox(
                width = 12,
                side = "left", height = "250px",
                selected = "Total Deaths",
                tabPanel(
                    title = "Total Deaths", 
                    icon = icon("chart-bar"),
                    radioButtons(
                        inputId = "demographics", 
                        label = "Select one:", 
                        choices = c("All", "Sex", "Ethnicity"), 
                        selected = "All", 
                        inline = TRUE
                    ),
                    plotlyOutput(outputId = "total_deaths", height = "80%")
                ),
                
                tabPanel("Data", "Tab content 2", icon = icon("table"))
            ),
        )
    ),
    
    # second row
    fluidRow(
        column(
            width = 6,
            
            box(
                width = 12,
                plotlyOutput(outputId = "women_men_ratio", height = "80%")
            ),
            
            tabBox(
                width = 12,
                side = "left", height = "250px",
                selected = "Sex",
                tabPanel(
                    title = "Sex", 
                    icon = icon("venus-mars"),
                    plotlyOutput(outputId = "boxplots1", height = "80%")
                ),
                tabPanel(
                    title = "Ethnicity", 
                    icon = icon("globe-americas"),
                    plotlyOutput(outputId = "boxplots2", height = "80%")
                )
            ),
        ),
        
        column(
            width = 6,
            box(
                width = 12,
                plotlyOutput(outputId = "lollipops", height = "80%")
            )
        )
    )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session){
    
    axis_template <- list(
        showgrid = FALSE, 
        zeroline = FALSE, 
        showticklabels = FALSE
    )
    
    # pie chart with death leading causes' relative frequencies
    output$pie <- renderPlotly({
        
        colors <- to_vec(
            for(i in leading_causes$LeadingCause) {
                if(input$cause == "All"){
                    my_pal
                } else if(i == input$cause) {
                    my_pal[1]
                } else {
                    my_pal[10]
                }
            }
        )
        
        leading_causes %>% 
            plot_ly(
                labels = ~LeadingCause, 
                values = ~wt,
                height = 200,
                textposition = 'inside',
                textinfo = 'percent',
                hoverinfo = 'label',
                marker = list(
                    colors = colors, 
                    line = list(color = '#FFFFFF', width = 1)
                )
            ) %>% 
            add_pie(hole = 0.4) %>% 
            layout(
                xaxis = axis_template,
                yaxis = axis_template,
                margin = list(l = 10, r = 10, t = 10, b = 10),
                showlegend = FALSE,
                plot_bgcolor  = "rgba(0, 0, 0, 0)",
                paper_bgcolor = "rgba(0, 0, 0, 0)"
            )
    })
    
    total_deaths_df <- reactive({
        req(input$cause)
        req(input$demographics)
        
        # when there is no filter (i.e., choice "All"), I choose all the leading causes
        # so basically, no filter is done
        
        if(input$cause == "All"){
            cause <- deaths$LeadingCause
        } else {
            cause <- input$cause
        }
        
        df <- deaths %>% 
            filter(LeadingCause %in% cause)
        
        # when there is no demographic variable selected, I group only by year
        if(input$demographics == "All"){
            
            df <- df %>% 
                group_by(Year)
                
        } else {
            # otherwise, I use the corresponding column
            df <- df %>% 
                rename(var = !!sym(input$demographics)) %>% 
                group_by(Year, var)
        }
        
        df %>% 
            summarise(Deaths = sum(Deaths), 
                      .groups = "drop")
        
    })
    
    output$total_deaths <- renderPlotly({
        
        # when the user selects "All" (default), I need to change the color argument
        if(input$demographics == "All"){
            fig <- total_deaths_df() %>% 
                plot_ly(
                    x = ~Year, 
                    y = ~Deaths, 
                    height = 200,
                    type = "bar",
                    marker = list(color = my_pal[10])
                )
        } else {
            fig <- total_deaths_df() %>% 
                plot_ly(
                    x = ~Year, 
                    y = ~Deaths, 
                    color = ~var, # no color argument for "All"
                    height = 200,
                    type = "bar",
                    colors = my_pal
                )
        }
        
         fig %>% 
            layout(
                margin = list(l = 10, r = 10, t = 10, b = 10),
                plot_bgcolor  = "rgba(0, 0, 0, 0)",
                paper_bgcolor = "rgba(0, 0, 0, 0)",
                font = list(color = '#FFFFFF', size = 10)
            )
        
    })
}


shinyApp(ui = ui, server = server)