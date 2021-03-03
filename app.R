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
    na = ".",
    file = "./data/NYC-death-causes.csv",
    col_types = cols(
        Year = col_double(),
        .default = col_character()
    )
)

names(deaths) <- str_replace_all(names(deaths), pattern = " ", "")

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
        .funs = as.numeric
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
                # static boxes
                infoBox(
                    title = "Total No. Deaths",
                    value = comma(sum(deaths$Deaths), digits = 0),
                    icon = icon("calculator"),
                    color = "purple"
                ),
                
                infoBox(
                    title = "Average No. Deaths",
                    value = comma(mean(deaths$Deaths), digits = 2),
                    icon = icon("balance-scale"),
                    color = "purple"
                ),
                
                infoBox(
                    title = "Average Rate of Death",
                    value = comma(mean(deaths$DeathRate), digits = 2),
                    icon = icon("percentage"),
                    color = "purple"
                ),
                # for dynamic info boxes (not working!)
                # infoBoxOutput(outputId = "total_deaths")
                # infoBoxOutput(outputId = "average_deaths"),
                # infoBoxOutput(outputId = "average_rate")
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
                
                tabPanel("Data", DTOutput("table1"), icon = icon("table"))
            ),
        )
    ),
    
    # second row
    fluidRow(
        column(
            width = 8,
            
            tabBox(
                width = 12,
                side = "left", height = "250px",
                selected = "Female / Male Deaths Ratio",
                tabPanel(
                    title = "Female / Male Deaths Ratio", 
                    icon = icon("venus-mars"),
                    plotlyOutput(outputId = "women_men_ratio", height = "80%")
                ),
                
                tabPanel(title = "Data", DTOutput("table2"), icon = icon("table")
                )
            ),
            
            tabBox(
                width = 12,
                side = "left", height = "250px",
                selected = "Sex",
                tabPanel(
                    title = "Sex", 
                    icon = icon("venus-mars"),
                    plotlyOutput(outputId = "boxplot1", height = "80%")
                ),
                tabPanel(
                    title = "Ethnicity", 
                    icon = icon("globe-americas"),
                    plotlyOutput(outputId = "boxplot2", height = "80%")
                )
            ),
        ),
        
        column(
            width = 4,
            box(
                title = "Average Deaths Rates by Year and Sex",
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
    
    # -- pie that shows the relative frequency of each leading cause, dynamic colors.
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
    
    # -- reactive object to filter the leading cause (global filter for the page)
    deaths_filtered <- reactive({
        req(input$cause)
        
        # when there is no filter (i.e., choice "All"), I choose all the leading causes
        # so basically, no filter is done
        
        if(input$cause == "All"){
            cause <- deaths$LeadingCause
        } else {
            cause <- input$cause
        }
        
        deaths %>% 
            filter(LeadingCause %in% cause)
    })
    
    # infobox 1: sum of Deaths columns from filtered df (global)
    output$total_deaths <- renderInfoBox({
        infoBox(
            title = "Total No. Deaths",
            value = comma(sum(deaths_filtered()$Deaths), digits = 0),
            icon = icon("calculator"),
            color = "purple"
        )
    })
    
    total_deaths_df <- reactive({
        
        req(input$demographics)
        
        # when there is no demographic variable selected, I group only by year
        if(input$demographics == "All"){
            
            df <- deaths_filtered() %>% 
                group_by(Year)
            
        } else {
            # otherwise, I use the corresponding column
            df <- deaths_filtered() %>% 
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
        
        # layout enhancement
        fig %>% 
            layout(
                margin = list(l = 10, r = 10, t = 10, b = 10),
                plot_bgcolor  = "rgba(0, 0, 0, 0)",
                paper_bgcolor = "rgba(0, 0, 0, 0)",
                font = list(color = '#FFFFFF', size = 10)
            )
        
    })
    
    output$table1 <- renderDT(
        total_deaths_df(),
        options = list(pageLength = 4, dom = 'ftip')
    )
    
    women_men_ratio_df <- reactive({
        
        # fir this df, I have to transform the data from wide to long
        # and then get both the ratio and the sum of total deaths
        # the rows where the # of deaths is zero are eliminated
        
        deaths_filtered() %>% 
            select(-DeathRate, -AgeAdjustedDeathRate) %>% 
            spread(key = Sex, value = Deaths, fill = 0) %>% 
            filter(Male != 0, Female != 0) %>%
            rowwise() %>% 
            mutate(ratio = Female / Male,
                   Deaths = Female + Male) %>% 
            group_by(Year, Ethnicity) %>% 
            summarise(
                ratio = mean(ratio), 
                Deaths = sum(Deaths),
                .groups = "drop"
            )
    })
    
    output$women_men_ratio <- renderPlotly(
        
        women_men_ratio_df() %>% 
            plot_ly(
                x = ~Year, 
                y = ~ratio, 
                color = ~Ethnicity, 
                size = ~Deaths, 
                colors = my_pal,
                type = 'scatter', 
                mode = 'markers', 
                height = 200,
                sizes = c(10, 60),
                marker = list(
                    symbol = 'circle', 
                    sizemode = 'diameter',
                    line = list(width = 2, color = '#FFFFFF')
                ),
                text = ~paste(
                    'Year:', Year,
                    '<br>Ratio:', round(ratio, 2),
                    '<br>Total Deaths:', comma(Deaths)
                )
            ) %>% 
            layout(
                margin = list(l = 10, r = 10, t = 10, b = 10),
                plot_bgcolor  = "rgba(0, 0, 0, 0)",
                paper_bgcolor = "rgba(0, 0, 0, 0)",
                font = list(color = '#FFFFFF', size = 10),
                legend = list(x = 0.8, y = 0.05),
                xaxis = list(showgrid = F)
            )
    )
    
    output$table2 <- renderDT(
        women_men_ratio_df() %>% mutate(ratio = round(ratio, 2)),
        options = list(pageLength = 4, dom = 'ftip')
    )
    
    output$boxplot1 <- renderPlotly({
        
        deaths_filtered() %>% 
            plot_ly(
                x = ~Deaths, 
                type = "box",
                color = ~Sex,
                colors = my_pal[c(1, 10)],
                height = 200
            ) %>% 
            layout(
                margin = list(l = 10, r = 10, t = 10, b = 10),
                plot_bgcolor  = "rgba(0, 0, 0, 0)",
                paper_bgcolor = "rgba(0, 0, 0, 0)",
                font = list(color = '#FFFFFF', size = 10),
                legend = list(x = 0.8, y = 0.05),
                xaxis = axis_template
            )
        
    })
    
    output$boxplot2 <- renderPlotly({
        
        deaths_filtered() %>% 
            plot_ly(
                x = ~Deaths, 
                type = "box",
                color = ~Ethnicity,
                colors = my_pal,
                height = 200
            ) %>% 
            layout(
                margin = list(l = 10, r = 10, t = 10, b = 10),
                plot_bgcolor  = "rgba(0, 0, 0, 0)",
                paper_bgcolor = "rgba(0, 0, 0, 0)",
                font = list(color = '#FFFFFF', size = 10),
                legend = list(x = 0.8, y = 0.05),
                xaxis = axis_template
            )
        
    })
    
    
    output$lollipops <- renderPlotly({
        
        # I get an average of death rates by year and sex (accounting for ethnicities)
        lollipop_df <- deaths_filtered() %>%
            group_by(Year, Sex) %>% 
            summarise(DeathRate = mean(DeathRate, na.rm = TRUE),
                      .groups = "drop") %>% 
            ungroup()
        
        # first trace
        fig <- plot_ly(
            data = lollipop_df %>% filter(Sex == "Female"),
            x = ~DeathRate, 
            y = ~Year, 
            name = "Women", 
            type = 'scatter',
            mode = "lines+markers", 
            marker = list(color = my_pal[1], size = 10),
            line = list(color = my_pal[1]),
            height = 500
        )
        
        # second trace
        fig <- fig %>% 
            add_trace(
                data = lollipop_df %>% filter(Sex == "Male"),
                x = ~DeathRate, 
                y = ~Year, 
                name = "Men", 
                type = 'scatter',
                mode = "lines+markers", 
                marker = list(color = my_pal[10], size = 10), 
                line = list(color = my_pal[10])
            )
        
        # layout enhancement
        fig <- fig %>% 
            layout(
                margin = list(l = 10, r = 10, t = 10, b = 10),
                plot_bgcolor  = "rgba(0, 0, 0, 0)",
                paper_bgcolor = "rgba(0, 0, 0, 0)",
                font = list(color = '#FFFFFF', size = 10),
                legend = list(x = 0.8, y = 0.05),
                yaxis = list(showgrid = F),
                xaxis = list(
                    dtick = 5, 
                    tick0 = 0, 
                    tickmode = "linear"
                )
            )
    })
}

shinyApp(ui = ui, server = server)