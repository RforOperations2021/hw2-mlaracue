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
library("broom")

# for plotting
library("ggplot2")
library("plotly")
library("scales")

options(warn=-1)

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

ethnicities <- deaths$Ethnicity %>% unique() %>% sort()

# --- split the components expected by the app for readability

header <- dashboardHeader(
    title = "NYC Leading Causes of Death",
    titleWidth = 350
)

sidebar <- dashboardSidebar(
    
    # --- dashboard global filter; has an effect on all modules/pages
    selectInput(
        inputId = "cause",
        label = "Select a leading cause:",
        choices = c("All", leading_causes$LeadingCause),
        selected = 'All',
        multiple = FALSE, 
        width = '400px'
    ),
    
    hr(),
    
    # --- the app has three main pages: EDA, hypothesis testing and forecasting
    # the forecasting page is only available when input$cause == "All"
    # so the sidebarMenu is created as a reactive object
    
    sidebarMenuOutput(
        outputId = "mymenu"
    )
)

# --- the three components/items of the module are also separated for readability
EDA <- tabItem(
    tabName = "EDA",
    
    h2("Exploratory Data Analysis"),
    
    # -- first row
    fluidRow(
        column(
            width = 3,
            
            # pie-chart with leading causes' relative frequencies
            box(
                width = 12, 
                status = "primary", 
                title = "Leading Cause Pie-Chart",
                plotlyOutput(outputId = "pie", height = "100%")
            )
        ),
        
        tags$style(".small-box {height: 80px}"),
        
        # Descriptive information for total number of deaths
        column(
            width = 9,
            fluidRow(
                # static boxes
                valueBox(
                    subtitle = "Total No. Deaths",
                    value = tags$p(comma(sum(deaths$Deaths), accuracy = 1), 
                                   style = "font-size: 80%"),
                    icon = tags$i(
                        class = "fas fa-calculator", 
                        style = "font-size: 40px; color: white; position:relative; right:25px;"
                    ),
                    color = "purple"
                ),
                
                valueBox(
                    subtitle = "Average No. Deaths",
                    value = tags$p(comma(mean(deaths$Deaths), accuracy = 0.1), 
                                   style = "font-size: 80%;"),
                    icon = tags$i(
                        class = "fas fa-balance-scale", 
                        style = "font-size: 40px; color: white; position:relative; right:25px;"
                    ),
                    color = "purple"
                ),
                
                valueBox(
                    subtitle = "Average Rate of Death",
                    value = tags$p(comma(mean(deaths$DeathRate, na.rm = TRUE), accuracy = 0.01), 
                                   style = "font-size: 80%;"),
                    icon = tags$i(
                        class = "fas fa-percentage", 
                        style = "font-size: 40px; color: white; position:relative; right:25px;"
                    ),
                    color = "purple"
                )
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
    
    # -- second row
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

hypothesis_testing <- tabItem(
    tabName = "testing",
    
    h2("Hypothesis Testing"),
    
    fluidRow(
        column(
            width = 4,
            
            box(
                width = 12,
                
                h4("Demographics"),
                
                br(),
                
                radioButtons(
                    inputId = "demographics2", 
                    label = "Select one:", 
                    choices = c("Sex", "Ethnicity"), 
                    selected = "Sex", 
                    inline = TRUE
                ),
                
                # -- groups 1 and 2 are conditional on input 'demographics2'
                uiOutput(outputId = "group1"),
                uiOutput(outputId = "group2"),
                
                hr(),
                
                h4("Hypothesis Parameters"),
                
                br(),
                
                numericInput(
                    inputId = "alpha", 
                    label = "Significance level:", 
                    value = .05,
                    min = 0,
                    max = 0.1, 
                    step = .01
                ),
                
                selectInput(
                    inputId = "test_type", 
                    label = "Type of test:", 
                    choices = c("two.sided", "less", "greater")
                ),
                
                actionButton(
                    inputId = "go",
                    label = "Analyze"
                )
            )
        ),
        
        column(
            width = 8,

            fluidRow(
                # -- auxiliary tables: test results
                column(
                    width = 4, 
                    DTOutput(outputId = "test_results"),
                    br(),
                    
                    h4("Decision:"),
                    textOutput("decision")
                ),
                
                column(
                    width = 8, 
                    
                    # -- main plot (i.e., density distributions)
                    box(
                        width = 12,
                        title = "Distribution Differences", 
                        status = "primary",
                        
                        plotlyOutput(outputId = "densities")
                    ),
                    
                    # -- auxiliary tables: summary statistics
                    DTOutput(outputId = "statistics")
                )
            )
        )
    )
)

forecasting <- tabItem(
    tabName = "forecasting",
    
    h2("Forecasting"),
    
    fluidRow(
        column(
            width = 3,
            
            box(
                width = 12,
                
                h4("Population 1:"),
                
                hr(),
                
                radioButtons(
                    inputId = "sex", 
                    label = "Which sex?", 
                    choices = c("Female", "Male"), 
                    selected = "Female", 
                    inline = TRUE
                ),
                
                selectInput(
                    inputId = "ethnicity", 
                    label = "Which ethnicity?", 
                    choices = ethnicities
                ),
                
                br(),
                
                h4("Population 2:"),
                
                hr(),
                
                radioButtons(
                    inputId = "sex2", 
                    label = "Which sex?", 
                    choices = c("Female", "Male"), 
                    selected = "Female", 
                    inline = TRUE
                ),
                
                selectInput(
                    inputId = "ethnicity2", 
                    label = "Which ethnicity?", 
                    choices = ethnicities
                ),
            )
        )
    )
)

body <- dashboardBody(
    shinyDashboardThemes(
        theme = "purple_gradient"
    ),
    
    tabItems(EDA, hypothesis_testing, forecasting)
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session){
    
    # -- sidebarMenu ---
    # when input$cause is different than "All", the forecasting tab is hidden
    observeEvent(input$cause, {
        
        if(input$cause == "All"){
            
            output$mymenu <- renderMenu(
                
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
            
        } else {
            
            output$mymenu <- renderMenu(
                
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
                    )
                )
            )
        }
    })
    
    # --- EDA module ---
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
                height = 350,
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
            height = 470
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
    
    # --- Hypothesis Testing module ---
    
    # -- dynamic choices for group 1 and 2
    dem_choices <- reactive({
        
        req(input$demographics2)
        deaths_filtered() %>% 
            count(!!sym(input$demographics2)) %>% 
            filter(n >= 10) %>% 
            pull(!!sym(input$demographics2)) %>% 
            sort()
    })
    
    output$group1 <- renderUI(
        selectInput(
            inputId = "group1",
            label = "Select comparison group 1:",
            choices = dem_choices()
        )
    )
    
    # -- an user cannot select the same group for both (group 1 and group 2)
    # So I take out the group selected in input$group1
    output$group2 <- renderUI({
        req(input$group1)
        
        selectInput(
            inputId = "group2",
            label = "Select comparison group 2:",
            choices = dem_choices()[dem_choices() != input$group1]
        )
    })
    
    # -- here I update the maximum value permitted for significance level
    observe({
        req(input$alpha)
        
        if(input$alpha > 0.1 | input$alpha < 0){
            updateNumericInput(
                session = session,
                inputId = "alpha",
                value = 0.1
            )
        }
    })
    
    test_df <- eventReactive(input$go, {
        
        req(input$demographics2)
        
        deaths_filtered() %>%
            rename(groups = !!sym(input$demographics2)) %>%
            filter(groups %in% c(input$group1, input$group2)) %>%
            select(groups, Deaths)
    })
    
    dens1 <- eventReactive(input$go, {
        density(x = test_df() %>% filter(groups == input$group1) %>% pull(Deaths))
    })
    
    dens2 <- eventReactive(input$go, {
        density(x = test_df() %>% filter(groups == input$group2) %>% pull(Deaths))
    })
    
    # # -- density plot for groups 1 and 2
    output$densities <- renderPlotly({
        
        plot_ly(
            x = ~dens1()$x,
            y = ~dens1()$y,
            type = 'scatter',
            mode = 'none',
            name = isolate(input$group1),
            fill = 'tozeroy',
            fillcolor = "rgb(116, 0, 184, 0.1)",
        ) %>%
            add_trace(
                x = ~dens2()$x,
                y = ~dens2()$y,
                name = isolate(input$group2),
                fill = 'tozeroy',
                fillcolor =  my_pal[10],
                opacity = .5
            ) %>% layout(
                xaxis = list(title = ''),
                yaxis = list(title = 'Density'),
                margin = list(l = 10, r = 10, t = 10, b = 10),
                plot_bgcolor  = "rgba(0, 0, 0, 0)",
                paper_bgcolor = "rgba(0, 0, 0, 0)",
                font = list(color = '#FFFFFF', size = 10),
                legend = list(x = 0.65, y = 0.99)
            )
    })
    
    summary_table <- reactive({
        
        test_df() %>% 
            group_by(groups) %>% 
            summarise_if(.predicate = is.numeric, 
                         .funs = list(
                             N = length, 
                             mean = mean, 
                             var = var, 
                             median = median
                         )
            ) %>% 
            mutate(groups = isolate(c(input$group1, input$group2)),
                   StDev = sqrt(var)) %>% 
            select(groups, N, mean, median, var, StDev)
    })
    
    output$statistics <- renderDT(
        datatable(
            data = summary_table(),
            options = list(dom = 't'),
            caption = "Summary statistics"
        ) %>% formatRound(columns = c(3:6), digits = 0)
    )
    
    results <- eventReactive(input$go, {
        
        test_df() %>% 
            group_by(groups) %>%
            do(tidy(
                t.test(x = test_df()$Deaths,
                       alternative = input$test_type, 
                       conf.level = (1 - input$alpha))
            )) %>% 
            ungroup() %>% 
            select(estimate:conf.high) %>% 
            distinct() %>% 
            gather(key = "var", value = "estimates")
        
    })
    
    output$test_results <- renderDT(
        datatable(
            data = results(),
            colnames = c("", "results"),
            options = list(dom = 't'),
            caption = "Test results", 
            rownames = FALSE,
        ) %>% formatRound(columns = "estimates", digits = 2)
    )
    
    output$decision <- renderText({
        
        if(results() %>% filter(var == "p.value") %>% pull(estimates) <= input$alpha) {
            
            paste("Reject the null at alpha =", input$alpha) 
            
        } else {
            paste("Fail to reject the null at alpha =", input$alpha) 
        }
        
    })
}

shinyApp(ui = ui, server = server)