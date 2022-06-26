# BUSINESS SCIENCE ----
# DS4B 202-R ----
# STOCK ANALYZER APP - DYNAMIC PLOT TABS -----
# Version 1

# APPLICATION DESCRIPTION ----
# - Add functionality that users can control plots for stock favorites

# Libraries ----
library(readxl)
library(httr)
library(lubridate)
library(dplyr)
library(tidyverse)
library(timetk)
library(tidyquant)
#install.packages("RMySQL")
library(RMySQL)
packageVersion("readxl")
# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)

library(plotly)
library(tidyquant)
library(tidyverse)

library(parsnip)
library(xgboost)
# source(file = "00_scripts/stock_analysis_functions.R")
# source(file = "00_scripts/info_card.R")
# source(file = "00_scripts/generate_favorite_cards.R")
source(file = "psx_stock_scripts.R")
stock_list_tbl <- get_stock_list("PSX")

current_user_favorites <- c("COLG", "FATIMA", "GLAXO")

# UI ----
ui <- navbarPage(
    title = "Stock Analyzer",
    inverse = FALSE,
    collapsible = TRUE,
    
    theme = shinytheme("cyborg"),
    
    tabPanel(
        title = "Analysis",
        
        # CSS ----
        shinythemes::themeSelector(),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
        ),
        
        # JS ----
        shinyjs::useShinyjs(),
        
        # 1.0 HEADER ----
        div(
            class = "container",
            id = "header",
            h1(class = "page-header", "PSX Stock Analyzer", tags$small("by Noman")),
            p(class = "lead", "App in Shiny for PSX Stock", 
              a(href = "https://www.psx.com.pk/", target = "_blank", "PSX Expert App"))
        ),
        
      
        
        # 2.0 FAVORITES ----
        div(
            class = "container hidden-sm hidden-xs",
            id = "favorite_container",
            # 2.1 USER INPUTS ----
            div(
                class = "",
                column(
                    width = 12,
                    h5(class = "pull-left", "Favorites"),
                    actionButton(inputId = "favorites_clear", "Clear Favorites", class = "pull-right"),
                    actionButton(inputId = "favorites_toggle", "Show/Hide", class = "pull-right")
                )
            ),
            # 2.2 FAVORITE CARDS ----
            div(
                class = "row",
                id = "favorite_card_section",
                uiOutput(outputId = "favorite_cards", class = "container")
            )
        ),
        
        # 3.0 APPLICATION UI -----
        div(
            class = "container",
            id = "application_ui",
            
            # 3.1 USER INPUTS ----
            column(
                width = 4, 
                wellPanel(
                    div(
                        id = "input_main",
                        pickerInput(
                            inputId = "stock_selection", 
                            label   = "Stock List (Pick One to Analyze)",
                            choices = stock_list_tbl$label,
                            multiple = FALSE, 
                            selected = stock_list_tbl %>% filter(label %>% str_detect("COLG")) %>% pull(label),
                            options = pickerOptions(
                                actionsBox = FALSE,
                                liveSearch = TRUE,
                                size = 10
                            )
                        )
                    ),
                    div(
                        id = "input_buttons",
                        actionButton(inputId = "analyze", label = "Analyze", icon = icon("download")),
                        actionButton(inputId = "loadrecent", label = "Reload", icon = icon("download")),
                        div(
                            class = "pull-right",
                            actionButton(inputId = "favorites_add", label = NULL, icon = icon("heart")),
                            actionButton(inputId = "settings_toggle", label = NULL, icon = icon("cog"))
                        )
                    ),
                  
                    div(
                        id = "input_settings",
                        hr(),
                        sliderInput(inputId = "mavg_short", label = "Short Moving Average", value = 20, min = 5, max = 40),
                        sliderInput(inputId = "mavg_long", label = "Long Moving Average", value = 50, min = 50, max = 120),
                        actionButton(inputId = "apply_and_save", label = "Apply & Save", icon = icon("save"))
                    ) %>% hidden()
                )
            ),
            
            # 3.2 PLOT PANEL ----
            column(
                width = 8, 
                uiOutput(outputId = "stock_charts")
            ),
            column(
                width = 8, 
                uiOutput(outputId = "forecast_charts")
            )
        ),
        
     
        # 4.0 ANALYST COMMENTARY ----
        div(
            class = "container",
            id = "commentary",
            column(
                width = 12,
                div(
                    class = "panel",
                    div(class = "panel-header", h4("Analyst Commentary")),
                    div(
                        class = "panel-body",
                        textOutput(outputId = "analyst_commentary")
                    )
                )
            )
        )
    )
    
)

# SERVER ----
server <- function(input, output, session) {
    # 
    # 1.0 SETTINGS ----

    # 1.1 Toggle Input Settings ----
    observeEvent(input$settings_toggle, {
        toggle(id = "input_settings", anim = TRUE)
    })

    # 1.2 Stock Symbol ----
    stock_symbol <- eventReactive(input$analyze, {
        get_symbol_from_user_input(input$stock_selection)
    }, ignoreNULL = FALSE)

    # 1.3 User Input ----
    stock_selection_triggered <- eventReactive(input$analyze, {
        input$stock_selection
    }, ignoreNULL = FALSE)
    
    stock_selection_triggered1 <- eventReactive(input$loadrecent, {
        populate_new_data()
    }, ignoreNULL = FALSE)
    
    

    # 1.4 Apply & Save Settings ----
    mavg_short <- eventReactive(input$apply_and_save, {
        input$mavg_short
    }, ignoreNULL = FALSE)

    mavg_long <- eventReactive(input$apply_and_save, {
        input$mavg_long
    }, ignoreNULL = FALSE)

    selected_tab <- eventReactive(input$apply_and_save, {
        if (is.character(input$tab_panel_stock_chart)) {
            # Tab already selected
            selected_tab <- input$tab_panel_stock_chart
        } else {
            # Tab panel not built yet
            selected_tab <- NULL
        }

        selected_tab

    }, ignoreNULL = FALSE)
    # 
    # 1.5 Get Stock Data ----
    stock_data_tbl <- reactive({
        stock_symbol() %>%
            get_stock_data(
                from = today()-days(180),
                to   = today(),
                mavg_short = mavg_short(),
                mavg_long  = mavg_long())
    })

    stock_data_tbl1 <- reactive({
        stock_data_tbl()%>%
            aggregate_time_series(time_unit = "day")%>%
            generate_forecast()
    })
    

    # 2.0 FAVORITE CARDS ----

    # # 2.1 Reactive Values - User Favorites ----
    reactive_values <- reactiveValues()
    reactive_values$favorites_list <- current_user_favorites

    # 2.2 Add Favorites ----
    observeEvent(input$favorites_add, {

        new_symbol <- get_symbol_from_user_input(input$stock_selection)
        new_symbol_already_in_favorites <- new_symbol %in% reactive_values$favorites_list

        if (!new_symbol_already_in_favorites) {
            reactive_values$favorites_list <- c(reactive_values$favorites_list, new_symbol) %>% unique()

            updateTabsetPanel(session = session, inputId = "tab_panel_stock_chart", selected = new_symbol)
        }

    })

    # 2.3 Render Favorite Cards ----
    output$favorite_cards <- renderUI({

        if (length(reactive_values$favorites_list) > 0) {
            generate_favorite_cards(
                favorites  = reactive_values$favorites_list,
                from       = today() - days(180),
                to         = today(),
                mavg_short = mavg_short(),
                mavg_long  = mavg_long()
            )
        }

    })

    # 2.4 Delete Favorites ----
    observeEvent(input$favorites_clear, {
        modalDialog(
            title = "Clear Favorites",
            size = "m",
            easyClose = TRUE,

            p("Are you sure you want to remove favorites?"),
            br(),
            div(
                selectInput(inputId = "drop_list",
                            label   = "Remove Single Favorite",
                            choices = reactive_values$favorites_list %>% sort()),
                actionButton(inputId = "remove_single_favorite",
                             label   = "Clear Single",
                             class   = "btn-warning"),
                actionButton(inputId = "remove_all_favorites",
                             label   = "Clear ALL Favorites",
                             class   = "btn-danger")
            ),

            footer = modalButton("Exit")
        ) %>% showModal()
    })

    # 2.4.1 Clear Single ----
    observeEvent(input$remove_single_favorite, {

        reactive_values$favorites_list <- reactive_values$favorites_list %>%
            .[reactive_values$favorites_list != input$drop_list]

        updateSelectInput(session = session,
                          inputId = "drop_list",
                          choices = reactive_values$favorites_list %>% sort())
    })

    # 2.4.2 Clear All ----
    observeEvent(input$remove_all_favorites, {

        reactive_values$favorites_list <- NULL

        updateSelectInput(session = session,
                          inputId = "drop_list",
                          choices = reactive_values$favorites_list %>% sort())
    })

    # 2.5 Show/Hide Favorites ----
    observeEvent(input$favorites_toggle, {
        shinyjs::toggle(id = "favorite_card_section", anim = TRUE, animType = "slide")
    })

    # 3.0 FAVORITE PLOT ----

    # 3.1 Plot Header ----
    output$plot_header <- renderText({
        stock_selection_triggered()
    })

    # 3.2 Plotly Plot ----
    output$plotly_plot <- renderPlotly({
        stock_data_tbl()%>%
            # mutate(Prediction=NA)%>%
            # add_row(as.data.frame(
            #     stock_data_tbl1()%>%
            #         pivot_wider(id_cols =  date, names_from = key,values_from = total_adjusted)%>%
            #         mutate(symbol=stock_symbol(),adjusted = NA, mavg_short=NA,mavg_long=NA, Prediction, date = as.character(date))%>%
            #         select(symbol,date,adjusted,mavg_short,mavg_long,Prediction)%>%
            #         filter(is.na(Prediction==FALSE))))%>%
            plot_stock_data()
    })
    
    output$plotly_plot1 <- renderPlotly({
        stock_data_tbl1() %>% plot_forecast()
    })

    # 3.3 Favorite Plots ----

    output$stock_charts <- renderUI({

        # First Tab Panel
        tab_panel_1 <- tabPanel(
            title = "Last Analysis",
            div(
                class = "panel",
                div(
                    class = "panel-header",
                    h4(stock_symbol())
                ),
                div(
                    class = "panel-body",
                    plotlyOutput(outputId = "plotly_plot")
                )
            )
        )

        # Favorite Panels
        favorite_tab_panels <- NULL
        if (length(reactive_values$favorites_list) > 0) {

            favorite_tab_panels <- reactive_values$favorites_list %>%
                map(.f = function(x) {
                    tabPanel(
                        title = x,
                        div(
                            class = "panel",
                            div(
                                class = "panel-header",
                                h4(x)
                            ),
                            div(
                                class = "panel-body",

                                x %>%
                                    get_stock_data(
                                        from = today() - days(180),
                                        to   = today(),
                                        mavg_short = mavg_short(),
                                        mavg_long  = mavg_long()
                                    ) %>%
                                    plot_stock_data()
                            )
                        )
                    )
                })

        }



    # Building the Tabset Panel
    do.call(
        what = tabsetPanel,
        args = list(tab_panel_1) %>%
            append(favorite_tab_panels) %>%
            append(list(id = "tab_panel_stock_chart", type = "pills", selected = selected_tab() ))
    )

})


    
    output$forecast_charts <- renderUI({
        
        # First Tab Panel
        tab_panel_2 <- tabPanel(
            title = "Last Analysis",
            div(
                class = "panel",
                div(
                    class = "panel-header",
                    h4(stock_symbol())
                ),
                div(
                    class = "panel-body",
                    plotlyOutput(outputId = "plotly_plot1")
                )
            )
        )

        # Favorite Panels
        favorite_tab_panels1 <- NULL
        if (length(reactive_values$favorites_list) > 0) {
            
            favorite_tab_panels1 <- reactive_values$favorites_list %>%
                map(.f = function(x) {
                    tabPanel(
                        title = x,
                        div(
                            class = "panel",
                            div(
                                class = "panel-header",
                                h4(x)
                            ),
                            div(
                                class = "panel-body",
                                
                                x %>%
                                    get_stock_data(
                                        from = today() - days(180),
                                        to   = today(),
                                        mavg_short = mavg_short(),
                                        mavg_long  = mavg_long()
                                    ) %>%
                                    aggregate_time_series(time_unit = "day")%>%
                                    generate_forecast()%>%
                                    plot_forecast()
                            )
                        )
                    )
                })
            
        }
        
        
        # Building the Tabset Panel
        do.call(
            what = tabsetPanel,
            args = list(tab_panel_2) %>%
                append(favorite_tab_panels1) %>%
                append(list(id = "tab_panel_stock_chart", type = "pills", selected = selected_tab() ))
        )
        
    })
# 4.0 COMMENTARY ----

# 4.1 Generate Commentary ----
output$analyst_commentary <- renderText({
    generate_commentary(data = stock_data_tbl(), user_input = stock_selection_triggered())
})


}

# RUN APP ----
shinyApp(ui = ui, server = server)