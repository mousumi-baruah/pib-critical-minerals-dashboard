# =========================
# Load libraries
# =========================
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(DT)

# =========================
# UI
# =========================
ui <- dashboardPage(
  
  dashboardHeader(title = "PIB Critical Minerals Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
      hr(),
      
      h4("Filters"),
      
      uiOutput("year_ui"),
      
      selectInput(
        "ministry",
        "Select Ministry",
        choices = NULL,
        multiple = TRUE
      ),
      
      textInput(
        "keyword",
        "Keyword search (Title)",
        placeholder = "e.g. lithium, mining, supply chain"
      ),
      
      radioButtons(
        "aggregation",
        "Time Aggregation",
        choices = c("Daily", "Monthly", "Yearly"),
        selected = "Monthly"
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
                hr(),
        fluidRow(
          column(
            width = 12,
            tags$small(
              HTML(
                paste0(
                  "Citation: Baruah, Mousumi (2025). ",
                  "<em>PIB Critical Minerals Dashboard</em>. ",
                  "<a href='https://mousumib.shinyapps.io/pib_shiny_app/' target='_blank'>",
                  "https://mousumib.shinyapps.io/pib_shiny_app/</a>"
                )
              )
            )
          )
        ),

        
        # ---- KPIs ----
        fluidRow(
          valueBoxOutput("kpi_total", width = 4),
          valueBoxOutput("kpi_years", width = 4),
          valueBoxOutput("kpi_ministries", width = 4)
        ),
        
        # ---- Plot ----
        fluidRow(
          box(
            width = 12,
            title = "Press Releases Over Time",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("time_plot", height = 300)
          )
        ),
        
        # ---- Table ----
        fluidRow(
          box(
            width = 12,
            title = "Filtered Press Releases",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("pib_table")
          )
        )
      )
    )
  )
)

# =========================
# Server
# =========================
server <- function(input, output, session) {
  
  # ---- Load data ----
  pib <- read.csv(
    "pib_critical_minerals_clean_v2.csv",
    stringsAsFactors = FALSE
  )
  
  pib$date <- as.Date(pib$date)
  pib$year <- year(pib$date)
  pib$month <- floor_date(pib$date, "month")
  
  # ---- Dynamic year filter ----
  output$year_ui <- renderUI({
    checkboxGroupInput(
      "year",
      "Select Year(s)",
      choices = sort(unique(pib$year)),
      selected = max(pib$year)
    )
  })
  
  # ---- Update ministry choices ----
  observe({
    updateSelectInput(
      session,
      "ministry",
      choices = sort(unique(pib$ministry)),
      selected = unique(pib$ministry)
    )
  })
  
  # ---- Reactive filtered data ----
  filtered_data <- reactive({
    req(input$year)
    
    data <- pib %>%
      filter(year %in% input$year)
    
    if (!is.null(input$ministry) && length(input$ministry) > 0) {
      data <- data %>% filter(ministry %in% input$ministry)
    }
    
    if (nzchar(input$keyword)) {
      data <- data %>%
        filter(str_detect(tolower(title), tolower(input$keyword)))
    }
    
    data
  })
  
  # =========================
  # KPIs
  # =========================
  output$kpi_total <- renderValueBox({
    valueBox(
      nrow(filtered_data()),
      "Total Press Releases",
      icon = icon("file-alt"),
      color = "aqua"
    )
  })
  
  output$kpi_years <- renderValueBox({
    valueBox(
      length(unique(filtered_data()$year)),
      "Years Covered",
      icon = icon("calendar"),
      color = "yellow"
    )
  })
  
  output$kpi_ministries <- renderValueBox({
    valueBox(
      length(unique(filtered_data()$ministry)),
      "Ministries Covered",
      icon = icon("building"),
      color = "green"
    )
  })
  
  # =========================
  # Time-series plot
  # =========================
  output$time_plot <- renderPlot({
    
    data <- filtered_data()
    
    plot_data <- switch(
      input$aggregation,
      "Daily" = data %>% count(date),
      "Monthly" = data %>% count(month),
      "Yearly" = data %>% count(year)
    )
    
    ggplot(plot_data, aes(x = 1:nrow(plot_data), y = n)) +
      geom_line() +
      geom_point() +
      labs(
        x = input$aggregation,
        y = "Number of Press Releases"
      ) +
      theme_minimal()
  })
  
  # =========================
  # Data table
  # =========================
  output$pib_table <- renderDT({
    
    table_data <- filtered_data()
    
    # Convert URL column into clickable links
    table_data$url <- paste0(
      '<a href="', table_data$url, '" target="_blank">',
      table_data$url,
      '</a>'
    )
    
    datatable(
      table_data,
      escape = FALSE,  # <-- THIS is critical
      options = list(
        pageLength = 10,
        autoWidth = TRUE
      ),
      filter = "top"
    )
  })
}

# =========================
# Run app
# =========================
shinyApp(ui = ui, server = server)


