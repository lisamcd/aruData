library(shiny)
library(tidyverse)


csv_url <- "https://raw.githubusercontent.com/lisamcd/ARU/refs/heads/main/gwdBirdsMay2025.csv"

ui <- fluidPage(
  titlePanel("Bird Observations Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("common_name_select", "Choose a species of bird:", choices = NULL),
      selectInput("aru_select", "Choose site(s):", choices = NULL, multiple = TRUE),
      numericInput("top_n_aru", "Choose the number of most common species to display at sites selected:", value = 10, min = 1, max = 140, step = 1)
    ),
    
    mainPanel(
      h3("Total number of bird observations for the sites selected"),
      plotOutput("total_obs_plot"),
      hr(),
      
      h3("Observations over time"),
      plotOutput("plot_date"),
      hr(),
      
      h3("Observations thoughout the day"),
      plotOutput("plot_time"),
      hr(),
      
      h3("Most commonly observed birds at site(s) selected"),
      h4("(as a percent of the total observations)"),
      fluidRow(
        column(6, tableOutput("aru_col1")),
        column(6, tableOutput("aru_col2"))
      ),
      hr(),
      
      h3("Most commonly observed birds across all Greenwood sites"),
      h4("(as a percent of the total observations)"),
      fluidRow(
        column(6, tableOutput("overall_col1")),
        column(6, tableOutput("overall_col2"))
  ))))

server <- function(input, output, session) {
  # Load data once reactively
  data <- reactive({
    read_csv(csv_url, show_col_types = FALSE)
  })
  
  observe({
    df <- data()
    updateSelectInput(session, "common_name_select", choices = sort(unique(df$common_name)))
    updateSelectInput(session, "aru_select", choices = unique(df$aru), selected = unique(df$aru)[1])
  })

  aru_ranked_birds <- reactive({
    req(input$aru_select, input$top_n_aru)
    df <- data() %>%
      filter(aru %in% input$aru_select)
    
    total_obs <- nrow(df)
    
    df %>%
      count(common_name, sort = TRUE) %>%
      mutate(
        percent = round(100 * n / total_obs, 2),
        rank = row_number()
      ) %>%
      slice(1:input$top_n_aru)
  })
  
  
  output$aru_col1 <- renderTable({
    df <- aru_ranked_birds()
    n <- nrow(df)
    cutoff <- ceiling(n / 2)
    df[1:cutoff, c("rank", "common_name", "percent")]
  })
  
  output$aru_col2 <- renderTable({
    df <- aru_ranked_birds()
    n <- nrow(df)
    cutoff <- ceiling(n / 2)
    if (n > cutoff) {
      df[(cutoff + 1):n, c("rank", "common_name", "percent")]
    } else {
      NULL
    }
  })
  

  overall_top25 <- reactive({
    data() %>%
      count(common_name, sort = TRUE) %>%
      mutate(percent = 100 * n / sum(n), rank = row_number()) %>%
      slice(1:25)
  })
  
  output$overall_col1 <- renderTable({
    df <- overall_top25()
    df[1:13, c("rank", "common_name", "percent")]
  })
  
  output$overall_col2 <- renderTable({
    df <- overall_top25()
    df[14:25, c("rank", "common_name", "percent")]
  })
  
  output$plot_date <- renderPlot({
    req(input$common_name_select, input$aru_select)
    df <- data() %>%
      filter(common_name == input$common_name_select,
             aru %in% input$aru_select)
    
    if (!inherits(df$date, "Date")) {
      df$date <- as.Date(df$date)
    }
    
    grouped <- df %>%
      count(date)
    
    ggplot(grouped, aes(x = date, y = n)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(title = paste("Number of observations of", input$common_name_select, "by date"),
           x = "Date", y = "Number of Observations") +
      theme_classic() +
      theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$plot_time <- renderPlot({
    req(input$common_name_select, input$aru_select)
    df <- data() %>%
      filter(common_name == input$common_name_select,
             aru %in% input$aru_select)
    
    grouped <- df %>%
      count(time)
    
    ggplot(grouped, aes(x = time, y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = paste("Number of observations of", input$common_name_select, "by time of day (24 hour clock)"),
           x = "Time", y = "Number of Observations") +
      theme_classic() +
      theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$total_obs_plot <- renderPlot({
    req(input$aru_select)
    
    df <- data() %>%
      filter(aru %in% input$aru_select)
    
    df_counts <- df %>%
      count(date)
    
    ggplot(df_counts, aes(x = date, y = n)) +
      geom_bar(stat = "identity", fill = "blue4") +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day") +
      #scale_x_date(date_labels = "%m-%d-%y", date_breaks = "1 day") +
      labs(
        #title = "Total observations of all species for the sites you selected",
        x = "Date", y = "Total observations"
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        #plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
}

shinyApp(ui = ui, server = server)