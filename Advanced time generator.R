# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

# Define UI for application
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("Advanced Time Series Dataset Generator"),
    sidebarLayout(
        sidebarPanel(
            numericInput("range_min", "Minimum Value:", value = 0, min = -100, step = 0.1),
            numericInput("range_max", "Maximum Value:", value = 100, min = -100, step = 0.1),
            numericInput("length", "Number of Data Points:", value = 100, min = 1, step = 1),
            selectInput("trend", "Trend Type:", choices = c("Increasing", "Decreasing", "Seasonal", "Random Walk")),
            numericInput("noise", "Noise Level (Standard Deviation):", value = 0, min = 0, step = 0.1),
            downloadButton("downloadData", "Download Dataset"),
            actionButton("generate", "Generate Dataset")
        ),
        mainPanel(
            h4("Generated Dataset"),
            tableOutput("dataset"),
            hr(),
            h4("Plot"),
            plotOutput("plot"),
            hr(),
            h4("Summary Statistics"),
            verbatimTextOutput("summary")
        )
    )
)

# Define server logic
server <- function(input, output) {
    generate_dataset <- eventReactive(input$generate, {
        range_min <- input$range_min
        range_max <- input$range_max
        length <- input$length
        trend <- input$trend
        noise <- input$noise
        
        set.seed(123) # For reproducibility
        data <- data.frame(Time = 1:length, Value = NA)
        
        if (trend == "Increasing") {
            data$Value <- seq(range_min, range_max, length.out = length)
        } else if (trend == "Decreasing") {
            data$Value <- seq(range_max, range_min, length.out = length)
        } else if (trend == "Seasonal") {
            data$Value <- range_min + (range_max - range_min) * sin(2 * pi * (1:length) / length)
        } else if (trend == "Random Walk") {
            data$Value <- cumsum(runif(length, min = -1, max = 1))
            data$Value <- range_min + (range_max - range_min) * (data$Value - min(data$Value)) / (max(data$Value) - min(data$Value))
        }
        
        if (noise > 0) {
            data$Value <- data$Value + rnorm(length, mean = 0, sd = noise)
        }
        
        return(data)
    })
    
    output$dataset <- renderTable({
        generate_dataset()
    })
    
    output$plot <- renderPlot({
        data <- generate_dataset()
        ggplot(data, aes(x = Time, y = Value)) +
            geom_line() +
            labs(title = "Generated Time Series Data", x = "Time", y = "Value") +
            theme_minimal()
    })
    
    output$summary <- renderPrint({
        data <- generate_dataset()
        summary(data$Value)
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("time_series_data_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(generate_dataset(), file, row.names = FALSE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

