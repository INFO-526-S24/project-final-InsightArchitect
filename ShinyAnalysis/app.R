
library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(viridis)
library(ggplot2)

# Load data
your_data <- read_csv('pip_dataset.csv')  # Assuming similar structure for both datasets
data <- read_csv("cleaned_pip_final.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Global Inequality and Poverty Dashboards"),
  tabsetPanel(
    tabPanel("Income Inequality Dashboard",
             sidebarLayout(
               sidebarPanel(
                 selectInput("country", "Select Country:", choices = unique(your_data$country)),
                 sliderInput("year_range", "Select Year Range:",
                             min = min(your_data$year), max = max(your_data$year),
                             value = c(min(your_data$year), max(your_data$year)),
                             step = 1)
               ),
               mainPanel(
                 plotlyOutput("filtered_heatmap_plot")
               )
             )),
    tabPanel("Dynamic Data Visualizations",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selectCountry", "Select Country:", choices = unique(data$Country)),
                 sliderInput("selectYear", "Select Year:",
                             min = min(data$year), max = max(data$year),
                             value = c(min(data$year), max(data$year)), step = 5, sep = ""),
                 selectInput("selectChart", "Select Chart Type:",
                             choices = c("Heatmap", "Line Graph", "Headcount Ratios"))
               ),
               mainPanel(
                 plotlyOutput("plot")
               )
             )),
    tabPanel("Poverty Metrics Visualization",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selectCountry2", "Select Country:", choices = unique(data$Country)),
                 sliderInput("selectYear2", "Select Year Range:",
                             min = min(data$year), max = max(data$year),
                             value = c(min(data$year), max(data$year)))
               ),
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Poverty Line $1", plotlyOutput("plot1")),
                             tabPanel("Poverty Line $10", plotlyOutput("plot10")),
                             tabPanel("Poverty Line $20", plotlyOutput("plot20")),
                             tabPanel("Poverty Line $30", plotlyOutput("plot30")),
                             tabPanel("Poverty Line $40", plotlyOutput("plot40"))
                 )
               )
             )),
    tabPanel("Advanced Poverty and Inequality Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selectCountry3", "Select Country:", choices = c("All", unique(data$Country))),
                 sliderInput("yearInput3", "Select Year Range:", 
                             min = min(data$year), 
                             max = max(data$year), 
                             value = range(data$year)),
                 selectInput("welfareInput", "Select Welfare Type:", 
                             choices = unique(data$welfare_type))
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Poverty Gap Index", plotlyOutput("mapPlot1")),
                   tabPanel("Palma Ratio", plotlyOutput("mapPlot2")),
                   tabPanel("Palma Ratio Time Series", plotlyOutput("timeSeriesPlot"))  # New tab for time series plot
                 )
               )
             ))
  )
)

# Define server logic
server <- function(input, output) {
  # Dashboard 1: Income Inequality
  filtered_data <- reactive({
    your_data %>%
      filter(country == input$country & year >= input$year_range[1] & year <= input$year_range[2])
  })
  
  output$filtered_heatmap_plot <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~year, y = ~country, z = ~gini, type = "heatmap", colorscale = "Viridis") %>%
      layout(title = paste("Filtered Income Inequality Heatmap for", input$country),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Country"),
             colorbar = list(title = "Gini Index"),
             dragmode = 'zoom')
  })
  
  # Dashboard 3: Dynamic Data Visualizations
  filteredData <- reactive({
    complete_data <- expand.grid(Country = unique(data$Country), year = seq(min(data$year), max(data$year)))
    merged_data <- merge(data, complete_data, by = c("Country", "year"), all = TRUE)
    merged_data[is.na(merged_data)] <- 0  # Fill NA values with 0
    merged_data %>%
      filter(Country %in% input$selectCountry, year >= input$selectYear[1], year <= input$selectYear[2])
  })
  
  output$plot <- renderPlotly({
    df <- filteredData()  # Capture the reactive data frame for use in plotting
    if(input$selectChart == "Heatmap") {
      p <- ggplot(df, aes(x = year, y = Country, fill = headcount_ratio_international_povline)) +
        geom_tile() +
        labs(title = "Heatmap of Poverty Levels", x = "Year", y = "Country") +
        scale_fill_viridis_c(option = "plasma", na.value = "lightgray")
      ggplotly(p)
    } else if(input$selectChart == "Line Graph") {
      plot_ly(df, x = ~year) %>%
        add_lines(y = ~mean, name = "Mean", line = list(color = 'blue'), mode = 'lines+markers', marker = list(symbol = "circle")) %>%
        add_lines(y = ~median, name = "Median", line = list(color = 'red'), mode = 'lines+markers', marker = list(symbol = "square")) %>%
        layout(title = "Trend of Mean and Median Incomes")
    } else if(input$selectChart == "Headcount Ratios") {
      p <- plot_ly(df, x = ~year) %>%
        add_lines(y = ~headcount_ratio_100, name = "$1/day", line = list(shape = 'linear', color = 'red'), marker = list(symbol = "circle")) %>%
        add_lines(y = ~headcount_ratio_1000, name = "$10/day", line = list(shape = 'spline', color = 'blue'), marker = list(symbol = "square")) %>%
        add_lines(y = ~headcount_ratio_2000, name = "$20/day", line = list(shape = 'vhv', color = 'green'), marker = list(symbol = "diamond")) %>%
        add_lines(y = ~headcount_ratio_3000, name = "$30/day", line = list(shape = 'vh', color = 'purple'), marker = list(symbol = "cross")) %>%
        add_lines(y = ~headcount_ratio_4000, name = "$40/day", line = list(shape = 'hv', color = 'orange'), marker = list(symbol = "triangle-up")) %>%
        layout(title = "Headcount Ratios Over Time", yaxis = list(title = "Percentage"), xaxis = list(title = "Year"),
               updatemenus = list(list(type = "buttons",
                                       buttons = list(list(method = "restyle",
                                                           args = list("visible", list(TRUE, FALSE, FALSE, FALSE, FALSE)),
                                                           label = "$1/day"),
                                                      list(method = "restyle",
                                                           args = list("visible", list(FALSE, TRUE, FALSE, FALSE, FALSE)),
                                                           label = "$10/day"),
                                                      list(method = "restyle",
                                                           args = list("visible", list(FALSE, FALSE, TRUE, FALSE, FALSE)),
                                                           label = "$20/day"),
                                                      list(method = "restyle",
                                                           args = list("visible", list(FALSE, FALSE, FALSE, TRUE, FALSE)),
                                                           label = "$30/day"),
                                                      list(method = "restyle",
                                                           args = list("visible", list(FALSE, FALSE, FALSE, FALSE, TRUE)),
                                                           label = "$40/day")))))
      p
    }
  })
  
  # Dashboard 3.2: Poverty Metrics Visualization
  filteredData2 <- reactive({
    data %>%
      filter(Country == input$selectCountry2, year >= input$selectYear2[1], year <= input$selectYear2[2])
  })
  
  # Function to create each plot
  output$plot1 <- renderPlotly({
    df <- filteredData2()
    plot_ly(df, x = ~year, mode = "lines", stackgroup = "one") %>%
      add_trace(y = ~income_gap_ratio_100, name = "Income Gap Ratio $1", fillcolor = 'rgba(255, 0, 0, 0.5)') %>%
      add_trace(y = ~total_shortfall_100, name = "Total Shortfall $1", fillcolor = 'rgba(0, 0, 255, 0.5)') %>%
      layout(title = paste("Metrics for Poverty Line $1/day in", input$selectCountry2),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Value"),
             showlegend = TRUE)
  })
  
  # Similar functions for other plots
  output$plot10 <- renderPlotly({
    df <- filteredData2()
    plot_ly(df, x = ~year, mode = "lines", stackgroup = "one") %>%
      add_trace(y = ~income_gap_ratio_1000, name = "Income Gap Ratio $10", fillcolor = 'rgba(255, 0, 0, 0.5)') %>%
      add_trace(y = ~total_shortfall_1000, name = "Total Shortfall $10", fillcolor = 'rgba(0, 0, 255, 0.5)') %>%
      layout(title = paste("Metrics for Poverty Line $10/day in", input$selectCountry2),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Value"),
             showlegend = TRUE)
  })
  
  # Additional plot configurations for $20, $30, $40 lines
  output$plot20 <- renderPlotly({
    df <- filteredData2()
    plot_ly(df, x = ~year, mode = "lines", stackgroup = "one") %>%
      add_trace(y = ~income_gap_ratio_2000, name = "Income Gap Ratio $20", fillcolor = 'rgba(255, 0, 0, 0.5)') %>%
      add_trace(y = ~total_shortfall_2000, name = "Total Shortfall $20", fillcolor = 'rgba(0, 0, 255, 0.5)') %>%
      layout(title = paste("Metrics for Poverty Line $20/day in", input$selectCountry2),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Value"),
             showlegend = TRUE)
  })
  
  output$plot30 <- renderPlotly({
    df <- filteredData2()
    plot_ly(df, x = ~year, mode = "lines", stackgroup = "one") %>%
      add_trace(y = ~income_gap_ratio_3000, name = "Income Gap Ratio $30", fillcolor = 'rgba(255, 0, 0, 0.5)') %>%
      add_trace(y = ~total_shortfall_3000, name = "Total Shortfall $30", fillcolor = 'rgba(0, 0, 255, 0.5)') %>%
      layout(title = paste("Metrics for Poverty Line $30/day in", input$selectCountry2),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Value"),
             showlegend = TRUE)
  })
  
  output$plot40 <- renderPlotly({
    df <- filteredData2()
    plot_ly(df, x = ~year, mode = "lines", stackgroup = "one") %>%
      add_trace(y = ~income_gap_ratio_4000, name = "Income Gap Ratio $40", fillcolor = 'rgba(255, 0, 0, 0.5)') %>%
      add_trace(y = ~total_shortfall_4000, name = "Total Shortfall $40", fillcolor = 'rgba(0, 0, 255, 0.5)') %>%
      layout(title = paste("Metrics for Poverty Line $40/day in", input$selectCountry2),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Value"),
             showlegend = TRUE)
  })
  
  
  
  # Dashboard 4: Advanced Poverty and Inequality Analysis
  filteredData3 <- reactive({
    data <- data %>%
      filter(year >= input$yearInput3[1], year <= input$yearInput3[2], welfare_type == input$welfareInput)
    if (input$selectCountry3 != "All") {
      data <- data %>% filter(Country == input$selectCountry3)
    }
    data
  })
  
  output$mapPlot1 <- renderPlotly({
    data <- filteredData3()
    plot_ly(data, type = "choropleth", locations = ~Country, locationmode = "country names",
            z = ~poverty_gap_index_international_povline, colorscale = "Viridis",
            marker = list(line = list(color = "rgb(255,255,255)", width = 2))) %>%
      layout(title = "Poverty Gap Index (International Poverty Line)",
             geo = list(scope = "world", showcountries = TRUE))
  })
  
  output$mapPlot2 <- renderPlotly({
    data <- filteredData3()
    plot_ly(data, type = "choropleth", locations = ~Country, locationmode = "country names",
            z = ~palma_ratio, colorscale = "Viridis",
            marker = list(line = list(color = "rgb(255,255,255)", width = 2))) %>%
      layout(title = "Palma Ratio", geo = list(scope = "world", showcountries = TRUE))
  })
  
  output$timeSeriesPlot <- renderPlotly({
    data <- filteredData3()
    plot_ly(data, x = ~year, y = ~palma_ratio, type = 'scatter', mode = 'lines+markers',
            line = list(width = 2), marker = list(size = 4), color = ~Country,
            hoverinfo = 'text', text = ~paste("Year:", year, "<br>Country:", Country, "<br>Palma Ratio:", palma_ratio),
            showlegend = TRUE) %>%
      layout(title = "Time Series of Palma Ratio",
             xaxis = list(title = "Year"), yaxis = list(title = "Palma Ratio"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
