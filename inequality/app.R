library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Assuming data is loaded here
data <- read.csv("cleaned_pip_final.csv")

ui <- fluidPage(
  titlePanel("Global Poverty and Inequality Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countryInput", "Select Country:",
                  choices = unique(data$Country)),
      sliderInput("yearInput", "Select Year Range:",
                  min = min(data$year), max = max(data$year),
                  value = c(min(data$year), max(data$year)))
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Gini Coefficient Trend", plotlyOutput("giniPlot")),
                  tabPanel("Palma Ratio", plotlyOutput("palmaPlot")),
                  tabPanel("P90/P10 Ratio", plotlyOutput("p90p10Plot")),
                  tabPanel("S80/S20 Ratio", plotlyOutput("s80s20Plot"))
      )
    )
  )
)

server <- function(input, output) {
  filteredData <- reactive({
    data %>%
      filter(Country == input$countryInput, year >= input$yearInput[1], year <= input$yearInput[2])
  })
  
  output$giniPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = year, y = gini, color = welfare_type, shape = welfare_type)) +
      geom_point(size = 3) +
      geom_line(aes(group = welfare_type), size = 1) +
      labs(title = "Trend of Gini Coefficient Over Time",
           x = "Year",
           y = "Gini Coefficient",
           color = "Welfare Type",
           shape = "Welfare Type") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  output$palmaPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = year, y = palma_ratio, color = welfare_type, shape = welfare_type)) +
      geom_point(size = 3) +
      geom_line(aes(group = welfare_type), size = 1) +
      labs(title = "Trend of Palma Ratio Over Time",
           x = "Year",
           y = "Palma Ratio",
           color = "Welfare Type",
           shape = "Welfare Type") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  output$p90p10Plot <- renderPlotly({
    # 3D scatter plot for P90/P10 Ratio
    plot_ly(filteredData(), x = ~year, y = ~p90_p10_ratio, z = ~welfare_type, type = "scatter3d", mode = "markers",
            marker = list(size = 5, color = ~p90_p10_ratio, colorscale = 'Viridis', showscale = TRUE),
            layout = list(title = "P90/P10 Ratio Over Time",
                          showlegend = TRUE,
                          scene = list(xaxis = list(title = "Year"),
                                       yaxis = list(title = "P90/P10 Ratio"),
                                       zaxis = list(title = "Welfare Type"),
                                       aspectmode = 'cube')))
  })
  
  output$s80s20Plot <- renderPlotly({
    # 3D scatter plot for S80/S20 Ratio
    plot_ly(filteredData(), x = ~year, y = ~s80_s20_ratio, z = ~welfare_type, type = "scatter3d", mode = "markers",
            marker = list(size = 5, color = ~s80_s20_ratio, colorscale = 'Viridis', showscale = TRUE),
            layout = list(title = "S80/S20 Ratio Over Time",
                          showlegend = TRUE,
                          scene = list(xaxis = list(title = "Year"),
                                       yaxis = list(title = "S80/S20 Ratio"),
                                       zaxis = list(title = "Welfare Type"),
                                       aspectmode = 'cube')))
  })
}

shinyApp(ui = ui, server = server)

