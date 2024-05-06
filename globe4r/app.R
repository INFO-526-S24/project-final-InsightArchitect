
library(shiny)
library(globe4r)
library(dplyr)
library(quarto)

# Read the dataset only once outside the server function to improve performance
pip_final <- read.csv("cleaned_pip_final.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Global Poverty Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("yearInput", "Select Year:", choices = unique(pip_final$year)),
      selectInput("pppInput", "Select PPP Version:", choices = unique(pip_final$ppp_version))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Globe 1", globeOutput("globe1")),
        tabPanel("Globe 2", globeOutput("globe2")),
        tabPanel("Globe 3", globeOutput("globe3")),
        tabPanel("Globe 4", globeOutput("globe4"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter data based on user input
  filteredData <- reactive({
    pip_final %>%
      filter(year == input$yearInput, ppp_version == input$pppInput)
  })
  
  # Prepare the dataset for globe plotting for each metric
  globeData <- function(data, metric_scaled) {
    data %>%
      mutate(
        scaled_value = 
          (metric_scaled - min(metric_scaled, na.rm = TRUE)) /
          (max(metric_scaled, na.rm = TRUE) - min(metric_scaled, na.rm = TRUE)),
        Latitude = round(as.numeric(Latitude), 2),
        Longitude = round(as.numeric(Longitude), 2)
      )
  }
  
  # Set up color palettes
  colors_frostbite <- colorRampPalette(c("#d8f3ff", "#000046"))(100)
  colors_electric_lime <- colorRampPalette(c("#a8e063", "#007f5f"))(100)
  colors_lavender <- colorRampPalette(c("#e6e6fa", "#967bb6"))(100)
  colors_maroon <- colorRampPalette(c("#c15050", "#800000"))(100)
  
  # Render each globe using different metrics and colors
  renderGlobeHelper <- function(outputId, colorPalette, metric) {
    output[[outputId]] <- renderGlobe({
      data <- globeData(filteredData(), filteredData()[[metric]])
      create_globe(height = "100vh") %>%
        labels_data(data = data) %>%
        labels_lat(lat = "Latitude") %>%
        labels_lon(lon = "Longitude") %>%
        labels_text(text = "Country") %>%
        labels_altitude(altitude = 30) %>%
        globe_choropleth(
          coords(
            country = Country.Code,
            altitude = scaled_value,
            cap_color = scaled_value
          ),
          data = data
        ) %>%
        polygons_side_color(color = constant("#ffffaa60")) %>%
        scale_choropleth_cap_color(colorPalette) %>%
        scale_choropleth_altitude(0.04, 0.2) %>%
        globe_img_url(image_url("blue")) %>%
        globe_rotate(speed = 0.5, rotate = TRUE)
    })
  }
  
  # Apply the helper function to each globe
  renderGlobeHelper("globe1", colors_frostbite, "headcount_ratio_international_povline")
  renderGlobeHelper("globe2", colors_electric_lime, "gini")
  renderGlobeHelper("globe3", colors_lavender, "avg_shortfall_international_povline")
  renderGlobeHelper("globe4", colors_maroon, "income_gap_ratio_international_povline")
}

# Run the application
shinyApp(ui = ui, server = server)
