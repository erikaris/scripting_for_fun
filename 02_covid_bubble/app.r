  # More info:
  #   https://github.com/jcheng5/googleCharts
  # Install:
    # devtools::install_github("jcheng5/googleCharts")
  library(googleCharts)
  library(shiny)
  library(dplyr)
  library(tidyverse)
  
  # data_healthexp <- readRDS("healthexp.Rds")
  # import the data set. 
  # data source: https://covid19.who.int/ accessed on 4/23/2020 1.28 pm GMT+7
  data <- read_csv("WHO-COVID-19-global-data.csv", na = c("", "NA", "NULL", "NaN", "null", "Null", "<NA>"))
  # rename the columns since we can't work with column whose name contains whitespace. 
  names(data) <- c("day", "country", "country_name", "region", "deaths", "cum_deaths", "confirmed", "cum_confirmed")
  
  # replace the region name 
  data <- data %>% 
            mutate(region = replace(region, region == "AFRO", "Africa"), 
               region = replace(region, region == "AMRO", "Americas"), 
               region = replace(region, region == "EMRO", "Eastern Mediterranean"), 
               region = replace(region, region == "EURO", "Europe"), 
               region = replace(region, region == "SEARO", "South East Asia"), 
               region = replace(region, region == "WPRO", "Western Pacific"))
  
  data$region <- as.factor(data$region)
  

  # Use global max/min for axes so the view window stays
  # constant as the user moves between years
  xlim <- list(
    # min = min(data$cum_confirmed) - 500,
    min = min(data$cum_confirmed) - 50000,
    # max = max(data$cum_confirmed) + 500
    max = max(data$cum_confirmed) + 50000
  )
  ylim <- list(
    min = min(data$cum_deaths) - 10000,
    max = max(data$cum_deaths) + 10000
  )
  
  ui <- fluidPage(
    # This line loads the Google Charts JS library
    googleChartsInit(),
  
    # Use the Google webfont "Source Sans Pro"
    tags$link(
      href=paste0("http://fonts.googleapis.com/css?",
                  "family=Source+Sans+Pro:300,600,300italic"),
      rel="stylesheet", type="text/css"),
    tags$style(type="text/css",
      "body {font-family: 'Source Sans Pro'}"
    ),
  
    h2("Global Covid-19 Cases as Reported to WHO on 4/23/2020"),
  
    googleBubbleChart("chart",
      width="100%", height = "475px",
      # Set the default options for this chart; they can be
      # overridden in server.R on a per-update basis. See
      # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
      # for option documentation.
      options = list(
        fontName = "Source Sans Pro",
        fontSize = 13,
        bold = TRUE, 
        # Set axis labels and ranges
        hAxis = list(
          title = "Cumulative Confirmed Cases (persons)",
          viewWindow = xlim
        ),
        vAxis = list(
          title = "Cumulative Deaths (persons)",
          viewWindow = ylim
        ),
        # The default padding is a little too spaced out
        chartArea = list(
          top = 50, left = 75,
          height = "75%", width = "75%"
        ),
        # Allow pan/zoom
        explorer = list(),
        # Set bubble visual props
        bubble = list(
          opacity = 0.4, stroke = "none",
          # Hide bubble label
          textStyle = list(
            color = "none"
          )
        ),
        # Set fonts
        titleTextStyle = list(
          fontSize = 16, 
          bold = TRUE
        ),
        tooltip = list(
          textStyle = list(
            fontSize = 12
          )
        )
      )
    ),
    fluidRow(
      shiny::column(4, offset = 4,
        sliderInput("day", "Day",
          min = min(data$day), max = max(data$day),
          value = min(data$day), animate = animationOptions(interval = 90, loop = TRUE))
      )
    ),
    
    h4("The bubble size represents the number of daily confirmed cases (not the cumulative one)")
  )
  
  
  server <- function(input, output, session) {
  
    # Provide explicit colors for regions, so they don't get recoded when the
    # different series happen to be ordered differently from year to year.
    # http://andrewgelman.com/2014/09/11/mysterious-shiny-things/
    defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")
    series <- structure(
      lapply(defaultColors, function(color) { list(color=color) }),
      names = levels(data$region)
    )
  
    dayData <- reactive({
      # Filter to the desired year, and put the columns
      # in the order that Google's Bubble Chart expects
      # them (name, x, y, color, size). Also sort by region
      # so that Google Charts orders and colors the regions
      # consistently.
      df <- data %>%
        filter(day == input$day) %>%
        select(country_name, cum_confirmed, cum_deaths,
          region, confirmed) %>% # population is removed cause it's not available. 
        arrange(region)
    })
  
    output$chart <- reactive({
      # Return the data and options
      list(
        data = googleDataTable(dayData()),
        options = list(
          title = sprintf(
            "Cumulative Confirmed Cases vs. Cumulative Confirmed Deaths, %s",
            input$day),
          series = series
        )
      )
    })
  }
  
  shinyApp(ui = ui, server = server)
