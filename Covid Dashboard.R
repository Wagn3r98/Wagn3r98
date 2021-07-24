
'Source: Johns Hopkins University Center for Systems Science and Engineering'
'Updated on a daily basis'


library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(DT)
library(maps)

{
coords <- read_csv("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv") %>%
  select(2:4) %>%
  unique


confirmed <- read_csv("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv") %>%
  gather("Date","Cases",5:ncol(.)) %>%
  mutate(Date = as.Date(Date, 
                        format = "%m/%d/%y")) %>%
  select(-`Province/State`,
         -`Lat`,
         -`Long`) %>%
  group_by(`Country/Region`,
           `Date`) %>%
  summarise(Cases = sum(Cases))


deaths <- read_csv("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv&filename=time_series_covid19_deaths_global.csv") %>%
  gather("Date","Deaths",5:ncol(.)) %>%
  mutate(`Date` = as.Date(Date,
                          format = "%m/%d/%y")) %>%
  select(-`Province/State`,
         -`Lat`,
         -`Long`) %>%
  group_by(`Country/Region`,
           `Date`) %>%
  summarise(Deaths = sum(Deaths))


recovered <- read_csv("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_recovered_global.csv&filename=time_series_covid19_recovered_global.csv") %>%
  gather("Date","Recovered",5:ncol(.)) %>%
  mutate(`Date` = as.Date(Date,
                          format = "%m/%d/%y")) %>%
  select(-`Province/State`,
         -`Lat`,
         -`Long`) %>%
  group_by(`Country/Region`,
           `Date`) %>%
  summarise(Deaths = sum(Recovered))  
  

covid <- confirmed %>%
  left_join(deaths,
            by = c("Country/Region",
                   "Date")) %>%
  left_join(recovered,
            by = c("Country/Region",
                   "Date")) %>%  
  rename("Deaths" = "Deaths.x",
         "Recovered" = "Deaths.y")

}  
'ok there is a dataset covering it all ... plus more, so lets take this then :('


covid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  select(-1:-2,
         -44:-60,
         -contains("smoothed")) %>%
  mutate_at(c(3:ncol(.)), ~replace(., is.na(.), 0))


country.info <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  select(1:3,45:60) %>%
  unique


# Define UI for application that draws a histogram
ui <- {fluidPage(
  
  # Application title
  titlePanel("Covid Dashboard"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3("Global Data for Cases, Death, Tests,"),
      h3("Hospital Patients and Vaccination"),
      br(),
      br(),
      sliderInput("date",
                  "Date Range:",
                  min = min(covid$date),
                  max = max(covid$date),
                  value = c(Sys.Date()-14,
                            Sys.Date())),
      selectInput("country",
                  "Country:",
                  country.info$location,
                  selected = "Germany",
                  multiple = T),
      selectInput("measures",
                  "Select Measure(s):",
                  names(covid)[-1:-2],
                  multiple = T,
                  selected = "new_cases"),
      selectInput("smooth",
                  "Smoothing Days",
                  0:100,
                  0)),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 plotOutput("plot")),
        tabPanel("Map", 
                 h3("Select only one measure"),
                 
                 div(style = "position:relative",   #div: html element grouping the input as separate box, used because of overlap
                     plotOutput("map",
                                hover = hoverOpts("plot_hover", 
                                                  delay = 50, 
                                                  delayType = "debounce")),
                     uiOutput("hover_info"))),
        
        tabPanel("Country-Info",
                 dataTableOutput("country")),
        tabPanel("Deepdive", 
                 dataTableOutput("table"))
      )
    )
  )
)}

# Define server logic required to draw a histogram
server <- function(input, output) {
  

 
  output$table <- renderDataTable({
    covid %>%
      filter(date > input$date[1] & date < input$date[2],
             location %in% input$country) %>%
      select(location,
             date,
             input$measures)})
  
  output$plot <- renderPlot({covid %>%
                              filter(date > input$date[1] & date < input$date[2],
                                     location %in% input$country) %>%
                              select(date,
                                     location,
                                     input$measures) %>%
                              gather("metric",
                                     "value",
                                     3:ncol(.)) %>%
                              ggplot(aes(x = date,
                                         y = value)) +
                              geom_point(aes(color = metric),
                                         alpha = 0.8,
                                         size = 1.5) +
                              theme_light() +
                              theme(legend.position = "bottom", 
                                    legend.title = element_blank()) +
                              scale_y_continuous(name="count", 
                                                 labels = scales::comma)},
                            height = 500)
  
  output$map <- renderPlot({temp <- sym(input$measures[1])
                            
                            map_data("world") %>%
                                select(-group,
                                       -subregion,
                                       -order) %>%
                                group_by(region) %>%
                                summarise(lat,
                                          long,
                                          region,
                                          m.lat  = mean(lat),
                                          m.long = mean(long)) %>%
                                unique %>%
                                left_join(covid %>%
                                            filter(date > input$date[1] & date < input$date[2]) %>%
                                            select(!!temp) %>% 
                                            aggregate(by  = list(location = covid %>%
                                                                              filter(date > input$date[1] & date < input$date[2]) %>%
                                                                              select(location) %>%
                                                                              unlist),
                                                      FUN = sum),
                                          by = c("region" = "location")) %>%
                                filter(is.na(!!temp) == F) %>%                   
                                ggplot() +
                                geom_point(aes(x = long,
                                               y = lat),
                                           size = 0.1,
                                           col = "grey",                
                                           alpha = 0.3) +
                                geom_point(aes(x = m.long,
                                               y = m.lat,
                                               size = 1.5 * !!temp / mean(!!temp, 
                                                                          na.rm = T))) +  
                                theme_light() +
                                xlab("") +
                                ylab("") +
                                labs(size=paste("mean of '",
                                                gsub("_",
                                                     " ",
                                                     as.character(temp)),
                                                "per day'")) +
                                theme(legend.position = "bottom")},
                           height = 500)
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(map_data("world") %>%
                          select(-group,
                                 -subregion,
                                 -order) %>%
                          group_by(region) %>%
                          summarise(region,
                                    lat,
                                    long,
                                    m.lat  = mean(lat),
                                    m.long = mean(long)) %>%
                          unique %>%
                          left_join(covid %>%
                                      filter(date > input$date[1] & date < input$date[2]) %>%
                                      select(c("new_cases","new_deaths")) %>%
                                      aggregate(by  = list(location = covid %>%
                                                             filter(date > input$date[1] & date < input$date[2]) %>%
                                                             select(location) %>%
                                                             unlist),
                                                FUN = sum),
                                    by = c("region" = "location")) %>%
                          filter(is.na(!!temp) == F), 
                          hover, 
                          threshold = 5, 
                          maxpoints = 1, 
                          addDist = T)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct  <- (hover$domain$top - hover$y)  / (hover$domain$top   - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right  - hover$range$left)
    top_px  <- hover$range$top  + top_pct  * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Country: </b>", point$region, "<br/>",
                    "<b> New Cases: </b>", point$new_cases, "<br/>",
                    "<b> New Deaths: </b>", point$new_deaths, "<br/>")))
    )
  })

  output$country <- renderDataTable({country.info %>%
                                        filter(location %in% input$country)})}

# Run the application 
shinyApp(ui = ui,
         server = server)
