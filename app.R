#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(listviewer)

airbnb_data <- read.csv("AB_NYC_2019.csv")
# airbnb_data$price_range <- if (airbnb_data$price < 50) {
#     "$0-$50"
#     } else {
#         if (airbnb_data$price < 100) {
#             "$50-$100"
#             } else {
#                 if (airbnb_data$price < 150) {
#                 "$50-$100"
#                 } else {
#                     if (airbnb_data$price < 200) {
#                         "$150-$200"
#                         } else {
#                             if (airbnb_data$price < 250) {
#                                 "$200-$250"
#                             } else {
#                                 if (airbnb_data$price < 300) {
#                                     "$250-$300"
#                                 } else {
#                                     if (airbnb_data$price < 350) {
#                                         "$300-$350"
#                                     } else {
#                                         if (airbnb_data$price < 400) {
#                                             "$350-$400"
#                                         } else {
#                                             if (airbnb_data$price < 450) {
#                                                 "$400-$450"
#                                             } else {
#                                                 if (airbnb_data$price < 500) {
#                                                     "$450-$500"
#                                                 } else {
#                                                     "$500+"
#                                                 }
#                                             }
#                                         }
#                                     }
#                                 }
#                             }
#                     }
#                 }
#         }
#         }
                                                
                                            
                                        
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Airbnb Stays in New York City"),

    sidebarLayout(
        sidebarPanel(
            selectInput("borough", "Borough(s):", choices = sort(unique(airbnb_data$neighbourhood_group)), multiple = TRUE),
            selectInput("neighborhood", "Neighborhood(s):", choices = sort(unique(airbnb_data$neighbourhood)), multiple = TRUE),  # TODO: Cascading filter from Borough?
            # selectInput("pricerange", "Price Range(s):", 
            #             choices = c("$0-$50", "$50-$100", "$50-$100", 
            #                         "$150-$200", "$200-$250", "$250-$300", 
            #                         "$300-$350", "$350-$400", "$400-$450", 
            #                         "$450-$500", "$500+"),
            #             multiple = TRUE),
            # sliderInput("price_slider", label = "Price Range:", min = 0, max = max(airbnb_data$price), value = c(0, 100)),
            actionButton(
                inputId = "goButton",
                label = "Submit")
            
            # TODO: Add "Reset Filters" button to clear all filters
            ),

        mainPanel(
           plotlyOutput("map_plotly"),
           
           tabsetPanel(
               tabPanel("Price Distribution", plotlyOutput("pricehist")),
               tabPanel("Price by Borough", plotlyOutput("priceByBorough"))
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    airbnb_data <- read.csv("AB_NYC_2019.csv")
    
    # price_range <- reactive ({
    #     cbind(input$price_slider[1], input$price_slider[2])
    # })
    

        #         airbnb_data <- 
    #             filter(airbnb_data,
    #                    (airbnb_data$neighbourhood_group %in% borough_filter) &
    
    # airbnb_data$price_range <- if (airbnb_data$price < 50) {
    #     "$0-$50"
    # } else {
    #     if (airbnb_data$price < 100) {
    #         "$50-$100"
    #     } else {
    #         if (airbnb_data$price < 150) {
    #             "$50-$100"
    #         } else {
    #             if (airbnb_data$price < 200) {
    #                 "$150-$200"
    #             } else {
    #                 if (airbnb_data$price < 250) {
    #                     "$200-$250"
    #                 } else {
    #                     if (airbnb_data$price < 300) {
    #                         "$250-$300"
    #                     } else {
    #                         if (airbnb_data$price < 350) {
    #                             "$300-$350"
    #                         } else {
    #                             if (airbnb_data$price < 400) {
    #                                 "$350-$400"
    #                             } else {
    #                                 if (airbnb_data$price < 450) {
    #                                     "$400-$450"
    #                                 } else {
    #                                     if (airbnb_data$price < 500) {
    #                                         "$450-$500"
    #                                     } else {
    #                                         "$500+"
    #                                     }
    #                                 }
    #                             }
    #                         }
    #                     }
    #                 }
    #             }
    #         }
    #     }
    # }
    
    observeEvent(
        eventExpr = input$goButton,
        ignoreNULL = FALSE,
        handlerExpr = {
            
            # airbnb_data %>% filter(airbnb_data, (airbnb_data$price >= input$price_slider[1]) & (airbnb_data$price <= input$price_slider[2]))
            
            if (length(input$borough) == 0) {
                borough_filter <- unique(airbnb_data$neighbourhood_group)
            } else {
                borough_filter <- input$borough
            }

            if (length(input$neighborhood) == 0) {
                neighborhood_filter <- unique(airbnb_data$neighbourhood)
            } else {
                neighborhood_filter <- input$neighborhood
            }

            # if (length(input$pricerange) == 0) {
            #     pricerange_filter <- unique(airbnb_data$price_range)
            # } else {
            #     pricerange_filter <- input$pricerange
            # }


            airbnb_data <-
                filter(airbnb_data,
                       (airbnb_data$neighbourhood_group %in% borough_filter) &
                           (airbnb_data$neighbourhood %in% neighborhood_filter) #& 
                           # (airbnb_data$price_range %in% pricerange_filter)
                )
    
    ############################################################################
    
    g <- list(
        scope = 'usa',
        # lonaxis = list(range = c(-73, -74)), # TODO: Fix these axes
        # lataxis = list(range = c(41, 40)),
        # projection = list(type = 'albers usa'),
        showland = TRUE,
        landcolor = toRGB("gray95"),
        subunitcolor = toRGB("gray85"),
        countrycolor = toRGB("gray85"),
        countrywidth = 0.5,
        subunitwidth = 0.5
    )
    
    output$map_plotly <- renderPlotly({
        map_fig <- plotly::plot_geo(airbnb_data, lat = ~latitude, lon = ~longitude)
        map_fig <- map_fig %>% add_markers(
            text = ~paste(name, neighbourhood, sprintf("$%.2f per night", price), sep = "<br />"),
            color = ~price, symbol = I("square"), size = I(8), hoverinfo = "text"
        )
        map_fig <- map_fig %>% colorbar(title = "Price")
        map_fig <- map_fig %>% layout(title = "Airbnb Stays in NYC - Mapped", geo = g)
        
        map_fig
    })
    
    ############################################################################
    
    output$pricehist <- renderPlotly({
        hist_fig <- plot_ly(x = airbnb_data$price, 
                            type = "histogram", 
                            text = ~paste(airbnb_data$name, 
                                          airbnb_data$neighbourhood, 
                                          airbnb_data$price, 
                                          sep = "<br />")) %>% 
            layout(
                title = "Distribution of Airbnb Prices",
                xaxis = list(title = "Price ($)"),
                yaxis = list(title = "Count"))
        })
    
    ############################################################################
    
    output$priceByBorough <- renderPlotly({
        
        # https://plotly.com/r/aggregations/#aggregate-functions
        
        s <- schema()
        agg <- s$transforms$aggregate$attributes$aggregations$items$aggregation$func$values
        
        l = list()
        for (i in 1:length(agg)) {
            ll = list(method = "restyle",
                      args = list('transforms[0].aggregations[0].func', agg[i]), # TODO: Specify just a few aggs here (median, avg, min, max)
                      label = agg[i]) 
            l[[i]] = ll
        }
        
        # https://plotly.com/r/bar-charts/
        price_borough_fig <- plot_ly(
            type = "bar",
            x = airbnb_data$neighbourhood_group,
            y = airbnb_data$price,
            transforms = list(
                list(
                    type = "aggregate",
                    groups = airbnb_data$neighbourhood_group,
                    aggregations = list(
                        list(
                            target = "y", func = "median", enabled = TRUE
                        )
                    )
                )
            )
        )
        
        price_borough_fig <- price_borough_fig %>% layout(
            title = '<b>Plotly Aggregations</b><br>use dropdown to change aggregation',
            xaxis = list(title = 'Borough'),
            yaxis = list(title = 'Price ($)'),
            updatemenus = list(
                list(
                    x = 0.25,
                    y = 1.04,
                    xref = 'paper',
                    yref = 'paper',
                    yanchor = 'top',
                    buttons = l
                )
            )
        )
        
        price_borough_fig
    })
        })  # For observeEvent
}

# Run the application 
shinyApp(ui = ui, server = server)
