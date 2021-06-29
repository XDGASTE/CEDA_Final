# Load packages ----

# Load data
source("test.r")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Taiwan President Election and Local Relationship"),

    # Input widgets
    sidebarLayout(
        sidebarPanel(
            helpText("Pleace choose the year and the map you would like:"),
            
            selectInput("Year", label = "Which year?",
                        choices = list("2004",
                                       "2008",
                                       "2012",
                                       "2016",
                                       "2020")),
            
            radioButtons("Show", label = "Which map?",
                         choices = list("Original Highest Vote Map" = "Orginal",
                                        "Local relationship factor" = "Index",
                                        "Adjuncted Vote Map" = "Corrected")),
            
            submitButton("Submit")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            conditionalPanel("input.Year == '2004' && input.Show == 'Orginal'",
                             leafletOutput("Map_O_2004")),
            conditionalPanel("input.Year == '2008' && input.Show == 'Orginal'",
                             leafletOutput("Map_O_2008")),
            conditionalPanel("input.Year == '2012' && input.Show == 'Orginal'",
                             leafletOutput("Map_O_2012")),
            conditionalPanel("input.Year == '2016' && input.Show == 'Orginal'",
                             leafletOutput("Map_O_2016")),
            conditionalPanel("input.Year == '2020' && input.Show == 'Orginal'",
                             leafletOutput("Map_O_2020")),
            conditionalPanel("input.Year == '2004' && input.Show == 'Index'",
                             leafletOutput("Map_I_2004")),
            conditionalPanel("input.Year == '2008' && input.Show == 'Index'",
                             leafletOutput("Map_I_2008")),
            conditionalPanel("input.Year == '2012' && input.Show == 'Index'",
                             leafletOutput("Map_I_2012")),
            conditionalPanel("input.Year == '2016' && input.Show == 'Index'",
                             leafletOutput("Map_I_2016")),
            conditionalPanel("input.Year == '2020' && input.Show == 'Index'",
                             leafletOutput("Map_I_2020")),
            conditionalPanel("input.Year == '2004' && input.Show == 'Corrected'",
                             leafletOutput("Map_C_2004")),
            conditionalPanel("input.Year == '2008' && input.Show == 'Corrected'",
                             leafletOutput("Map_C_2008")),
            conditionalPanel("input.Year == '2012' && input.Show == 'Corrected'",
                             leafletOutput("Map_C_2012")),
            conditionalPanel("input.Year == '2016' && input.Show == 'Corrected'",
                             leafletOutput("Map_C_2016")),
            conditionalPanel("input.Year == '2020' && input.Show == 'Corrected'",
                             leafletOutput("Map_C_2020")),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Map_Original = renderLeaflet({
        leaflet(Taiwan_T) %>%
            addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
                        fillColor = "blue") %>%
            setView(120.982024, 23.973875, zoom = 7)
        })
    output$Map_O_2004 = renderLeaflet({map_origin_2004})
    
    output$Map_O_2008 = renderLeaflet({map_origin_2008})
    
    output$Map_O_2012 = renderLeaflet({map_origin_2012})
    
    output$Map_O_2016 = renderLeaflet({map_origin_2016})
    
    output$Map_O_2020 = renderLeaflet({map_origin_2020})
        
    output$Map_I_2004 = renderLeaflet({map_Index_2004})
    
    output$Map_I_2008 = renderLeaflet({map_Index_2008})
    
    output$Map_I_2012 = renderLeaflet({map_Index_2012})
    
    output$Map_I_2016 = renderLeaflet({map_Index_2016})
    
    output$Map_I_2020 = renderLeaflet({map_Index_2020})
    
    output$Map_C_2004 = renderLeaflet({map_corrected_2004})
    
    output$Map_C_2008 = renderLeaflet({map_corrected_2008})
    
    output$Map_C_2012 = renderLeaflet({map_corrected_2012})
    
    output$Map_C_2016 = renderLeaflet({map_corrected_2016})
    
    output$Map_C_2020 = renderLeaflet({map_corrected_2020})
}

# Run the application 
shinyApp(ui = ui, server = server)
