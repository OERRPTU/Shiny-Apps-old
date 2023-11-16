#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("shiny")
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Titel"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId="plottype",
                      label="Diagrammart",
                      choices=c("Histogramm","Boxplot")
          ),
          uiOutput("histbins")
          
          ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           textOutput("daten"),
           textOutput("faith")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$daten <- renderText({
      "Benutzte Daten:"
    })
    output$faith <- renderText({
      print(faithful[, 2])
    })
    output$histbins <- renderUI({
    req(input$plottype == "Histogramm")
      sliderInput(inputId="bins",
                                    "Anzahl Intervalle:",
                                     min = 1,
                                    max = 30,
                                     value = 15)
  })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        if (input$plottype=="Histogramm")
         {
        # draw the histogram with the specified number of bins
         hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
          }
        else
          {
          boxplot(x)
          }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
