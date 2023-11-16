###############################################
# Shiny App zur Hypergeometrischen Verteilung #
###############################################


# Load required libraries
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Hypergeometrische Verteilung"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("N", "N: Anzahl der Objekte insgesamt", min = 1, max = 100, value = 50),
      sliderInput("K", "K: Anzahl der ausgezeichneten Objekte", min = 1, max = 50, value = 25),
      sliderInput("n", "n: Anzahl der gezogenen Objekte", min = 1, max = 50, value = 10)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server
server <- function(input, output,session) {
  output$plot <- renderPlot({
   # set k an n  to be always smaller than N
     observe({
      if (input$K >= input$N) {
        updateSliderInput(session, "K", value = input$N - 1)
      }
      
       if (input$n >= input$N) {
         updateSliderInput(session, "n", value = input$N - 1)
       }
    })
    
    N <- input$N
    K <- input$K
    n <- input$n
    
    # Calculate the probabilities using the hypergeometric distribution
    k_values <- 0:K
    probabilities <- dhyper(k_values, K, N - K, n)
    
    # Plot the probabilities
    plot(k_values, probabilities, type = "p",pch=19, xlab = "k", ylab = "P(A = k)", main = "Hypergeometrische Verteilung")
  })
}

# Run the app
shinyApp(ui = ui, server = server)

