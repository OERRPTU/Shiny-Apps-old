####################################
# Shiny App zur Binomialverteilung #
####################################

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Binomialverteilung"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Versuchzahl n",
                        min = 0,
                        max = 100,
                        value = 30),
            sliderInput("p",
                        "Trefferwahrscheinlichkeit p",
                        min = 0,
                        max = 1,
                        value = 0.2)
  
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
    output$distPlot <- renderPlot({
    
        k<- seq(0,input$n,by=1)

        y<- dbinom(k,input$n,input$p)
     
        plot(k,y, lwd = 4, pch=19, type = "p", col = "darkgreen", xlab = "k", ylab = "P(T=k)", xlim =c(0,input$n))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
