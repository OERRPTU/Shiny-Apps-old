##################################
# Shiny App zur Normalverteilung #
##################################

#install.packages("latex2exp")
library(shiny)
library(latex2exp)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Gaußsche Funktion"),
  # \uxxxx codes für griechische Buchstaben: http://www.javascripter.net/faq/greekletters.htm
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu",
                  "Erwartungswert	\u03BC",
                  min = -10,
                  max = 10,
                  value = 4),
      
      sliderInput("sigma",
                 "Standardabweichung \u03C3",
                  min = 0,
                  max = 8,
                  value = 2,
                  step=0.01),
      checkboxInput("Wdichte",
                    "Wahrscheinlichkeitsdichte \u03C6",
                    value = TRUE
                    ),
      checkboxInput("Verteilungsfknt",
                    "Verteilungsfunktion \u03A6",
                    value = FALSE
                    )
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
    # define my Gauss function
    gaussianwdichte <- function(x, mu, sigma) {
      return(dnorm(x, mean = mu, sd = sigma))
    }
    gaussianverteilungsfkt <- function(x, mu, sigma) {
      return(pnorm(x, mean = mu, sd = sigma))
    }
    
    # generate bins based on input$bins from ui.R
    x    <- seq(-10,10,length.out=10000)
    
    # Gaußsche Funktion für die gegebenen Parameterwerte berechnen
    ywdichte <- gaussianwdichte(x, input$mu, input$sigma)
    
    yverteilungsfkt <- gaussianverteilungsfkt(x, input$mu, input$sigma)
    # Plot der Gaußschen Funktion
    
    if (input$Wdichte & input$Verteilungsfknt)
      {
      plot(x, ywdichte, type = "l", xlab = "t,x", ylab=TeX("$\\phi (\\mu, \\sigma ) , \\Phi (\\mu, \\sigma )$"),col="darkgreen",lwd=3, main = "Gaußsche Funktion",ylim=c(0,1))
      lines(x, yverteilungsfkt, col="red",lwd=3)
      legend("topleft", legend = c(TeX("$\\phi$"),TeX("$\\Phi$"))  ,cex = 1, col = c( "darkgreen","red"),
             title.adj =0, lty = 1, lwd=2, box.lty = 1)
      }
    else if (input$Wdichte & !input$Verteilungsfknt)
    {
    plot(x, ywdichte, type = "l", xlab = "t,x", ylab = TeX("$\\phi (\\mu, \\sigma ) $"),col="darkgreen",lwd=3, main = "Gaußsche Funktion",ylim=c(0,1))
      legend("topleft", legend = c(TeX("$\\phi$"),TeX("$\\Phi$"))  ,cex = 1, col = c( "darkgreen","red"),
             title.adj =0, lty = 1, lwd=2, box.lty = 1)
    }
    else if (!input$Wdichte & input$Verteilungsfknt)
    {
      plot(x, yverteilungsfkt, type = "l", xlab = "t,x", ylab=TeX("$ \\Phi (\\mu, \\sigma )$"),col="red",lwd=3, main = "Gaußsche Funktion",ylim=c(0,1)) 
      legend("topleft", legend = c(TeX("$\\phi$"),TeX("$\\Phi$"))  ,cex = 1, col = c( "darkgreen","red"),
             title.adj =0, lty = 1, lwd=2, box.lty = 1)
    } 
    else
    {
     plot(NULL, xlab = "t,x", ylab=TeX("$\\phi (\\mu, \\sigma ) , \\Phi (\\mu, \\sigma )$"),col="red",lwd=3, main = "Gaußsche Funktion",ylim=c(0,1),xlim=c(-10,10)) 
      legend("topleft", legend = c(TeX("$\\phi$"),TeX("$\\Phi$"))  ,cex = 1, col = c( "darkgreen","red"),
             title.adj =0, lty = 1, lwd=2, box.lty = 1)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
