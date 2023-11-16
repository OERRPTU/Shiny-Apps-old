###############################################
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(latex2exp)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Gleichverteilte Zufallsvariable  auf dem Intervall [a, b]"),
  # \uxxxx codes für griechische Buchstaben: http://www.javascripter.net/faq/greekletters.htm
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("b",
                  "Parameter	b",
                  min = 0,
                  max = 60,
                  value = 0.5,
                  step=0.1),
      sliderInput("a",
                  "Parameter	a",
                  min = 0,
                  max = 60,
                  value = 0,
                  step=0.1),
      
      checkboxInput("f",
                    "Wahrscheinlichkeitsdichte f",
                    value = TRUE
      ),
      checkboxInput("F",
                    "Verteilungsfunktion F",
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
server <- function(input, output,session) {
  
  output$distPlot <- renderPlot({
    
    observe({
      if (input$a >= input$b) {
        updateSliderInput(session, "a", value = input$b - 1)
      }
    })
    
    xt <- 0
    
    a<- input$a
    b<- input$b
    # adapt our  x axis for different values of Lambda
    
    if (b<=5)
    {
      xt <- seq(-1,7, by = 0.001)
      
    }else if(5< b & b<=60)
    {
      xt <- seq(0,80, by = 0.001)
      
    }else
    {
      xt <- seq(-0.5,2, by =0.0001)
    }
   
  
    f1<- dunif(xt,a,b)   
   
    F1<-punif(xt,a,b)
    
    
    if(input$f & !input$F)
    {
      
      plot(xt, f1, type = "p",xlab="t,x",ylab = "f(t),F(x)", lwd=2, col = "blue",pch= 16,ylim=c(0,max(f1,F1)), cex=0.5, main="W-Dichte f und Verteilungsfunktion F einer gleichverteilten ZV")
      legend("topleft", legend = c("f", "F"),cex = 0.7, col = c( "blue","darkgreen"),title.adj =0, lty = 1, lwd=2, box.lty = 1)
    }else if(!input$f & input$F)
    {
      
      plot(xt, F1, type = "p",xlab="t,x",ylab = "f(t),F(x)", lwd=2, col = "darkgreen",pch= 16, ylim=c(0,max(f1,F1)), cex=0.5, main="W-Dichte f und Verteilungsfunktion F einer gleichverteilten ZV" )
      legend("topleft", legend = c("f", "F"),cex = 0.7, col = c( "blue","darkgreen"),title.adj =0, lty = 1, lwd=2, box.lty = 1)
    }else if((input$f) & (input$F))
    {
      plot(xt, F1, type = "p",xlab="t,x",ylab = "f(t),F(x)", lwd=2, col = "darkgreen",pch= 16, cex=0.5, ylim=c(0,max(f1,F1)), main="W-Dichte f und Verteilungsfunktion F einer gleichverteilten ZV")
      
      lines(xt, f1, col = "blue", type = "p", pch= 16, cex=0.5)
      
      # Legend
      legend("topleft", legend = c("f", "F"),cex = 0.7, col = c( "blue","darkgreen"),title.adj =0, lty = 1, lwd=2, box.lty = 1)
    }else
    {
      plot(NULL, xlab="t,x",ylab = "f(t),F(x)",col="red",lwd=3, main="W-Dichte f und Verteilungsfunktion F einer gleichverteilten ZV",ylim=c(0,1),xlim=c(-10,10)) 
      legend("topleft", legend = c("f", "F"),cex = 0.7, col = c( "blue","darkgreen"),title.adj =0, lty = 1, lwd=2, box.lty = 1)
    }
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



###################################################################


