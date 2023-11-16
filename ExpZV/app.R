

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
  titlePanel("Exponentialverteilte Zufallsvariable"),
  # \uxxxx codes für griechische Buchstaben: http://www.javascripter.net/faq/greekletters.htm
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda",
                  "Parameter	\u03BB",
                  min = 0,
                  max = 4,
                  value = 0.05,
                  step=0.01),
      
      checkboxInput("f",
                    "W-Dichte f",
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
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    
    
    xt <- seq(-47,174, by = 0.001)
    
    lambda<- input$lambda
    # adapt our  x axis for different values of Lambda
    
    if (lambda<=0.05)
      {
        xt <- seq(-47,174, by = 0.001)
      
      }else if(0.5<=lambda & lambda<=0.1)
    {
        xt <- seq(-4,15, by = 0.001)
      
    }else
      {
        xt <- seq(-0.5,2, by =0.0001)
      }
    f1<- dexp(xt,lambda)   
    F1<-pexp(xt,lambda)
   
    
    if(input$f & !input$F)
    {
      
      plot(xt, f1, type = "p",xlab="t,x",ylab = "f(t),T(x)", ylim=c(0,max(f1,F1)),lwd=2, col = "blue",pch= 16, cex=0.5, main="W-Dichte fn und VF einer exponentialverteilten ZV")
      legend("topleft", legend = c("f", "F"),cex = 0.7, col = c( "blue","darkgreen"),
             title.adj =0, lty = 1, lwd=2, box.lty = 1)
    }else if(!input$f & input$F)
    {
      
      plot(xt, F1, type = "p",xlab="t,x",ylab = "f(t),T(x)",ylim=c(0,max(f1,F1)), lwd=2, col = "darkgreen",pch= 16, cex=0.5, main="W-Dichte fn und VF einer exponentialverteilten ZV" )
      legend("topleft", legend = c("f", "F"),cex = 0.7, col = c( "blue","darkgreen"),
             title.adj =0, lty = 1, lwd=2, box.lty = 1)
    }else if((input$f) & (input$F))
    {
      plot(xt, F1, type = "p",xlab="t,x",ylab = "f(t),T(x)",ylim=c(0,max(f1,F1)), lwd=2, col = "darkgreen",pch= 16, cex=0.5, main="W-Dichte fn und VF einer exponentialverteilten ZV")
      
      lines(xt, f1, col = "blue", type = "p", pch= 16, cex=0.5)
      
      # Legend
      legend("topleft", legend = c("f", "F"),cex = 0.7, col = c( "blue","darkgreen"),
             title.adj =0, lty = 1, lwd=2, box.lty = 1)
    }else
    {
      plot(NULL, xlab="t,x",ylab = "f(t),T(x)",col="red",lwd=3, main = "W-Dichte fn und VF einer exponentialverteilten ZV",ylim=c(0,1),xlim=c(-10,10)) 
    }
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

