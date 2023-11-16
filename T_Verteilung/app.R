#
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
  titlePanel("T-verteilte ZV mit k Freiheitsgraden (FG)"),
  
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("k",
                  "k (FG)",
                  min = 0,
                  max = 40,
                  value = 2,
                  step = 1
      ),
      checkboxInput("Wdichte",
                    "Wahrscheinlichkeitsdichte fn",
                    value = TRUE
      ),
      checkboxInput("Verteilungsfknt",
                    "Verteilungsfunktion Tn",
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
    xt <- seq(-10,10 , by = 0.001)
    
    f<- dt(xt, input$k)   
    T<-pt(xt,input$k)
   
    
    
    if (input$Wdichte & input$Verteilungsfknt)
    {
      plot(xt, f, type = "l",xlim=c(-10,10),ylim = c(0, 1),xlab="t,x", ylab = "fn(t),Tn(x)", lwd = 1.5, col = "blue",  main="W-Dichte fn und VF Tn von t-verteiltet ZV mit n FG")
      lines(xt, T, col = "red", lty = 1, lwd = 1.5)
      
      legend("topleft", legend = c("f", "T"),cex = 0.76, col = c( "blue","red"),
             title.adj =0, lty = 1, lwd = 2, box.lty = 1)
    }
    else if (input$Wdichte & !input$Verteilungsfknt)
    {
      plot(xt, f, type = "l",ylim = c(0, 1),xlim=c(-10,10),xlab="t,x", ylab = "fn(t)", lwd = 1.5, col = "blue",  main="W-Dichte fn und VF Tn von t-verteiltet ZV mit n FG")
      legend("topleft", legend = c("f", "T"),cex = 0.76, col = c( "blue","red"),
             title.adj =0, lty = 1, lwd = 2, box.lty = 1)
    }
    else if (!input$Wdichte & input$Verteilungsfknt)
    {
      plot(xt, T, type = "l",ylim = c(0, 1),xlab="t,x", xlim=c(-10,10),ylab = "Tn(x)", lwd = 1.5, col = "red",  main="W-Dichte fn und VF Tn von t-verteiltet ZV mit n FG")
      legend("topleft", legend = c("f", "T"),cex = 0.76, col = c( "blue","red"),
             title.adj =0, lty = 1, lwd = 2, box.lty = 1)
      }
    else
    {
      plot(NULL,xlim = c(0, 1),ylim = c(0, 1),xlab="t,x",xlim=c(-10,10), ylab = "Tn(x)", lwd = 1.5, col = "red",  main="W-Dichte fn und VF Tn von t-verteiltet ZV mit n FG")
      legend("topleft", legend = c("f", "T"),cex = 0.76, col = c( "blue","red"),
             title.adj =0, lty = 1, lwd = 2, box.lty = 1)
      }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
