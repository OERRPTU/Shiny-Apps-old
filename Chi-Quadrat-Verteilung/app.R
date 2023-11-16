##################################
# Shiny App zur Chi^2 Verteilung #
##################################

library(shiny)
library(latex2exp)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("X^2-verteilung mit k Freiheitsgraden (FG)"),

    
    sidebarLayout(
        sidebarPanel(
            sliderInput("k",
                        "k (FG)",
                        min = 0,
                        max = 10,
                        value = 2,
                        step = 1
                        ),
            checkboxInput("Wdichte",
                          "W-dichte fn",
                          value = TRUE
            ),
            checkboxInput("Verteilungsfknt",
                          "Verteilungsfunktion Sn",
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
      xt <- seq(-2,20 , by = 0.001)
      
      f<- dchisq(xt, input$k)   
      S<-pchisq(xt,input$k)
      
      
      
     
      if (input$Wdichte & input$Verteilungsfknt)
      {
        plot(xt, f, type = "l",ylim = c(0, 1),xlab="t,x", ylab = "fn(t),Sn(x)", lwd = 1.5, col = "blue",  main="W-Dichte fn und VF Sn von chi-Quadrat-verteilten ZV mit n FG")
      lines(xt, S, col = "red", lty = 1, lwd = 1.5)
      
      legend("topleft", legend = c("fn", "Sn"),cex = 0.76, col = c( "blue","red"),
             title.adj =0, lty = 1, lwd = 2, box.lty = 1)
      }
      else if (input$Wdichte & !input$Verteilungsfknt)
      {
        plot(xt, f, type = "l",ylim = c(0, 1),xlab="t,x", ylab = "fn(t)", lwd = 1.5, col = "blue",  main="W-Dichte fn und VF Sn von chi-Quadrat-verteilten ZV mit n FG")
        
        legend("topleft", legend = c("fn", "Sn"),cex = 0.76, col = c( "blue","red"),
               title.adj =0, lty = 1, lwd = 2, box.lty = 1)
      }
      else if (!input$Wdichte & input$Verteilungsfknt)
      {
        plot(xt, S, type = "l",ylim = c(0, 1),xlab="t,x", ylab = "Tn(x)", lwd = 1.5, col = "red",  main="W-Dichte fn und VF Sn von chi-Quadrat-verteilten ZV mit n FG")
        
        legend("topleft", legend = c("fn", "Sn"),cex = 0.76, col = c( "blue","red"),
               title.adj =0, lty = 1, lwd = 2, box.lty = 1)
        }
      else
      {
        plot(NULL,xlim = c(0, 1),ylim = c(0, 1),xlab="t,x", ylab = "fn(t),Tn(x)", lwd = 1.5, col = "red",  main="W-Dichte fn und VF Sn von chi-Quadrat-verteilten ZV mit n FG")
        
        legend("topleft", legend = c("fn", "Sn"),cex = 0.76, col = c( "blue","red"),
               title.adj =0, lty = 1, lwd = 2, box.lty = 1)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
