library(shiny)

ui <-  fluidPage(
  
  sliderInput(inputId = "r",
              label = "Choose the growth rate, r",
              min = 0,max = 4,value = 1,step = 0.01),
  numericInput(inputId = "IC",
               label = "Initial condition",
               value = 0.1,min = 0,max = 1,step = 0.1),
  
  plotOutput(outputId = "TimeSeries")
  
)

server <- function(input, output) {
  Iterations<-20
  
  y1<-reactive({
    x <- c(Iterations+1,1)
    x[1] <- input$IC
    for (i in 2:(Iterations+1)){
      x[i]<-input$r*x[i-1]*(1-x[i-1])
    }
    x
  })
  
  Time<-0:(Iterations);
  output$TimeSeries <- renderPlot({
    plot(Time,y1(),"b",xlab="Iterations",ylab="x",col="blue",ylim=c(0,1), lwd = 3)
  })
}


shinyApp(ui = ui, server = server)

