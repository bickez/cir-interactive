require(shiny)
require(ggplot2)
source('cir.R')

shinyServer(function(input, output) {
    set.seed(666)
  output$singleRun <- renderPlot({
    N <- input$years * 252
    t <- (1:N)/252
    short.rates <- CIRSimulation(N, input$r0, input$kappa, input$theta, input$sigma)
    ggplot(data.frame(t, short.rates), aes(x=t, y=short.rates)) + geom_line() + ylab('Short Rate') + xlab('Year')
  })
  output$multipleRuns <- renderPlot({
    N <- input$years * 252
    PlotShorts(input$simulations, N, input$r0, input$kappa, input$theta, input$sigma)
  })
  
  output$yieldCurve <- renderPlot({
    yields <- CIRYieldCurve(input$r0, input$kappa, input$theta, input$sigma)
    t <- 1:10
    ggplot(data.frame(t, yields), aes(x=t, y=yields)) + geom_line() + ylab('Yield') + xlab('Maturity')
  })
  
})