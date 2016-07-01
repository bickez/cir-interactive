require(shiny)
require(shinythemes)

shinyUI(fluidPage(theme=shinytheme('cosmo'),
 titlePanel('Interactive CIR Simulations'),
 br(),
 sidebarLayout(
   sidebarPanel(
     helpText('Inputs for CIR simulations.'),
     sliderInput('years', label='Years', min=1, max=10, value=5),
     sliderInput('simulations', label='Simulations', min=10, max=50, value=20),
     withMathJax(),
     numericInput('r0', label='Starting Value (\\(r_0\\))', min=0.01, max=0.1, step=0.001, value=0.02),
     numericInput('sigma', label='Volatility (\\(\\sigma\\))', min=0.001, max=0.1, step=0.001, value=0.01),
     numericInput('kappa', label='Long-Run Average (\\(\\kappa\\))', min=0.001, max=0.1, step=0.001, value=0.058),
     numericInput('theta', label='Mean-Reversion (\\(\\theta\\))', min=0.001, max=0.1, step=0.001, value=0.057)
   ),
   mainPanel(
     p('This is an interactive version of the CIR model, based on a previous blog post, which uses RStudio\'s Shiny.'),
     a('Link to original blog post.', href='https://puppyeconomics.wordpress.com/2015/06/01/cox-ingersoll-ross-model/'),
     h3('Single Run'),
     plotOutput('singleRun'),
     br(),
     h3('Multiple Runs'),
     plotOutput('multipleRuns'),
     br(),
     h3('Yield Curve'),
     plotOutput('yieldCurve')
   )
 )
 
))