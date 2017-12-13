library(shiny)
library(plotrix)
#-----------------------------------------------------------------
set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31) )

#-----------------------------------------------------------------

ui <- fluidPage(
  
  
  titlePanel("Estimating pi with Monte Carlo simulation"),
  
  
  sidebarLayout(
    
    #
    sidebarPanel(
      
      
      sliderInput(inputId = "rad",
                  label = "Radius of the circle",
                  min = 0.1,
                  max = 10,
                  value = 1), 
      
                 
      sliderInput(inputId = "simtime",
                  label = "Number of simulations per unit circle",
                  min = 50,
                  max = 10000,
                  value = 100)
    ),
    
    
    mainPanel(
      
      
      plotOutput(outputId = "distPlot"), 
      verbatimTextOutput("summary")
      
    )
  )
)


server <- function(input, output) {
    
  
  output$distPlot <- renderPlot({
    #radius of the circle and other parameters
    r <- input$rad
    fromx <- -r
    tox <- r
    fromy <- fromx
    toy <- tox
    simtime <- input$simtime
    xr <- seq(fromx, tox, 1/simtime)
    yr <- runif(simtime*(tox-fromx)+1, fromy, toy)
    #data frame which calculates if a point is in the circle or out of it
    df <- data.frame(x = xr, y = yr, 
                     type = ifelse(test = (xr^2+yr^2) < r^2, 
                     yes = "inside", no = "outside"))
    #plot the output
    plot(df$x, df$y, col= df$type, asp = 1, xlab = "X", ylab = "Y")
    draw.circle(0,0,r)
    rect(fromx, fromy, tox, toy)
  })
  output$summary <- renderPrint({
    r <- input$rad
    fromx <- -r
    tox <- r
    fromy <- fromx
    toy <- tox
    simtime <- input$simtime
    xr <- seq(fromx, tox, 1/simtime)
    yr <- runif(simtime*(tox-fromx)+1, fromy, toy)
    
    df <- data.frame(x = xr, y = yr, 
                     type = ifelse(test = (xr^2+yr^2) < r^2, 
                                   yes = "inside", no = "outside"))
    count.table <- table(df$type)
    #calculate pi
    pi <- (4*count.table[1])/(count.table[1]+count.table[2])
    print(paste("Estimated value of pi: ", pi[[1]]))
  })
}


shinyApp(ui = ui, server = server)