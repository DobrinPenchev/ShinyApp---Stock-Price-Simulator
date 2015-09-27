require(shiny)
require(TTR)
require(PerformanceAnalytics)
require(markdown)

ui <- fluidPage(
        
        # Application title
        h4(titlePanel("Stock Price Simulator")),
        
        # Sidebar with a slider input
        sidebarPanel(
                sliderInput(inputId = "obs",
                            label = "Number of observations:",
                            min = 500,
                            max = 1500,
                            step =1,
                            value = 500,
                            animate = animationOptions(interval = 300)),
                sliderInput(inputId = "m.m",
                            label = "Stock Average Daily Return Mean (bps):",
                            min = 0,
                            max = 20,
                            step = 1,
                            value = 0),
                sliderInput(inputId = "m.sd",
                            label = "Stock Average Daily Return Volatility (%):",
                            min = 0,
                            max = 10,
                            step = 1,
                            value = 1),
                sliderInput(inputId = "sd.sd",
                            label = "Variability of the Asset Daily Return Volatility (%):",
                            min = 0,
                            max = 10,
                            step = 1,
                            value = 3),
                sliderInput(inputId = "start.Price",
                            label = "Initial Asset Value ($):",
                            min = 10,
                            max = 100,
                            step = 5,
                            value = 50),
                numericInput("fastMA", "MACD Fast Moving Average", 12),
                numericInput("slowMA", "MACD Slow Moving Average", 26),
                numericInput("signal", "MACD Signal", 9),
                numericInput("histBars","Histogram Bars", 50)
        ),
        
        # Shows a plot of the generated output
        mainPanel(
                tabsetPanel(
                        tabPanel("Price Charts", plotOutput(outputId = "price", height = "900px")),
                        tabPanel("Distribution Plots", plotOutput(outputId = "distribution", height = "900px")),
                        tabPanel("Data Summary", tableOutput(outputId = "summaryTable")),
                        tabPanel("User Guide", includeHTML("SPS_UG.html"))
                )
        )
)

server <- function(input, output) {
        
        rnormA <- repeatable(rnorm)
        runifA <- repeatable(runif)
        rnormB <- repeatable(rnorm)
        rnormC <- repeatable(rnorm)
        rnormD <- repeatable(rnorm)
        rnormE <- repeatable(rnorm)
        rnormF <- repeatable(rnorm)
        
        n.obs <- reactive(c(input$obs))
        mm <- reactive(c(input$m.m))
        msd <- reactive(c(input$m.sd))
        sdsd <- reactive(c(input$sd.sd))
        startPrice <- reactive(c(input$start.Price))
        
        m <- reactive(rnormA(n.obs(),mm()/10000,msd()/100)) 
        sd <- reactive(runifA(n.obs(),0,sdsd()/100))
        e <- reactive(c(rnormB(1,0,sdsd()/100), rep(0,n.obs()-1)))

        A <- rnormC(1,0.272,0.01)
        B <- rnormD(1,0.099,0.001)

        y <- reactive({c(e()[1],sd()[2:n.obs()]*rnormE(n.obs()-1))})
        z <- reactive(c(sd()[1],sqrt(A*y()[1:(n.obs()-1)]^2 + B*sd()[1:(n.obs()-1)]^2)))
        
        x <- reactive({m() + rnormF(n.obs(),0,z())})

        prices <- reactive({c(startPrice(),cumprod(exp(x()))*startPrice())})
         
        mom <- reactive(MACD(prices(),input$fastMA,input$slowMA,input$signal))
        
        output$price <- renderPlot({

                par(mfcol=c(2,1))
                 
                plot(prices(),type="l",col="blue", main="Stock Price Evolution in Time",
                     xlab = "Time Elapsed (in business days)", ylab = "Stock Price")
                 
                plot(mom()[,"macd"],type="l",col="blue", main = "Moving Average Convergence Divergence Indicator",
                     xlab = "Time Elapsed (in business days)", ylab = "Indicator Value")
                lines(mom()[,"signal"],col="brown")
                abline(h=0)
        })
        
        output$distribution <- renderPlot({
                
                par(mfcol=c(3,1))
        
                plot(x(),type="l",col="red",main="Asset Log Returns", xlab = "Daily Observations",
                     ylab = "Returns magnitude",cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.2)
                abline(h=0)
        
                qqnorm(x(), col = "blue",cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.2)
                qqline(x(),  col = "brown")
        
                hist(x(),breaks=input$histBars,main="Asset Log Returns", xlab = "Returns Magnitude",
                     cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.2)
        })
        
        output$summaryTable <- renderTable(table.Stats(x()),digits = 4)
        
}

shinyApp(server = server, ui = ui)

