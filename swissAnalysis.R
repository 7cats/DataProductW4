#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
data(swiss)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("Analysis of swiss dataset"),
    
    sidebarLayout(
        sidebarPanel(
            
            selectInput("var1", 
                        label = "Choose a variable as x",
                        choices = colnames(swiss),
                        selected = colnames(swiss)[1]),
            
            selectInput("var2", 
                        label = "Choose another variable as y",
                        choices = colnames(swiss),
                        selected = colnames(swiss)[2]),
            
            
            checkboxInput("fitLine",
                          label = "Fit a linear regression",
                          value = FALSE)
        ),
        
        mainPanel(
            h2(textOutput('Plot of two chosen variables')),
            plotOutput("linePlot",
                       brush = brushOpts(id = "select_area")),
            h3(textOutput('fittedEq')),
            h3(textOutput('fittedEqSelect'))
        )
    )
)

server <- function(input, output) {
    plotVar <- reactive({
        x <- input$var1
        y <- input$var2
        DF <- data.frame(x = swiss[,x], y = swiss[,y])
        plotTitle <- paste("Relation between", input$var1, 'and', input$var2, sep = ' ')
        fitLine <- lm(data = DF, y ~ x)
        pred <- data.frame(x = DF$x, y = predict(fitLine, DF))
        DFSelect <- brushedPoints(DF, input$select_area)
        if (dim(DFSelect)[1] != 0) {
            fitLineSelect <- lm(data = DFSelect, y ~ x)
            predSelect <- data.frame(x = DF$x, y = predict(fitLineSelect, DF))
            plotVar <- list(x = x, y = y, DF = DF, plotTitle = plotTitle, 
                            fitLine = fitLine, pred = pred, DFSelect = DFSelect,
                            fitLineSelect = fitLineSelect, predSelect = predSelect)}
        else{
            plotVar <- list(x = x, y = y, DF = DF, plotTitle = plotTitle, 
                            fitLine = fitLine, pred = pred)}
        return(plotVar)
    })
    
    output$linePlot <- renderPlot({
        plotVar <- plotVar()
        x <- plotVar$var1
        y <- plotVar$var2
        DF <- plotVar$DF
        plotTitle <- plotVar$plotTitle
        fitLine <- plotVar$fitLine
        pred <- plotVar$pred
        
        p <- ggplot(DF, aes(x,y)) +
            geom_point( size = 3) +
            labs( x = x, y = y, title = plotTitle) +
            theme( plot.title = element_text(hjust = 0.5,
                                             face = "bold",
                                             size = 20),
                   axis.title = element_text(size = 15),
                   axis.text = element_text(size = 14))
        
        if(input$fitLine == TRUE)
            p <- p + geom_line(data = pred, aes(x,y), color = 'orange', lwd = 1.5)
        
        DFSelect <- brushedPoints(DF, input$select_area)
        if (dim(DFSelect)[1] != 0)
        {
            fitLineSelect <- plotVar$fitLineSelect
            predSelect <- plotVar$predSelect
            p <- p +
                geom_point(data = DFSelect, aes(x,y), size = 5, shape = 8) + 
                geom_line(data = predSelect, aes(x,y), color = 'lightblue', lwd = 1.5)
        }
        p
    })
    
    output$fittedEq <- renderText({
        plotVar <- plotVar()
        if(input$fitLine == TRUE)
            paste('R^2 of fitted linear regression line is ',
                  toString(round(summary(plotVar$fitLine)$r.squared,3)), sep = '')
        else
            return('Please check the box to fit a linear regression model')
    })
    
    output$fittedEqSelect <- renderText({
        plotVar <- plotVar()
        DFSelect <- brushedPoints(plotVar$DF, input$select_area)
        if (dim(DFSelect)[1] != 0)
            paste('R^2 of fitted linear regression line for selected data is ',
                  toString(round(summary(plotVar$fitLineSelect)$r.squared,3)), sep = '')
        else
            return('Please select an area from the plot as input')
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
