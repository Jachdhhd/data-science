#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




library(shiny)
library(tidyverse)


setwd("C:/Program Files/R/R-4.0.3/projects")
titan <- read.csv("Titanic.csv", encoding = 'UTF-8')


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Titanic Passengers"),
    
    fluidRow(
        column(4, 
               selectInput("bar", "Barplot:",
               c("Survived", "Pclass", "Sex")))
    ),
    fluidRow(
        column(4, 
               selectInput("hist", "Histogram:",
               c("Age","Fare")))),
        

    
    sidebarLayout(
        sidebarPanel(
         selectInput('ucolor1', 'Color1:',
                    choices = c(`Red` = "red",
                                `Yellow` = "yellow",
                                `Skyblue` = "skyblue"))),
    
        sidebarPanel(
            selectInput('ucolor2', 'Color2:',
                            choices = c(`Green` = "green",
                                        `Yellow` = "yellow",
                                        `Skyblue` = "skyblue")))),




        mainPanel(splitLayout(cellWidths = c("50%", "50%"), 
        plotOutput("distPlot1"), plotOutput("distPlot2")))
)
         
   

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot1 <- renderPlot({
        
        x    <- titan[, 'Survived']
        y  <- titan[, 'Pclass']
        z  <- titan[, 'Sex']
        
        
        if (input$bar == "Survived") {
            barplot(table(x),  col = input$ucolor1, border = 'navy',
                 main = "Survived",
                 xlab = "Yes/No", ylab = "counts")
            
        }
        if (input$bar == "Pclass") {
            barplot(table(y), col = input$ucolor1, border = 'navy',
                    main = "Class",
                    xlab = "Class", ylab = "counts")
        }
        if (input$bar == "Sex") {
            barplot(table(z),  col = input$ucolor1, border = 'navy',
                    main = "Sex",
                    xlab = "Sex", ylab = "counts")
        }
        })
    
    output$distPlot2 <- renderPlot({
        
        x1 <- titan[, 'Age']
        y1 <- titan[, 'Fare']

        
        if (input$hist == "Age") {
        
        hist(x1,  col = input$ucolor2, border = 'navy',
             main = "Age",
             xlab = "Age", ylab = "counts")
        }
        if (input$hist == "Fare") {
        hist(y1,  col = input$ucolor2, border = 'navy',
             main = "Fare",
                 xlab = "Fare", ylab = "counts")}})
}

# Run the application 
shinyApp(ui = ui, server = server)
