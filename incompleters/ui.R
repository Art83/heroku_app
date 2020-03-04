#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme="styles.css",
    h1("Incompleters plots"),
    sidebarLayout(
        sidebarPanel(
        radioButtons("typeInput", "Choose the variable",
                     choices = list("time for everyone", "time without outliers", 
                                    "key strokes total", "key strokes without outliers", 
                                    "key strokes L&R total", "key strokes L&R without outliers"))
        ),
        mainPanel(plotOutput("plot",width = "100%", height = "400px"),
                  br(),
                  br(),
                  tableOutput("tab"))
    )
    
))
