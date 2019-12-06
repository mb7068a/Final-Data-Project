
library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Basic widgets"),
  
  fluidRow(
    
    column(3,
           radioButtons("Party", h3("What party is candidate A"),
                        choices = list("Republican" = 1, "Democrat" = 2),selected = 1)),
 
    column(3, 
           numericInput("LER", 
                        h3("What percentage of the vote did the Democrat recieve for this race in the last election?"), 
                        value = 1)),
  
  column(3, 
         numericInput("CBF", 
                      h3("How much money is candidate B expected to raise"), 
                      value = 1)),
  column(3,
         submitButton("Submit"))))
    
    

  

# Define server logic ----
server <- function(input, output) {
  output$Party <- renderText({ 
  paste()
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
