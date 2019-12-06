
library(shiny)
install.packages(shinyalert)
install.packages(tidyverse)
library(shinyalert)

# Define UI ----
ui <- fluidPage(
  titlePanel("Fundraising Calculator"),
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      radioButtons("Party", "Political Party of Candidate", c("Republican"=1, 
                                     "Democrat"=2), selected = NULL),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput("LER", 
                h3("What percentage of the vote did the candidate of the same party as Candidate A recieve for this race in the last election?"), 
                value = 50.00),
    numericInput("CBF", 
                h3("How much money is candidate B expected to raise"), 
                value = 1),
    numericInput("LE", 
                h3("When was the last election?"), 
                value = 2012),
    numericInput("NPV", 
                h3("What percentage of the vote is candidate a's party expected to get in congressional elections this year"), 
                value = 50.00),
    actionButton("submit","submit")

),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("value")
      
    )
  )
)


# Define server logic ----
server <- function(input, output, session) {
  observeEvent(input$submit, {
    output$value<-renderPrint({
    LE<-input$LE
    Party<-input$Party
    LER<-input$LER
    NPV<-input$NPV
    CBF<-input$CBF
    NPVI<-(2*((NPV*.01)-.5))
    PRPVI<-ifelse(Party==1,(a-NPVI),NPVI-a)
    yp<-read.csv("https://raw.github.com/mb7068a/Final-Data-Project/master/Tables/ypv2.csv", sep="\t")
    a<-yp%>%dplyr::filter(year==LE)
    a<-a[-c(1,3,4)]
    paste(a)
    
    
     })
  })
}
# Run the app ----
shinyApp(ui = ui, server = server)
