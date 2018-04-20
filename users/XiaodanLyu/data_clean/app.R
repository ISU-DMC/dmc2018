#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(tidyverse)

## diagnosis
theme_mine <- function(){
  theme_bw(base_size = 15)  +
    theme(axis.text.x = element_text(angle = 45))
}
test_Jan <- read.table("test_Jan.txt", sep = "|", header  = T)
test_Jan$soldOutDate <- ymd(test_Jan$soldOutDate)
pred.guess <- ymd("2018-01-16")
err.guess <- (test_Jan$soldOutDate - pred.guess) %>% abs %>% sum %>% as.numeric %>% sqrt

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Prediction Diagnosis"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        p("Make sure your predition file shares the same format and colnames as test_Jan.txt"),
        fileInput("file", "Upload file",
                  multiple = TRUE,
                  accept = "text"),
        p("err.guess is the error when all items are predicted to be sold-out on 2018-01-16."),
        textOutput("Err")
      ),
  
      # Diagnosis Plot
      mainPanel(
         plotOutput("Plot", height = "600px")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  comparison <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file)
    
    preds_Jan <- read.csv(input$file$datapath, header = TRUE, sep = "|")
    preds_Jan$soldOutDate <- ymd(preds_Jan$soldOutDate)
    comparison <- left_join(test_Jan, preds_Jan, by = c("pid", "size"), suffix = c(".true", ".pred"))
    
    comparison
    
    })
  
  output$Err <- renderPrint({
    comparison <- comparison()
    err.model <- (comparison$soldOutDate.true - comparison$soldOutDate.pred) %>% 
      abs %>% sum %>% as.numeric %>% sqrt
    comment <- ifelse(err.model>err.guess, "No better than random guess!", "Nice job!")
    cat(comment, fill = T)
    cat(sprintf("err.guess: %.3f, err.model: %.3f", err.guess, err.model), fill = T)
  })
    
   
   output$Plot <- renderPlot({
     comparison <- comparison()
     table(truth = day(comparison$soldOutDate.true), pred = day(comparison$soldOutDate.pred)) -> tb2way
     # tb2way %>% data.frame %>% glimpse
     tb2way %>% data.frame %>% filter(Freq>0) %>%
       ggplot(aes(x = truth, y = pred, fill = Freq)) +
       scale_y_discrete(limits = seq(1, 31, 1)) +
       coord_equal() +
       geom_tile(color = "grey") + geom_abline(slope = 1, linetype = 2) +
       scale_fill_distiller(palette = "RdYlBu") +
       theme_bw()
   }, res = 100)
}

# Run the application 
shinyApp(ui = ui, server = server)

