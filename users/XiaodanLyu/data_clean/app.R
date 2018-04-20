#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(DT)
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

ui <- fluidPage(
  
  # Application title
  titlePanel("Prediction Diagnosis"),
  
  sidebarLayout(
    sidebarPanel(
      p("Make sure your predition file shares the same format and colnames as test_Jan.txt"),
      fileInput("file", "Upload file",
                multiple = TRUE,
                accept = "text"),
      p("err.guess is the error when all items are predicted to be sold-out on 2018-01-16."),
      br(),
      textOutput("Err"),
      br(),
      p("heatmap on the right hand side shows the decomposition of err.model")
    ),
    
    # Diagnosis Plot
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotlyOutput("Plot", height = "600px")),
        tabPanel("LeaderBoard", br(), dataTableOutput("tb_board"))
        # tabPanel("GoogleForms", 
        #          tags$iframe(id = "googleform",
        #                      src = "",
        #                      width = 400,
        #                      height = 600,
        #                      frameborder = 0,
        #                      marginheight = 0))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  comparison <- reactive({
    
    # input$file1 will be NULL initially.
    
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
    comment <- ifelse(err.model>=err.guess, "No better than random guess!", "Nice job!")
    cat(comment, fill = T)
    cat(sprintf("err.guess: %.3f, err.model: %.3f", err.guess, err.model), fill = T)
  })
  
  
  output$Plot <- renderPlotly({
    comparison <- comparison()
    tb2way <- comparison %>% 
      mutate(truth = day(soldOutDate.true), pred = day(soldOutDate.pred),
             abs_err = abs(as.numeric(soldOutDate.true-soldOutDate.pred))) %>%
      group_by(truth, pred) %>% summarise(freq = n(), abs_err = sum(abs_err)) %>% ungroup
    tb2way %>% filter(freq>0) %>%
      mutate(sqrt_abs_err = sqrt(abs_err)) %>%
      ggplot(aes(x = truth, y = pred, fill = sqrt_abs_err)) +
      geom_text(aes(label = abs_err), color = NA) +
      scale_y_discrete(limits = seq(1, 31, 1)) +
      scale_x_discrete(limits = seq(1, 31, 1)) +
      coord_equal() +
      geom_tile(color = "darkgrey") + geom_abline(slope = 1, linetype = 2) +
      scale_fill_gradient2(midpoint = 8, high = "red", low = "green", mid = "yellow") -> gg
    ggplotly(gg, tooltip = c("truth", "pred", "abs_err"))
    
  })
  
  
  output$tb_board <- renderDataTable({
    board <- read.csv("leaderboard.csv")
    # library(googlesheets)
    # get.board <- gs_title("leaderboard")
    # board <- get.board %>%
    #   gs_read(ws = "board")
    datatable(board %>% arrange(Error), class = "compact display nowrap",
              options = list(pageLength = 25)) %>%
      formatStyle("Model", target = "row", backgroundColor = styleEqual("Guess", "yellow")) %>%
      formatStyle('Error',  color = 'red', backgroundColor = 'orange', fontWeight = 'bold')
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

