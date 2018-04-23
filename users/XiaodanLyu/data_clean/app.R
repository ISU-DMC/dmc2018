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
ax <- list(
  showline = TRUE,
  mirror = "ticks",
  zeroline = FALSE,
  type = "log",
  tickfont = list(size = 15)
)
test_Jan <- read.table("test_Jan.txt", sep = "|", header  = T)
test_Jan$soldOutDate <- ymd(test_Jan$soldOutDate)
pred.guess <- ymd("2018-01-16")
err.guess <- (test_Jan$soldOutDate - pred.guess) %>% abs %>% sum %>% as.numeric %>% sqrt
train_Jan <- read.table("train_Jan.txt", sep = "|", header = T)
train_Jan %>% group_by(pid, size, stock) %>% 
  summarise(daysold = sum(units>0, na.rm = T)) %>% ungroup -> daysold_key


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
        tabPanel("Err_Heatmap", plotlyOutput("heatmap", height = "600px"), hr()),
        tabPanel("Err_Scatter", br(),
                 p("err.sum, err.med, err.mean: sum, median, mean of absolute error grouping by
                   stock and daysold (number of days items being sold)."),
                 radioButtons("color_var", "Color by Err.",
                              choices = c("sum" = "err.sum",
                                          "median" = "err.med",
                                          "mean" = "err.mean"),
                              selected = "err.sum", inline = TRUE),
                 plotlyOutput("scatter", height = "600px"),
                 hr()),
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
    
    preds_Jan <- read.table(input$file$datapath, header = TRUE, sep = "|")
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
  
  
  output$heatmap <- renderPlotly({
    
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
  
  output$scatter <- renderPlotly({
    
    comparison <- comparison()
    comparison_err <- comparison %>% left_join(daysold_key, by = c("pid", "size")) %>%
      mutate(err = abs(soldOutDate.true - soldOutDate.pred) %>% as.numeric)
    comparison_err_aggregate <- comparison_err %>% group_by(daysold, stock) %>%
      summarise(err.sum = sum(err), n = n(), err.med = median(err), err.mean = mean(err))
   
    p <- ifelse(input$color_var == "err.sum", 1/3, 1)
    color_title <- ifelse(input$color_var == "err.sum", "err.sum^(1/3)", input$color_var)
    color_pal <- leaflet::colorFactor("Reds", domain = 0:30)
    
    plot_ly(x = ~stock, y = ~daysold, data = comparison_err_aggregate,
            text = ~sprintf("n = %.0f, err.sum: %.0f, err.med: %.0f, err.mean: %.0f", 
                            n, err.sum, err.med, err.mean),
            marker = list(colorbar = list(title = color_title)),
            # color = ~err.sum^(1/3),
            color = ~get(input$color_var)^p,
            colors = color_pal(0:30),
            size = ~n^(1/3),
            type = "scatter", mode = "markers"
    ) %>% layout(xaxis = ax, yaxis = ax)
    
  })
  
  
  output$tb_board <- renderDataTable({
    board <- read.csv("leaderboard - subset_train_Jan.csv")
    # library(googlesheets)
    # get.board <- gs_title("leaderboard")
    # board <- get.board %>%
    #   gs_read(ws = "board")
    datatable(board %>% arrange(desc(Score)), class = "compact display nowrap",
              options = list(pageLength = 25)) %>%
      # formatStyle("Model", target = "row", backgroundColor = styleEqual("Guess", "yellow")) %>%
      formatStyle('Score',  color = 'red', backgroundColor = 'orange', fontWeight = 'bold')
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

