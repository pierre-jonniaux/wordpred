library(shiny)
library(shinyWidgets)

source("predict_word.R")


ui <- fluidPage(
  titlePanel("Word prediction"),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        helpText("Type the begining of you sentence. Please press enter when finished."),
        # search input = textInput waiting for enter keypress
        searchInput("query", label = "Enter your text"),
        checkboxInput("include_skip", label = "Use 2-words skip gram data", value = FALSE),
        sliderInput("pred_nbr", label = "Number of predictions", min = 1, max = 10, value = 3)
      ),
      mainPanel(
        strong(h2("Proposed words")),
        # verbatimTextOutput will print a verbatim (e.g. "\n" interpreted as line breaks)
        strong(verbatimTextOutput("words"))
        )
    )
  ),
  fluidRow(
    strong(h3(uiOutput("hyperlink_1")))
  )
)

server <- function(input, output, session) {
  # Hyperlink
  output$hyperlink_1 <- renderUI({
    # tag uses html tag. "a" is for link address
    tags$a(href="https://rpubs.com/Tapewormer/860809", "Online information")
  })
  
  # wait and gets the result from predict_word.R
  res <- reactive({
    # wait for input$query to change to react
    phrase = req(input$query)
    return(backoff_scoring(phrase, use_skipgram = input$include_skip))
  })
  
  # process the results if any and send them to UI
  output$words <- renderText({
    if (!is.null(res())){
      words <- res()[1:input$pred_nbr,][[1]]
      paste(words, collapse = "\n")
    }else{
      show_alert("Sorry", text = "No fitting proposal found. Please try again with another sentence", type = "warning")
    }
  })
  
}

shinyApp(ui, server)
