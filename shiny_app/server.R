# server.R

library(shiny)
source("utils.R")

shinyServer(
    function(input, output) {
        output$text1 <- renderText({
            paste(filter_string(get_word(input$input_str)))
        })
        output$text2 <- renderText({
            paste(filter_string(get_pred(input$input_str)))
        })
    }
)