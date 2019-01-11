# ui.R

suppressPackageStartupMessages(c(
 
   library(shinythemes),
  library(shiny),
  library(tm),
  library(stringr),
  library(markdown),
  library(stylo)))

shinyUI(navbarPage(h4("Word Predictor"),
                   theme = shinytheme("superhero"),
                   tabPanel("Aplication",
                            fluidRow(
                              column(3),
                              column(6,
                                     tags$div(textInput("input_str", 
                                                        label = h4("enter your text followed by space"),
                                                        value = ),
                                              tags$span(style="color:grey",("we replace profanity language")),
                                              br(),
                                              tags$hr(),
                                              h4("predicted next word:"),
                                              tags$span(style="color:yellow",
                                                        tags$strong(tags$h3(textOutput("text2")))),
                                              br(),
                                              align="left")
                              ),
                              column(3)
                            )
                   ),
                   tabPanel("About",
                            fluidRow(
                              column(2,
                                     p("")),
                              column(8,
                                     includeMarkdown("./project_description.md")),
                              column(2,
                                     p(""))
                            )
                   ),
                   tags$hr(),
                   tags$br(),
                   tags$span(style="color:grey", 
                             tags$footer(("Entre Ríos - Argentina - 2019"), 
                                         tags$a(
                                           href="www.linkedin.com/in/castillocs",
                                           target="_blank",
                                           "castillocs")))
                   )
)
