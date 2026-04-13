packages <- c("shiny", "arules", "arulesViz")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

library(shiny)
library(arules)
library(arulesViz)

data("Groceries")

ui <- fluidPage(
  titlePanel("Market Basket Analysis - Groceries Dataset"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Apriori Parameters"),
      sliderInput(
        inputId = "support",
        label   = "Minimum Support",
        min     = 0.001,
        max     = 0.1,
        value   = 0.01,
        step    = 0.001
      ),
      sliderInput(
        inputId = "confidence",
        label   = "Minimum Confidence",
        min     = 0.1,
        max     = 1.0,
        value   = 0.5,
        step    = 0.05
      ),
      hr(),
      uiOutput("ruleCount")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Item Frequency",
          br(),
          plotOutput("freqPlot", height = "500px")
        ),
        tabPanel(
          "Scatter Plot",
          br(),
          plotOutput("scatterPlot", height = "500px")
        ),
        tabPanel(
          "Network Graph",
          br(),
          plotOutput("graphPlot", height = "500px")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  rules <- reactive({
    apriori(
      Groceries,
      parameter = list(
        support    = input$support,
        confidence = input$confidence,
        minlen     = 2
      ),
      control = list(verbose = FALSE)
    )
  })

  output$ruleCount <- renderUI({
    r <- rules()
    tags$p(
      style = "color: #555; font-size: 13px;",
      paste0("Rules mined: ", length(r))
    )
  })

  output$freqPlot <- renderPlot({
    itemFrequencyPlot(
      Groceries,
      topN      = 10,
      type      = "absolute",
      col       = "steelblue",
      xlab      = "Items",
      ylab      = "Frequency (Count)",
      main      = "Top 10 Most Frequent Items"
    )
  })

  output$scatterPlot <- renderPlot({
    r <- rules()
    validate(
      need(length(r) > 0,
           "No rules found. Try lowering Support or Confidence thresholds.")
    )
    plot(
      r,
      method  = "scatterplot",
      measure = c("support", "confidence"),
      shading = "lift",
      engine  = "default",
      main    = "Association Rules: Support vs Confidence (shaded by Lift)"
    )
  })

  output$graphPlot <- renderPlot({
    r <- rules()
    validate(
      need(length(r) > 0,
           "No rules found. Try lowering Support or Confidence thresholds.")
    )
    top_rules <- head(sort(r, by = "lift"), 20)
    plot(
      top_rules,
      method = "graph",
      engine = "igraph",
      main   = "Top 20 Rules by Lift — Item Association Network"
    )
  })
}

shinyApp(ui, server)
