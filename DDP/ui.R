library(shiny)
shinyUI(fluidPage(
    titlePanel("Sample size calculation"),

    sidebarLayout(
        sidebarPanel(
            numericInput("a", "alpha", 0, min=0, max=1, step=0.01),
            sliderInput("alpha", "alpha:", min=0.001, max=0.999, value=0.05),
            sliderInput("beta", "beta:", min=0, max=100, value=0.2),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        mainPanel(
            plotOutput("distPlot")
        )
    )
))
