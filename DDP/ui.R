library(shiny)

shinyUI(fluidPage(
    titlePanel("Statistical power & Sample size calculation"),

    sidebarLayout(
        sidebarPanel(
            h3("Statistical parameters"),
            sliderInput("a", "alpha:", min=0.001, max=0.999, value=0.05),
            sliderInput("b", "beta:", min=0, max=100, value=20),
            
            h3("Distribution parameters"),
            numericInput("mu0", "Mean of H0: ", value=0),
            numericInput("std.dev0", "Stand dev of H0: ", value=1),
            numericInput("effect.size", "Effect size: ", value=2), 
            numericInput("std.dev1", "Stand dev of H1: ", value=1),
            checkboxInput("one.two", "2 sided test", value=TRUE)
        ),

        mainPanel(
            plotOutput("distPlot")
        )
    )
))


# statistical parameters
# - a, b
#
# distribution
# - study design
# - mu0/mu1, stddev0/stddev1
# - effect size

#numericInput("a", "alpha", 0, min=0, max=1, step=0.01),