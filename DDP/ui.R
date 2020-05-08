library(shiny)

shinyUI(fluidPage(
    titlePanel("Sample size calculation - 2 dichotomous groups(t-test)"),

    sidebarLayout(
        sidebarPanel(
            h3("Statistical parameters"),
            sliderInput("a", "alpha:", min=0.001, max=0.999, value=0.05),
            sliderInput("b", "beta:", min=0, max=100, value=20),
            
            h3("Distribution parameters"),
            numericInput("mu0", "Mean of H0: ", value=0),
            numericInput("std.dev0", "Stand dev of H0: ", value=1),
            numericInput("std.dev1", "Stand dev of H1: ", value=1),
            numericInput("effect.size", "Effect size: ", value=2), 
            numericInput("n1", "sample number(H1): ", value=NULL), 
            numericInput("n1.n0", "ratio of number(H1/H0): ", value=1), 
            checkboxInput("one.two", "2 sided test", value=TRUE)
        ),

        mainPanel(
            textOutput("print.q12.1"),
            verbatimTextOutput("print.q12.3"),
        )
        
        
    )
))
