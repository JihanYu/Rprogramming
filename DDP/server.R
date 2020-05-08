library(shiny);  library(ggplot2)

shinyServer(function(input, output) {
    q12 <- reactive ({ input$n1.n0 })
    #q12 <- reactive({
    #    if(input$n1.n0 >= 1){
    #        1 + 1/input$n1.n0
    #    }else{
    #        1 + input$n1.n0
    #    }
    #})
    
    output$print.q12.1 <- renderPrint({ input$n1.n0 })
    #output$print.q12.2 <- renderText({ print(q12) })
    output$print.q12.3 <- renderPrint({ input$n1.n0 })
    #output$print.q12.4 <- renderText({ q12 })

})
