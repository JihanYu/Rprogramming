library(shiny);  library(ggplot2)

shinyServer(function(input, output) {
    q12 <- reactive({
        if(input$n1.n0 >= 1){
            1 + 1/input$n1.n0
        }else{
            1 + input$n1.n0
        }
    })
    
    est.sample.size <- reactive({
        za <- qnorm(input$a/two.sided, lower.tail=FALSE)
        zb <- qnorm(input$b, lower.tail=FALSE)
        ( (za+zb)^2 * input$std.dev0^2 * q12 ) / ( input$effect.size^2 )
    })

    n1.new <- reactive({
        if(is.na(input$n1)){
            est.sample.size
        }else{
            input$n1
        }
    })
        

    output$dispPlot <- renderPlot(({
        mu1 <- input$mu0 + input$effect.size
        two.sided <- 1;  if(input$one.two == TRUE) two.sided <- 2
        
        n0 <- n1.new/input$n1.n0
        
        std.err0 <- input$std.dev0 / sqrt(n0/2)
        std.err1 <- input$std.dev1 / sqrt(n1.new/2)
        
        x <- seq(from = input$mu0-5*std.erro0, to = mu1+5*std.err1, by=0.01)
        h0 <- dnorm(x, mean=input$mu0, sd=std.err0)
        h1 <- dnorm(x, mean=mu1, sd=std.err1)
        h.norm <- data.frame(x, h0, h1)
        
        a.h0 <- qnorm(input$a/two.sided, mean=input$mu0, sd=std.err0, lower.tail=FALSE)
        b.h1 <- qnorm(input$b, mean=mu1, sd=std.err1)
    }))
    

    #output$dispPlot <- renderPlot({
    #    ggplot(h.norm, aes(x=x, y=h0)) +
    #        geom_line(aes(x=x, y=h0), color="red") +
    #        geom_line(aes(x=x, y=h1), color="blue") +
    #        geom_vline(xintercept=a.h0) +
    #        geom_vline(xintercept=b.h1, linetype="dashed") +
    #        geom_ribbon(data=subset(h.norm, a.h0 <= x & x <= max(x)),
    #                    aes(ymin=0, ymax=h0, fill="H0", alpha=0.5)) +
    #        geom_ribbon(data=subset(h.norm, min(x) <= x & x <= a.h0),
    #                    aes(ymin=0, ymax=h1, fill="H1", alpha=0.5))
    #})
    output$sample.size <- renderText({
        cat("suggested sample size :", est.sample.size, "\n")
    })

})
