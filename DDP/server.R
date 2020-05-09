library(shiny);  library(ggplot2)

shinyServer(function(input, output) {
    mu1 <- reactive({ input$mu0 + input$effect.size })
    two.sided <- reactive({
        if(input$one.two == TRUE){ 2 } else{ 1 }
    })
    
    za <- reactive({ qnorm(input$a/two.sided(), lower.tail=FALSE) })
    zb <- reactive({ qnorm(input$b, lower.tail=FALSE) })
    
    q12 <- reactive({
        if(input$n1.n0 >= 1){ 1 + 1/input$n1.n0 } else{ 1 + input$n1.n0 }
    })
    
    est.sample.size <- reactive({
        ( (za() + zb())^2 * input$std.dev0^2 * q12() ) / (input$effect.size^2)
    })

    n1.new <- reactive({
        if(is.na(input$n1)){ est.sample.size() } else{ input$n1 }
    })
    n0 <- reactive({ n1.new()/input$n1.n0 })
    
    std.err0 <- reactive({ input$std.dev0/sqrt(n0()/2) })
    std.err1 <- reactive({ input$std.dev1/sqrt(n1.new()/2) })
    
    output$dispPlot <- renderPlot({
        x <- seq(from = input$mu0-5*std.err0(), to=mu1()+5*std.err1(), by=0.01)
        h0 <- dnorm(x, mean=input$mu0, sd=std.err0())
        h1 <- dnorm(x, mean=mu1(), sd=std.err1())
        h.norm <- data.frame(x, h0, h1)
        
        a.h0 <- qnorm(input$a/two.sided(), mean=input$mu0, sd=std.err0(), lower.tail=FALSE)
        b.h1 <- qnorm(input$b, mean=mu1(), sd=std.err1())
        
        p <- ggplot(h.norm, aes(x=x, y=h0)) +
            geom_line(aes(x=x, y=h0), color="red") +
            geom_line(aes(x=x, y=h1), color="blue") +
            geom_vline(xintercept=a.h0) +
            geom_vline(xintercept=b.h1, linetype="dashed") +
            geom_ribbon(data=subset(h.norm, a.h0 <= x & x <= max(x)), 
                        aes(ymin=0, ymax=h0, fill="H0"), alpha=0.5, legend=FALSE) +
            geom_ribbon(data=subset(h.norm, min(x) <= x & x <= a.h0),
                        aes(ymin=0, ymax=h1, fill="H1"), alpha=0.5, legend=FALSE)
        return(p)
     })
    
    power.b <- reactive({
        pnorm( qnorm(input$a/two.sided(), mean=input$mu0, sd=std.err0(), lower.tail=FALSE),
               mean=mu1(), sd=std.err1(), lower.tail=FALSE)
    })
    
    output$print.sample.n <- renderText({ 
        paste("suggested sample size : ", round(est.sample.size(), 2), "\n")
    })
    
    output$print.power <- renderText({
        if(power.b() >= (1-input$b)){
            paste("Power : ", round(power.b(), 2), "-adequate\n")
        }else{
            paste("Power : ", round(power.b(), 2), "-inadequate\n")
        }
    })
})
