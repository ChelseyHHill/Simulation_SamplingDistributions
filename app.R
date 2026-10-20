library(shiny)

# 1. UI Section (Paste content from your old ui.R here)
parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("Normal",
           numericInput("mean", "Mean", value = 0),
           numericInput("sd", "Standard Deviation", min = 0, value = 1)
  ),
  tabPanel("Uniform", 
           numericInput("min", "Minimum", value = 0),
           numericInput("max", "Maximum", value = 1)
  ),
  tabPanel("Poisson", 
           numericInput("lambda", "Lambda", value = 5),
  ),
  tabPanel("Exponential",
           numericInput("rate", "rate", value = 1, min = 0),
  )
)

ui <- fluidPage(titlePanel("Sampling Distributions"),
                tabPanel("Sampling Distributions",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("dist", "Distribution", 
                                         choices = c("Normal", "Uniform", "Exponential", "Poisson")),
                             sliderInput("n",
                                         "Sample Size (n)",
                                         min = 1,
                                         max = 50,
                                         value = 10,
                                         step = 5),
                             sliderInput("r",
                                         "Number of Samples",
                                         min = 1,
                                         max = 5000,
                                         value = 1,
                                         step = 50),
                             parameter_tabs
                           ),
                           mainPanel(tabsetPanel(type = "tabs",
                                                 tabPanel("Main", 
                                                          h3("Instructions"),
                                                          p("To use this sampling distribution app, first choose a distribution from the drop-down menu on the left. Next, input the accompanying values of the input parameter(s) that are required for that distribution. You can also keep the default values provided."),
                                                          br(),
                                                          p("Then, navigate to the following tabs:"),
                                                          tags$ol(
                                                            tags$li("Population, which will update based on your chosen distribution and distribution parameters"), 
                                                            tags$li("One Sample, which will plot one sample from your population, based on your chosen sample size"), 
                                                            tags$li("Sampling Distribution, which will plot the distribution of the sample means for your chosen number of samples")
                                                          ),
                                                          br(),
                                                          hr(),
                                                          
                                                 ),
                                                 tabPanel("Population", 
                                                          br(),
                                                          p("Directions: Use the drop-down menu to choose a distribution. Then, use the default parameter value(s) or change the value(s) to see the underlying population distribution."),
                                                          plotOutput("underlying"),
                                                          hr()),
                                                 tabPanel("One Sample", 
                                                          br(),
                                                          p("Directions: Use the slider to change the Sample Size (n) to see the impact on the distribution"),
                                                          plotOutput("hist"),
                                                          hr()),
                                                 tabPanel("Sampling Distribution", 
                                                          br(),
                                                          p("Directions: Use the slider to change the Sample Size (n) and Number of Samples to see the impact on the sampling distribution"),
                                                          plotOutput("sampling"),
                                                          hr())
                           )
                           )
                         )))
  
  # 2. Server Section (Paste content from your old server.R here)
  server <- function(input, output, session){
    
    observeEvent(input$dist, {
      updateTabsetPanel(inputId = "params", selected = input$dist)
    }) 
    
    
    sample <- reactive({
      set.seed(1)
      switch(input$dist,
             Normal = rnorm(input$n, input$mean, input$sd),
             Uniform = runif(input$n, input$min, input$max),
             Exponential = rexp(input$n, input$rate),
             Poisson = rpois(input$n, input$lambda),
             Binomial = rbinom(input$n, input$n, input$p)
      )
    })
    output$hist <- renderPlot({hist(sample(), xlab = "", ylab = "", 
                                    main = "",
                                    sub = paste0("Sample Mean = ", round(mean(sample()),2)))
      abline(v = mean(sample()), col = "hotpink")})
    popdist <- reactive({
      set.seed(1)
      switch(input$dist,
             Normal = rnorm(100000, input$mean, input$sd),
             Uniform = runif(100000, input$min, input$max),
             Exponential = rexp(100000, input$rate),
             Poisson = rpois(100000, input$lambda)
      )
    })
    output$underlying <- renderPlot({hist(popdist(), xlab = "", 
                                          ylab = "", 
                                          main = "Underlying Population Distribution",
                                          sub = paste0("Population Mean = ", round(mean(popdist()),2)))
      abline(v = mean(popdist()), col = "red")})
    
    samplingdist <- reactive({
      set.seed(1)
      switch(input$dist,
             Normal = apply(replicate(input$r, rnorm(input$n, input$mean, input$sd)), 2, mean),
             Uniform = apply(replicate(input$r, runif(input$n, input$min, input$max)), 2, mean),
             Exponential = apply(replicate(input$r, rexp(input$n, input$rate)), 2, mean),
             Poisson = apply(replicate(input$r, rpois(input$n, input$lambda)),2,mean)
      )
    })
    
    
    
    output$sampling <- renderPlot({hist(samplingdist(), xlab = "", 
                                        ylab = "", 
                                        main = "Sampling Distribution",
                                        sub = paste0("Sampling Distribution Mean = ", round(mean(samplingdist()),2)))
      abline(v = mean(samplingdist()), col = "purple", lty=2)
      abline(v = mean(popdist()), col = "red")})}
  
    
    # 3. App Launch (This binds the UI and Server together)
    shinyApp(ui = ui, server = server)