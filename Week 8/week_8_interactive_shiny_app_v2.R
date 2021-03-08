### Shiny app for weeek 8 v2



library(shiny)
library(tidyverse)
library(broom)
library(purrr)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("SAT Scores Paired Samples T-Test Example"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      textOutput(outputId = "directions"),
      
      # create the "Create Population distribution" button for the simulation
      actionButton(inputId = "gen_pop", label = "Generate Population"),
      
      
      # Population level inputs ----
      
      sliderInput(inputId = "pop_size",
                  label = "Population",
                  min = 100,
                  max = 15000,
                  value = 5000),
      
      
      # Input: Slider for SAT verbal score standard deviation 
      sliderInput(inputId = "pop_verbal_sd",
                  label = "Population verbal score sd",
                  min = 0,
                  max = 100,
                  value = 50),
      
      # Input: Slider for SAT verbal score mean 
      sliderInput(inputId = "pop_verbal_mean",
                  label = "Population verbal score mean",
                  min =200,
                  max = 800,
                  value = 600),
      
      # Input: Slider for SAT math score standard deviation 
      sliderInput(inputId = "pop_math_sd",
                  label = "Population math score sd",
                  min = 0,
                  max = 100,
                  value = 50),
      
      # Input: Slider for SAT math score mean 
      sliderInput(inputId = "pop_math_mean",
                  label = "Population math score mean",
                  min = 200,
                  max = 800,
                  value = 600),
      
      
      
      
      
      # Inputs for sample one ----
      
      
      # Input: Slider for the sample size 
      sliderInput(inputId = "samp_size_1",
                  label = "Sample size #1",
                  min = 0,
                  max = 2500,
                  value = 500),
      
      
      
      
      # Inputs for sample two ----
      
      
      # Input: Slider for the sample size 
      sliderInput(inputId = "samp_size_2",
                  label = "Sample size #2",
                  min = 0,
                  max = 2500,
                  value = 500),
      
      
      
      
      # Inputs for sample three ----
      

      # Input: Slider for the sample size 
      sliderInput(inputId = "samp_size_3",
                  label = "Sample size #3",
                  min = 0,
                  max = 2500,
                  value = 500),

      
      
      
      # create the "Collect Sample" button for sample 2
      actionButton(inputId = "collect_samples", label = "Collect Three Samples"),
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Population Histogram ----
      
      plotOutput(outputId = "pop_dist_plot"),
      
      
      
      # Output for first sample
      
      plotOutput(outputId = "samp_dist_plot_1"),

      textOutput("desc_1_m"),
      
      textOutput("desc_1_v"),
            
      textOutput("t_stat_1"),
      
      textOutput("effect_size_1"),
      
      tableOutput("tidy_t_test_1"),
      
      verbatimTextOutput("t_test_1_output"),
      
      
      
      # Output for second sample
      plotOutput(outputId = "samp_dist_plot_2"),
      
      textOutput("desc_2_m"),

      textOutput("desc_2_v"),
            
      textOutput("t_stat_2"),
      
      textOutput("effect_size_2"),
      
      tableOutput("tidy_t_test_2"),
      
      verbatimTextOutput("t_test_2_output"),
      
      
      # Output for third sample
      plotOutput(outputId = "samp_dist_plot_3"),
      
      textOutput("desc_3_m"),
      
      textOutput("desc_3_v"),
      
      textOutput("t_stat_3"),
      
      textOutput("effect_size_3"),
      
      tableOutput("tidy_t_test_3"),
      
      verbatimTextOutput("t_test_3_output")
    )
  )
)





### Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$directions <- renderText({
    "Change the population parameters and click Generate Population. Then change sample sizes and click Collect Three Samples"
  })
  
  
  # generate the population distribution to draw samples from
  
  pop_size <- eventReactive(input$gen_pop, {input$pop_size})
  pop_v_mean <- eventReactive(input$gen_pop, {input$pop_verbal_mean})
  pop_v_sd <- eventReactive(input$gen_pop, {input$pop_verbal_sd})
  pop_m_mean <- eventReactive(input$gen_pop, {input$pop_math_mean})
  pop_m_sd <- eventReactive(input$gen_pop, {input$pop_math_sd})
  
  
  student_id_pop <- eventReactive(input$gen_pop, 
                                  {seq(pop_size())})
  
  sat_verbal_pop <- eventReactive(input$gen_pop, 
                                    {round(rnorm(n = pop_size(), mean = pop_v_mean(), sd = pop_v_sd()), 0)})
  
  sat_math_pop <- eventReactive(input$gen_pop, 
                                  {round(rnorm(n = pop_size(), mean = pop_m_mean(), sd = pop_m_sd()), 0)})
  
  #cap the scores at 800
  sat_verbal_capped_pop <- eventReactive(input$gen_pop, 
                                       {modify(.x = sat_verbal_pop(), .f = ~ min(800, .x))})
  sat_math_capped_pop <- eventReactive(input$gen_pop, 
                                     {modify(.x = sat_math_pop(), .f = ~ min(800, .x))})
  
  
  sat_pop_df <- eventReactive(input$gen_pop, 
                              {tibble(id = student_id_pop(),
                                      verbal = sat_verbal_capped_pop(),
                                      math = sat_math_capped_pop())
  })
  
  
  
  
  # generate the three samples from input parameters ----
  
  
  # Step 1: store three sample sizes
  samp_size_1 <- eventReactive(input$collect_samples, {input$samp_size_1})
  
  samp_size_2 <- eventReactive(input$collect_samples, {input$samp_size_2})
  
  samp_size_3 <- eventReactive(input$collect_samples, {input$samp_size_3})
  
  
  # Step 2: create indices for random sample of population for each of the three samples

  samp_1_idx <- eventReactive(input$collect_samples, 
                              {sample(pop_size(), size = samp_size_1(), replace = FALSE)})
  
  samp_2_idx <- eventReactive(input$collect_samples, 
                              {sample(pop_size(), size = samp_size_2(), replace = FALSE)})
  
  samp_3_idx <- eventReactive(input$collect_samples, 
                              {sample(pop_size(), size = samp_size_3(), replace = FALSE)})
  
  
  
  # Step 3: make df for each of the three samples by subsetting part of the population df according to the randomly generated indices
  
  samp_1_df <- eventReactive(input$collect_samples,
                             {sat_pop_df()[samp_1_idx(), ]})
  
  samp_2_df <- eventReactive(input$collect_samples,
                             {sat_pop_df()[samp_2_idx(), ]})
  
  samp_3_df <- eventReactive(input$collect_samples,
                             {sat_pop_df()[samp_3_idx(), ]})  
  
  
  
  
  # create the output for the population ----
  
  
  output$pop_dist_plot <- renderPlot({
    
    
    
    ggplot(data = sat_pop_df()) +
      geom_histogram(aes(x = math, fill = "red"), alpha = 0.2) +
      labs(title = "Simulated SAT Scores Histogram for Population",
           x = "SAT Score",
           y = "Count") +
      geom_histogram(aes(x = verbal, fill = "blue"), alpha = 0.2) +
      scale_fill_manual(name = "Category", values = c("blue", "red"), labels = c("verbal", "math")) +
      theme(plot.title = element_text(hjust = 0.5))
    
    
  })
  
  
  
  
  
  # create the output for first sample ----
  
  
  output$samp_dist_plot_1 <- renderPlot({
    
    
    
    ggplot(data = samp_1_df()) +
      geom_histogram(aes(x = math, fill = "red"), alpha = 0.2) +
      labs(title = "Simulated SAT Scores Histogram for Sample 1",
           x = "SAT Score",
           y = "Count") +
      geom_histogram(aes(x = verbal, fill = "blue"), alpha = 0.2) +
      scale_fill_manual(name = "Category", values = c("blue", "red"), labels = c("verbal", "math")) +
      theme(plot.title = element_text(hjust = 0.5))
    
    
  })
  
  output$desc_1_m <- renderText({
    paste0("The mean of sample 1 math is: ", round(mean(samp_1_df()$math), 2), " and the sd of sample 1 math is: ", round(sd(samp_1_df()$math), 2), ".")
  })
  
  output$desc_1_v <- renderText({
    paste0("The mean of sample 1 verbal is: ", round(mean(samp_1_df()$verbal), 2), " and the sd of sample 1 verbal is: ", round(sd(samp_1_df()$verbal), 2),  ".")
  })
  
  t_test_1 <- reactive(t.test(samp_1_df()$verbal, samp_1_df()$math, paired = TRUE))  
  
  output$t_stat_1 <- renderText({
    paste0("The value of the t statistic is: t = ", round(t_test_1()$statistic[[1]], 3))
  })
  
  output$effect_size_1 <- renderText({
    paste0("The value of the effect size is: r = ", round(sqrt((t_test_1()$statistic[[1]])^2/(t_test_1()$statistic[[1]]^2 + t_test_1()$parameter[[1]])), 3))
  })
  
  output$tidy_t_test_1 <- renderTable({
    tidy(t_test_1())
  })
  
  output$t_test_1_output <- renderPrint({
    t_test_1()
  })
  
  
  
  # create the output for second sample
  
  output$samp_dist_plot_2 <- renderPlot({
    
    
    ggplot(data = samp_2_df()) +
      geom_histogram(aes(x = math, fill = "red"), alpha = 0.2) +
      labs(title = "Simulated SAT Scores Histogram for Sample 2",
           x = "SAT Score",
           y = "Count") +
      geom_histogram(aes(x = verbal, fill = "blue"), alpha = 0.2) +
      scale_fill_manual(name = "Category", values = c("blue", "red"), labels = c("verbal", "math")) +
      theme(plot.title = element_text(hjust = 0.5))
    
    
  })
  
  output$desc_2_m <- renderText({
    paste0("The mean of sample 2 math is: ", round(mean(samp_2_df()$math), 2), " and the sd of sample 2 math is: ", round(sd(samp_2_df()$math), 2), ".")
  })
  
  output$desc_2_v <- renderText({
    paste0("The mean of sample 2 verbal is: ", round(mean(samp_2_df()$verbal), 2), " and the sd of sample 2 verbal is: ", round(sd(samp_2_df()$verbal), 2), ".")
  })
  
  t_test_2 <- reactive(t.test(samp_2_df()$verbal, samp_2_df()$math, paired = TRUE))  
  
  output$t_stat_2 <- renderText({
    paste0("The value of the t statistic is: t = ", round(t_test_2()$statistic[[1]], 3))
  })
  
  output$effect_size_2 <- renderText({
    paste0("The value of the effect size is: r = ", round(sqrt((t_test_2()$statistic[[1]])^2/(t_test_2()$statistic[[1]]^2 + t_test_2()$parameter[[1]])), 3))
  })
  
  output$tidy_t_test_2 <- renderTable({
    tidy(t_test_2())
  })
  
  output$t_test_2_output <- renderPrint({
    t_test_2()
  })
  
  
  
  
  # create output for third sample
  
  output$samp_dist_plot_3 <- renderPlot({
    
    
    
    ggplot(data = samp_3_df()) +
      geom_histogram(aes(x = math, fill = "red"), alpha = 0.2) +
      labs(title = "Simulated SAT Scores Histogram for Sample 3",
           x = "SAT Score",
           y = "Count") +
      geom_histogram(aes(x = verbal, fill = "blue"), alpha = 0.2) +
      scale_fill_manual(name = "Category", values = c("blue", "red"), labels = c("verbal", "math")) +
      theme(plot.title = element_text(hjust = 0.5))
    
    
  })
  
  output$desc_3_m <- renderText({
    paste0("The mean of sample 3 math is: ", round(mean(samp_3_df()$math), 2), " and the sd of sample 3 math is: ", round(sd(samp_3_df()$math), 2), ".")
      })
  
  output$desc_3_v <- renderText({
    paste0("The mean of sample 3 verbal is: ", round(mean(samp_3_df()$verbal), 2), " and the sd of sample 3 verbal is: ", round(sd(samp_3_df()$verbal), 2), ".")
  })
  
  t_test_3 <- reactive(t.test(samp_3_df()$verbal, samp_3_df()$math, paired = TRUE))  
  
  output$t_stat_3 <- renderText({
    paste0("The value of the t statistic is: t = ", round(t_test_3()$statistic[[1]], 3))
  })
  
  output$effect_size_3 <- renderText({
    paste0("The value of the effect size is: r = ", round(sqrt((t_test_3()$statistic[[1]])^2/(t_test_3()$statistic[[1]]^2 + t_test_3()$parameter[[1]])), 3))
  })
  
  output$tidy_t_test_3 <- renderTable({
    tidy(t_test_3())
  })
  
  output$t_test_3_output <- renderPrint({
    t_test_3()
  })
  
  
  
}




shinyApp(ui = ui, server = server)

