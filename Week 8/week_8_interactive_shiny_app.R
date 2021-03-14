### Shiny app for week 8 v1



library(shiny)
library(tidyverse)
library(broom)
library(purrr)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("SAT Scores T-Test Example"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      # create the "Collect Sample" button for sample 1
      actionButton(inputId = "run_sim_1", label = "Collect Sample 1"),
      
      
      # Inputs for sample one ----
      
      # Input: Slider for the sample size 
      sliderInput(inputId = "samp_size_1",
                  label = "Sample size #1",
                  min = 0,
                  max = 2500,
                  value = 500),
      
      
       # Input: Slider for SAT verbal score standard deviation 
       sliderInput(inputId = "verbal_sd_1",
                   label = "Verbal score sd #1",
                   min = 0,
                   max = 100,
                   value = 50),
       
       # Input: Slider for SAT verbal score mean 
       sliderInput(inputId = "verbal_mean_1",
                   label = "Verbal score mean #1",
                   min =200,
                   max = 800,
                   value = 600),
       
       # Input: Slider for SAT math score standard deviation 
       sliderInput(inputId = "math_sd_1",
                   label = "Math score sd #1",
                   min = 0,
                   max = 100,
                   value = 50),
       
       # Input: Slider for SAT math score mean 
       sliderInput(inputId = "math_mean_1",
                   label = "math score mean #1",
                   min = 200,
                   max = 800,
                   value = 600),
      
      
      
      
      # Inputs for sample two ----

      # create the "Collect Sample" button for sample 2
      actionButton(inputId = "run_sim_2", label = "Collect Sample 2"),
      
            
      # Input: Slider for the sample size 
      sliderInput(inputId = "samp_size_2",
                  label = "Sample size #2",
                  min = 0,
                  max = 2500,
                  value = 500),
      
      
      # Input: Slider for SAT verbal score standard deviation 
      sliderInput(inputId = "verbal_sd_2",
                  label = "Verbal score sd #2",
                  min = 0,
                  max = 100,
                  value = 50),
      
      # Input: Slider for SAT verbal score mean 
      sliderInput(inputId = "verbal_mean_2",
                  label = "Verbal score mean #2",
                  min =200,
                  max = 800,
                  value = 600),
      
      # Input: Slider for SAT math score standard deviation 
      sliderInput(inputId = "math_sd_2",
                  label = "Math score sd #2",
                  min = 0,
                  max = 100,
                  value = 50),
      
      # Input: Slider for SAT math score mean 
      sliderInput(inputId = "math_mean_2",
                  label = "math score mean #2",
                  min = 200,
                  max = 800,
                  value = 600),
      
      
      
      # Inputs for sample three ----
      
      # create the "Collect Sample" button for sample 3
      actionButton(inputId = "run_sim_3", label = "Collect Sample 3"),
      
      # Input: Slider for the sample size 
      sliderInput(inputId = "samp_size_3",
                  label = "Sample size #3",
                  min = 0,
                  max = 2500,
                  value = 500),
      
      
      # Input: Slider for SAT verbal score standard deviation 
      sliderInput(inputId = "verbal_sd_3",
                  label = "Verbal score sd #3",
                  min = 0,
                  max = 100,
                  value = 50),
      
      # Input: Slider for SAT verbal score mean 
      sliderInput(inputId = "verbal_mean_3",
                  label = "Verbal score mean #3",
                  min =200,
                  max = 800,
                  value = 600),
      
      # Input: Slider for SAT math score standard deviation 
      sliderInput(inputId = "math_sd_3",
                  label = "Math score sd #3",
                  min = 0,
                  max = 100,
                  value = 50),
      
      # Input: Slider for SAT math score mean 
      sliderInput(inputId = "math_mean_3",
                  label = "math score mean #3",
                  min = 200,
                  max = 800,
                  value = 600)
      
      
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----

            # Output for first sample
      plotOutput(outputId = "distPlot_1"),
      
      textOutput("t_stat_1"),
      
      textOutput("effect_size_1"),
      
      tableOutput("tidy_t_test_1"),
      
            # Output for second sample
      plotOutput(outputId = "distPlot_2"),
      
      textOutput("t_stat_2"),
      
      textOutput("effect_size_2"),
      
      tableOutput("tidy_t_test_2"),
      
      
      
      # Output for third sample
      plotOutput(outputId = "distPlot_3"),
      
      textOutput("t_stat_3"),
      
      textOutput("effect_size_3"),
      
      tableOutput("tidy_t_test_3"),
      
      verbatimTextOutput("tidy_t_test_3_output")
    )
  )
)





# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  
  # generate the three samples from input parameters 
  
  
  # first sample generation

  sample_size_1 <- eventReactive(input$run_sim_1, {input$samp_size_1})
  v_mean_1 <- eventReactive(input$run_sim_1, {input$verbal_mean_1})
  v_sd_1 <- eventReactive(input$run_sim_1, {input$verbal_sd_1})
  m_mean_1 <- eventReactive(input$run_sim_1, {input$math_mean_1})
  m_sd_1 <- eventReactive(input$run_sim_1, {input$math_sd_1})

  
    
  student_id_sim_1 <- eventReactive(input$run_sim_1, {seq(sample_size_1())})
  sat_verbal_sim_1 <- eventReactive(input$run_sim_1, 
                                    {round(rnorm(n = sample_size_1(), mean = v_mean_1(), sd = v_sd_1()), 0)})
  sat_math_sim_1 <- eventReactive(input$run_sim_1, 
                                  {round(rnorm(n = sample_size_1(), mean = m_mean_1(), sd = m_sd_1()), 0)})
  
  #cap the scores at 800
  sat_verbal_capped_1 <- eventReactive(input$run_sim_1, 
                                       {modify(.x = sat_verbal_sim_1(), .f = ~ min(800, .x))})
  sat_math_capped_1 <- eventReactive(input$run_sim_1, 
                                     {modify(.x = sat_math_sim_1(), .f = ~ min(800, .x))})
  
  
  sat_df_sim_1 <- eventReactive(input$run_sim_1, {tibble(id = student_id_sim_1(),
                                 verbal = sat_verbal_capped_1(),
                                 math = sat_math_capped_1())
    })
  
  
  # second sample generation
  
  
  sample_size_2 <- eventReactive(input$run_sim_2, {input$samp_size_2})
  v_mean_2 <- eventReactive(input$run_sim_2, {input$verbal_mean_2})
  v_sd_2 <- eventReactive(input$run_sim_2, {input$verbal_sd_2})
  m_mean_2 <- eventReactive(input$run_sim_2, {input$math_mean_2})
  m_sd_2 <- eventReactive(input$run_sim_2, {input$math_sd_2})
  
  
  
  student_id_sim_2 <- eventReactive(input$run_sim_2, {seq(sample_size_2())})
  sat_verbal_sim_2 <- eventReactive(input$run_sim_2, 
                                    {round(rnorm(n = sample_size_2(), mean = v_mean_2(), sd = v_sd_2()), 0)})
  sat_math_sim_2 <- eventReactive(input$run_sim_2, 
                                  {round(rnorm(n = sample_size_2(), mean = m_mean_2(), sd = m_sd_2()), 0)})
  
  #cap the scores at 800
  sat_verbal_capped_2 <- eventReactive(input$run_sim_2, 
                                       {modify(.x = sat_verbal_sim_2(), .f = ~ min(800, .x))})
  sat_math_capped_2 <- eventReactive(input$run_sim_2, 
                                     {modify(.x = sat_math_sim_2(), .f = ~ min(800, .x))})
  
  
  sat_df_sim_2 <- eventReactive(input$run_sim_2, {tibble(id = student_id_sim_2(),
                                                       verbal = sat_verbal_capped_2(),
                                                       math = sat_math_capped_2())
  })
  
  
  
  # third sample generation
  
  sample_size_3 <- eventReactive(input$run_sim_3, {input$samp_size_3})
  v_mean_3 <- eventReactive(input$run_sim_3, {input$verbal_mean_3})
  v_sd_3 <- eventReactive(input$run_sim_3, {input$verbal_sd_3})
  m_mean_3 <- eventReactive(input$run_sim_3, {input$math_mean_3})
  m_sd_3 <- eventReactive(input$run_sim_3, {input$math_sd_3})
  
  
  
  student_id_sim_3 <- eventReactive(input$run_sim_3, {seq(sample_size_3())})
  sat_verbal_sim_3 <- eventReactive(input$run_sim_3, 
                                    {round(rnorm(n = sample_size_3(), mean = v_mean_3(), sd = v_sd_3()), 0)})
  sat_math_sim_3 <- eventReactive(input$run_sim_3, 
                                  {round(rnorm(n = sample_size_3(), mean = m_mean_3(), sd = m_sd_3()), 0)})
  
  #cap the scores at 800
  sat_verbal_capped_3 <- eventReactive(input$run_sim_3, 
                                       {modify(.x = sat_verbal_sim_3(), .f = ~ min(800, .x))})
  sat_math_capped_3 <- eventReactive(input$run_sim_3, 
                                     {modify(.x = sat_math_sim_3(), .f = ~ min(800, .x))})
  
  
  sat_df_sim_3 <- eventReactive(input$run_sim_3, {tibble(id = student_id_sim_3(),
                                                       verbal = sat_verbal_capped_3(),
                                                       math = sat_math_capped_3())
  })
  
  
  
  # create the output for first sample
  output$distPlot_1 <- renderPlot({
    
    


    
    
    
    ggplot(data = sat_df_sim_1()) +
    geom_histogram(aes(x = math, fill = "red"), alpha = 0.2) +
    labs(title = "Simulated SAT Scores Histogram for Sample 1",
         x = "SAT Score",
         y = "Count") +
    geom_histogram(aes(x = verbal, fill = "blue"), alpha = 0.2) +
    scale_fill_manual(name = "Category", values = c("blue", "red"), labels = c("verbal", "math")) +
    theme(plot.title = element_text(hjust = 0.5))
  
    
  })
  
  t_test_1 <- reactive(t.test(sat_verbal_capped_1(), sat_math_capped_1(), paired = TRUE))  
  
  output$t_stat_1 <- renderText({
    paste0("The value of the t statistic is: t = ", round(t_test_1()$statistic[[1]], 3))
    })
    
  output$effect_size_1 <- renderText({
    paste0("The value of the effect size is: r = ", round(sqrt((t_test_1()$statistic[[1]])^2/(t_test_1()$statistic[[1]]^2 + t_test_1()$parameter[[1]])), 3))
    })
    
  output$tidy_t_test_1 <- renderTable({
      tidy(t_test_1())
    })
  
    
  
    # create the output for second sample
    
  output$distPlot_2 <- renderPlot({
  
    
    ggplot(data = sat_df_sim_2()) +
      geom_histogram(aes(x = math, fill = "red"), alpha = 0.2) +
      labs(title = "Simulated SAT Scores Histogram for Sample 2",
           x = "SAT Score",
           y = "Count") +
      geom_histogram(aes(x = verbal, fill = "blue"), alpha = 0.2) +
      scale_fill_manual(name = "Category", values = c("blue", "red"), labels = c("verbal", "math")) +
      theme(plot.title = element_text(hjust = 0.5))
    
      
    })
    
  t_test_2 <- reactive(t.test(sat_verbal_capped_2(), sat_math_capped_2(), paired = TRUE))  
    
  output$t_stat_2 <- renderText({
    paste0("The value of the t statistic is: t = ", round(t_test_2()$statistic[[1]], 3))
  })
  
  output$effect_size_2 <- renderText({
    paste0("The value of the effect size is: r = ", round(sqrt((t_test_2()$statistic[[1]])^2/(t_test_2()$statistic[[1]]^2 + t_test_2()$parameter[[1]])), 3))
  })
  
  output$tidy_t_test_2 <- renderTable({
    tidy(t_test_2())
  })
    
    
  
  
  # create output for third sample
  
  output$distPlot_3 <- renderPlot({
    
    
    
    
    
    
    
    ggplot(data = sat_df_sim_3()) +
      geom_histogram(aes(x = math, fill = "red"), alpha = 0.2) +
      labs(title = "Simulated SAT Scores Histogram for Sample 3",
           x = "SAT Score",
           y = "Count") +
      geom_histogram(aes(x = verbal, fill = "blue"), alpha = 0.2) +
      scale_fill_manual(name = "Category", values = c("blue", "red"), labels = c("verbal", "math")) +
      theme(plot.title = element_text(hjust = 0.5))
    
    
  })
  
  t_test_3 <- reactive(t.test(sat_verbal_capped_3(), sat_math_capped_3(), paired = TRUE))  
  
  output$t_stat_3 <- renderText({
    paste0("The value of the t statistic is: t = ", round(t_test_3()$statistic[[1]], 3))
  })
  
  output$effect_size_3 <- renderText({
    paste0("The value of the effect size is: r = ", round(sqrt((t_test_3()$statistic[[1]])^2/(t_test_3()$statistic[[1]]^2 + t_test_3()$parameter[[1]])), 3))
  })
  
  output$tidy_t_test_3 <- renderTable({
    tidy(t_test_3())
  })
  
  output$tidy_t_test_3_output <- renderPrint({
    t_test_3()
  })
    
      
  
}




shinyApp(ui = ui, server = server)

