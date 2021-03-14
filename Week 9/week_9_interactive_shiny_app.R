### Shiny app for weeek 9



library(shiny)
library(tidyverse)
library(broom)
library(purrr)
library(shiny)
library(multcomp)
library(car)
library(pwr)
library(sjstats)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Student Study Hours One-Way ANOVA Example"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel(title = "Population Opts",
                           
                           # create the "Create Population distribution" button for the simulation
                           actionButton(inputId = "gen_pop", label = "Generate Population"),
                           
                           textOutput(outputId = "directions_1"), 
                           
                           # Population level inputs ----
                           
                           # Input: Slider for architecture student population size
                           sliderInput(inputId = "pop_arc_size",
                                       label = "Archiecture Student Population",
                                       min = 100,
                                       max = 2500,
                                       value = 500),
                           
                           
                           # Input: Slider for architecture student population standard deviation 
                           sliderInput(inputId = "pop_arc_sd",
                                       label = "Architecture Population Weekly Study Hours SD",
                                       min = 0,
                                       max = 25,
                                       value = 7),
                           
                           # Input: Slider for architecture student population mean study hours 
                           sliderInput(inputId = "pop_arc_mean",
                                       label = "Architecture Population Weekly Study Hours Mean",
                                       min =0,
                                       max = 100,
                                       value = 20),
                           
                           
                           # Input: Slider for biology student population size
                           sliderInput(inputId = "pop_bio_size",
                                       label = "Biology Student Population",
                                       min = 100,
                                       max = 2500,
                                       value = 500),
                           
                           
                           # Input: Slider for biology student population standard deviation 
                           sliderInput(inputId = "pop_bio_sd",
                                       label = "Biology Population Weekly Study Hours SD",
                                       min = 0,
                                       max = 25,
                                       value = 7),
                           
                           # Input: Slider for biology student population mean study hours 
                           sliderInput(inputId = "pop_bio_mean",
                                       label = "Biology Population Weekly Study Hours Mean",
                                       min =0,
                                       max = 100,
                                       value = 20),
                           
                           
                           # Input: Slider for bme student population size
                           sliderInput(inputId = "pop_bme_size",
                                       label = "BME Student Population",
                                       min = 100,
                                       max = 2500,
                                       value = 500),
                           
                           
                           # Input: Slider for bme student population standard deviation 
                           sliderInput(inputId = "pop_bme_sd",
                                       label = "BME Population Weekly Study Hours SD",
                                       min = 0,
                                       max = 25,
                                       value = 7),
                           
                           # Input: Slider for bme student population mean study hours 
                           sliderInput(inputId = "pop_bme_mean",
                                       label = "BME Population Weekly Study Hours Mean",
                                       min =0,
                                       max = 100,
                                       value = 20),
                           
                           
                           
                           # Input: Slider for business student population size
                           sliderInput(inputId = "pop_bus_size",
                                       label = "Business Student Population",
                                       min = 100,
                                       max = 2500,
                                       value = 500),
                           
                           
                           # Input: Slider for business student population standard deviation 
                           sliderInput(inputId = "pop_bus_sd",
                                       label = "Business Population Weekly Study Hours SD",
                                       min = 0,
                                       max = 25,
                                       value = 7),
                           
                           # Input: Slider for business student population mean study hours 
                           sliderInput(inputId = "pop_bus_mean",
                                       label = "Business Population Weekly Study Hours Mean",
                                       min =0,
                                       max = 100,
                                       value = 20),
                           
                           
                           
                           # Input: Slider for chemE student population size
                           sliderInput(inputId = "pop_cheg_size",
                                       label = "ChemE Student Population",
                                       min = 100,
                                       max = 2500,
                                       value = 500),
                           
                           
                           # Input: Slider for chemE student population standard deviation 
                           sliderInput(inputId = "pop_cheg_sd",
                                       label = "ChemE Population Weekly Study Hours SD",
                                       min = 0,
                                       max = 25,
                                       value = 7),
                           
                           # Input: Slider for chemE student population mean study hours 
                           sliderInput(inputId = "pop_cheg_mean",
                                       label = "ChemE Population Weekly Study Hours Mean",
                                       min =0,
                                       max = 100,
                                       value = 20),
                           
                           
                           
                           # Input: Slider for chem student population size
                           sliderInput(inputId = "pop_chem_size",
                                       label = "Chemistry Student Population",
                                       min = 100,
                                       max = 2500,
                                       value = 500),
                           
                           
                           # Input: Slider for chemE student population standard deviation 
                           sliderInput(inputId = "pop_chem_sd",
                                       label = "Chemistry Population Weekly Study Hours SD",
                                       min = 0,
                                       max = 25,
                                       value = 7),
                           
                           # Input: Slider for chemE student population mean study hours 
                           sliderInput(inputId = "pop_chem_mean",
                                       label = "Chemistry Population Weekly Study Hours Mean",
                                       min =0,
                                       max = 100,
                                       value = 20)
                           ),
                  
                  tabPanel(title = "Sample Opts",
                           
                           textOutput(outputId = "directions_2"),
                           
                           # create the "Collect Sample" button for sampling from the population
                           actionButton(inputId = "collect_samples", label = "Collect Samples"),
                           
                           # Inputs for samples ----
                           
                           # Input: Slider for architecture sample size 
                           sliderInput(inputId = "samp_arc_size",
                                       label = "Architecture Sample Size",
                                       min = 0,
                                       max = 1000,
                                       value = 50),
                           
                           
                           # Input: Slider for biology sample size 
                           sliderInput(inputId = "samp_bio_size",
                                       label = "Biology Sample Size",
                                       min = 0,
                                       max = 1000,
                                       value = 50),
                           
                           
                           # Input: Slider for bme sample size 
                           sliderInput(inputId = "samp_bme_size",
                                       label = "BME Sample Size",
                                       min = 0,
                                       max = 1000,
                                       value = 50),
                           
                           
                           # Input: Slider for business sample size 
                           sliderInput(inputId = "samp_bus_size",
                                       label = "Business Sample Size",
                                       min = 0,
                                       max = 1000,
                                       value = 50),
                           
                           
                           # Input: Slider for chemical engineering sample size 
                           sliderInput(inputId = "samp_cheg_size",
                                       label = "ChemE Sample Size",
                                       min = 0,
                                       max = 1000,
                                       value = 50),
                           
                           
                           # Input: Slider for chemistry sample size 
                           sliderInput(inputId = "samp_chem_size",
                                       label = "Chemistry Sample Size",
                                       min = 0,
                                       max = 1000,
                                       value = 50)
                           
                           )
                  
                  )
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Population Data", 
                           # population main panel text
                           textOutput(outputId = "pop_text"),
                           
                           tableOutput(outputId = "pop_desc_stats"),
                           
                           # Output: Population Histogram ----
                           plotOutput(outputId = "pop_hist_plot"),
                           
                           # Output: Population boxplot
                           plotOutput(outputId = "pop_box_plot")
                           ),
                  
                  
                  tabPanel("Sample Data",
                           # sample main panel text
                           textOutput(outputId = "samp_text"),
                           
                           # Output: Descriptive Statistics
                           tableOutput(outputId = "samp_desc_stats"),
                           
                           # Output: Sample histogram ----
                           plotOutput(outputId = "samp_hist_plot"),
                           
                           # Output: Sample boxplot
                           plotOutput(outputId = "samp_box_plot"),
                           
                           ),
                  
                  
                  tabPanel("ANOVA output",
                           #tableOutput("test_2"),
                           textOutput("aov_summary_directions"),
                           
                           verbatimTextOutput(outputId = "aov_output_summary"),
                           
                           textOutput("aov_tidy_directions"),
                           
                           tableOutput(outputId = "aov_output_tidy"),

                           textOutput("aov_effects_directions"),
                           
                           verbatimTextOutput(outputId = "aov_output_effects"),
                           
                           textOutput("aov_tukey_directions"),
                           
                           verbatimTextOutput(outputId = "aov_output_tukey")
                           
                                                      
                           ),
                  
                  tabPanel("ANOVA output pt.2",
                           #tableOutput("test_2"),
                           textOutput("aov_5_directions"),
                           
                           verbatimTextOutput(outputId = "aov_output_levene"),
                           
                           textOutput("aov_6_directions"),
                           
                           verbatimTextOutput(outputId = "aov_output_welch")
                           
                           
                  )
                  
                  # tabPanel("Joined Output",
                  #          textOutput("test_1"),
                  #          
                  #          )
                  # 
                  )
      

  
      
      
    )
  )
)





### Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$directions_1 <- renderText({
    "Change the population parameters and click Generate Population."
  })
  
  output$directions_2 <- renderText({
    "Change sample sizes and click Collect Samples"
  })
  
  output$pop_text <- renderText({
    "Notice the changes in the distributions as your change the parameters to the left in the side panel."
  })
  
  output$samp_text <- renderText({
    "Notice the changes in the distributions as your change the parameters in the underlying populations and sample sizes."
  })
  # generate the population distribution to draw samples from
  
  disciplines <- c("arc", "bio", "bme", "bus", "cheg", "chem")
  
  # store the input population sizes
  pop_arc_size <- eventReactive(input$gen_pop, input$pop_arc_size)
  pop_bio_size <- eventReactive(input$gen_pop, input$pop_bio_size)
  pop_bme_size <- eventReactive(input$gen_pop, input$pop_bme_size)
  pop_bus_size <- eventReactive(input$gen_pop, input$pop_bus_size)
  pop_cheg_size <- eventReactive(input$gen_pop, input$pop_cheg_size)
  pop_chem_size <- eventReactive(input$gen_pop, input$pop_chem_size)
  

   
  pop_n <- eventReactive(input$gen_pop, 
                         {c(pop_arc_size(),
                            pop_bio_size(),
                            pop_bme_size(),
                            pop_bus_size(),
                            pop_cheg_size(),
                            pop_chem_size())
     })
  

   
   
   
  # store the input population means and SDs
  pop_arc_mean <- eventReactive(input$gen_pop, input$pop_arc_mean)
  pop_bio_mean <- eventReactive(input$gen_pop, input$pop_bio_mean)
  pop_bme_mean <- eventReactive(input$gen_pop, input$pop_bme_mean)
  pop_bus_mean <- eventReactive(input$gen_pop, input$pop_bus_mean)
  pop_cheg_mean <- eventReactive(input$gen_pop, input$pop_cheg_mean)
  pop_chem_mean <- eventReactive(input$gen_pop, input$pop_chem_mean)
  
  pop_disc_mean <- eventReactive(input$gen_pop, {c(pop_arc_mean(),
                     pop_bio_mean(),
                     pop_bme_mean(),
                     pop_bus_mean(),
                     pop_cheg_mean(),
                     pop_chem_mean())
  })
  
  
  
  
  pop_arc_sd <- eventReactive(input$gen_pop, input$pop_arc_sd)
  pop_bio_sd <- eventReactive(input$gen_pop, input$pop_bio_sd)
  pop_bme_sd <- eventReactive(input$gen_pop, input$pop_bme_sd)
  pop_bus_sd <- eventReactive(input$gen_pop, input$pop_bus_sd)
  pop_cheg_sd <- eventReactive(input$gen_pop, input$pop_cheg_sd)
  pop_chem_sd <- eventReactive(input$gen_pop, input$pop_chem_sd)
  
  pop_disc_sd <- eventReactive(input$gen_pop, {c(pop_arc_sd(),
                   pop_bio_sd(),
                   pop_bme_sd(),
                   pop_bus_sd(),
                   pop_cheg_sd(),
                   pop_chem_sd())
  })

  
  
  pop_disc_name_vect <- eventReactive(input$gen_pop, {rep(x = disciplines, times = pop_n())})
  
  pop_disc_mean_vect <- eventReactive(input$gen_pop, {rep(x = as.double(pop_disc_mean()), times = pop_n())})

  pop_disc_sd_vect <- eventReactive(input$gen_pop, {rep(x = as.double(pop_disc_sd()), times = pop_n())})
  
  pop_study_hrs_vect <- eventReactive(input$gen_pop, 
                                      {modify2(.x = pop_disc_mean_vect(), .y = pop_disc_sd_vect(), .f = ~rnorm(n = 1, mean = .x, sd = .y))})

  
#  pop_arc_df <- eventReactive(input$gen_pop, {
#    tibble(pop_arc_size = rep())
#  })
  
  
  pop_data_df <- eventReactive(input$gen_pop, {tibble(discipline = pop_disc_name_vect(), 
                        pop_disc_mean = pop_disc_mean_vect(), 
                        pop_disc_sd = pop_disc_sd_vect(),
                        pop_study_hrs = pop_study_hrs_vect())
  })
  

  
  
  
  
  
  
  # generate the samples from input parameters ----
  
  
  # Step 1: store the sample sizes
  samp_arc_size <- eventReactive(input$collect_samples, {input$samp_arc_size})
  samp_bio_size <- eventReactive(input$collect_samples, {input$samp_bio_size})
  samp_bme_size <- eventReactive(input$collect_samples, {input$samp_bme_size})
  samp_bus_size <- eventReactive(input$collect_samples, {input$samp_bus_size})
  samp_cheg_size <- eventReactive(input$collect_samples, {input$samp_cheg_size})
  samp_chem_size <- eventReactive(input$collect_samples, {input$samp_chem_size})
  
  
  samp_n <- eventReactive(input$collect_samples, {c(samp_arc_size(),
              samp_bio_size(),
              samp_bme_size(),
              samp_bus_size(),
              samp_cheg_size(),
              samp_chem_size())
  })
  
  
  
  
  
  # Step 2: create the sample dfs
  
  samp_arc_df <- eventReactive(input$collect_samples, {pop_data_df() %>% 
    filter(discipline == "arc") %>% 
    sample_n(samp_arc_size())
  })
  
  samp_bio_df <- eventReactive(input$collect_samples, {pop_data_df() %>% 
    filter(discipline == "bio") %>% 
    sample_n(samp_bio_size())
  })
  
  samp_bme_df <- eventReactive(input$collect_samples, {pop_data_df() %>% 
    filter(discipline == "bme") %>% 
    sample_n(samp_bme_size())
  })
  
  samp_bus_df <- eventReactive(input$collect_samples, {pop_data_df() %>% 
    filter(discipline == "bus") %>% 
    sample_n(samp_bus_size())
  })
  
  samp_cheg_df <- eventReactive(input$collect_samples, {pop_data_df() %>% 
    filter(discipline == "cheg") %>% 
    sample_n(samp_cheg_size())
  })
  
  samp_chem_df <- eventReactive(input$collect_samples, {pop_data_df() %>% 
    filter(discipline == "chem") %>% 
    sample_n(samp_chem_size())
  })
  
  
  # Step 3: make df for each of the three samples by subsetting part of the population df according to the randomly generated indices
  
  data_gen_df <- eventReactive(input$collect_samples,
                               {bind_rows(list(samp_arc_df(), samp_bio_df(), samp_bme_df(), samp_bus_df(), samp_cheg_df(), samp_chem_df()))
                                 })
  
  
  # rename pop_study_hrs to study_hrs
  data_gen_df_2 <- eventReactive(input$collect_samples,
                               {rename(data_gen_df(), study_hrs = pop_study_hrs)
                               })
  
  # round the study_hrs to one decimal place
  data_gen_df_3 <- eventReactive(input$collect_samples, {data_gen_df_2() %>% 
    mutate(study_hrs = round(study_hrs, 1))
  })
  
  
  
  
  
  # Population output ----
  
  
  # visualize the population data to make sure created correctly
  
  output$pop_desc_stats <- renderTable({
    
    pop_data_df() %>% 
      group_by(discipline) %>% 
      summarize(n = n(),
                mean = mean(pop_study_hrs),
                sd = sd(pop_study_hrs))
    
  })
  
  output$pop_hist_plot <- renderPlot({
    
    ggplot(data = pop_data_df(), aes(x = pop_study_hrs, fill = discipline)) +
      geom_histogram() + 
      facet_grid(discipline ~ .) +
      labs(title = "Population histogram",
           x  = "Study hours",
           y = "Count")
  })
  
  
  output$pop_box_plot <- renderPlot({
    
    ggplot(data = pop_data_df(), aes(x = discipline, y = pop_study_hrs, fill = discipline)) +
      geom_boxplot() +
      labs(title = "Population box plot",
           x = "Discipline",
           y = "Study hours")
  })
  
  
  # output$test_2 <- renderTable({
  #   data_gen_df_3()
  # })
  # 
  
  
  ### Sample ouput ----
  # create sample descriptive stats output
  
  output$samp_desc_stats <- renderTable({
    
    data_gen_df_3() %>% 
      group_by(discipline) %>% 
      summarize(n = n(),
                mean = mean(study_hrs),
                sd = sd(study_hrs))
    
  })
  
  output$samp_hist_plot <- renderPlot({
    
    ggplot(data = data_gen_df_3(), aes(x = study_hrs, fill = discipline)) +
      geom_histogram() + 
      facet_grid(discipline ~ .) +
      labs(title = "Sample Histogram",
           x = "Study hours",
           y = "Count")
  })
  
  
  
  output$samp_box_plot <- renderPlot({
    
    ggplot(data = data_gen_df_3(), aes(x = discipline, y = study_hrs, fill = discipline)) +
      geom_boxplot() +
      labs(title = "Sample Boxplot",
           x = "Discipline",
           y = "Study hours")
  })
  
  
  # output for the ANOVA test
  data_gen_df_4 <- eventReactive(input$collect_samples, {
    mutate(data_gen_df_3(), discipline = as.factor(discipline))
  })
   

  study_hrs_aov <- eventReactive(input$collect_samples, {
    aov(study_hrs ~ discipline, data = data_gen_df_4())
  })
  
  
  
  output$aov_summary_directions <- renderText({
    "Output from summary(aov_model): "
  })
  
    
  output$aov_output_summary <- renderPrint({
    summary(study_hrs_aov())
  })
  
  
  output$aov_tidy_directions <- renderText({
    "Output from tidy(aov_model): "
  })
  
  output$aov_output_tidy <- renderTable({
    tidy(study_hrs_aov())
  })

  output$aov_effects_directions <- renderText({
    "Output from anova_stats() from sjstats package, including effect sizes: "
  })
  
  output$aov_output_effects <- renderPrint({
    anova_stats(study_hrs_aov())
  })
  
  
  output$aov_tukey_directions <- renderText({
    "Output from post hocs (using glht()): "
  })

  ### Let's use Tukey's method for our post hoc multiple comparisons
  post_hocs <- eventReactive(input$collect_samples, {
    glht(study_hrs_aov(), linfct = mcp(discipline = "Tukey"))
  })
  
  output$aov_output_tukey <- renderPrint({
    
    summary(post_hocs())
    
  })
  
  
  output$aov_5_directions <- renderText({
    "Check assumption of homogeneous variances using Levene's test"
  })
  
  # calculate results from Levene's test
  output$aov_output_levene <- renderPrint({
    leveneTest(data_gen_df_4()$study_hrs, data_gen_df_4()$discipline, center = mean)
  })
  
  
  output$aov_6_directions <- renderText({
    "If Levene's test suggests heterogeneous variances, try using Welch's F with oneway.tests()"
  })
  
  output$aov_output_welch <- renderPrint({
    oneway.test(study_hrs ~ discipline, data = data_gen_df_4())
  })
  
  
  
}




shinyApp(ui = ui, server = server)

