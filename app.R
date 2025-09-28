# Project: Breast Cancer Screening AMB
# Author: Karthik Krishnan

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(shinyWidgets)

source("BrCaScreen_ABM.R")

ui <- fluidPage(
  chooseSliderSkin(
    skin = c("Flat"),
    color = "#112446"
  ),
  # Custom CSS styles
  tags$head(
    tags$style(HTML("
    body {
      background-color: #cfd8dc;
      color: black;
      font-family: Poppins, sans-serif;
    }
    .navbar {
      background-color: #333333;
      font-size: 18px;
      color: white;
    }
    .navbar-brand {
      font-weight: bold;
      color: white;
    }
    .navbar-nav {
      margin-left: auto;
    }
    .navbar-nav li a {
      color: white;
    }
    .sidebar-panel {
      display: flex;
      flex-direction: column;
      align-items: center;
    }
    .btn-success {
      background-color: #03ac13;  
      border-color: #03ac13;
      font-size: 15px;
      padding: 10px;
      transition: background-color 0.3s ease;
      width: 100px;
    }
    .btn-danger {
      background-color: #d0312d;
      border-color: #d0312d;
      font-size: 15px;
      padding: 10px;
      transition: background-color 0.3s ease;
      width: 100px;
    }
    .btn-success:hover {
      background-color: #AEDCAE;
      border-color: #AEDCAE;
      opacity: 0.9;
    }
    .btn-danger:hover {
      background-color: #ff7f7f;  
      border-color: #ff7f7f;
      opacity: 0.9;
    }
    .plot-container {
      background-color: #F0F0F0;
      border: 3px solid #F0F0F0;
    }
    .button-container {
      width: 60%;
    }
    /* Customize the labels and the slider itself */
    .slider-wrapper .control-label {
      font-family: Poppins, sans-serif;
      font-weight: normal;
      font-size: 15px;
      color: #000000;
    }
    .slider-wrapper {
      margin-bottom: 5px;
      display: flex;
      flex-direction: column;
      align-items: center;
      width: 100%;
    }
    .slider-wrapper .form-group {
      width: 100%;
      margin-bottom: 5px;
      display: flex;
      flex-direction: column;
      align-items: center;
    }
    .slider-wrapper .irs {
      width: 100% !important;
      margin-bottom: 20px;
    }
    .slider-wrapper .irs-with-grid {
      width: 100% !important;
    }
    .slider-wrapper input,
    .slider-wrapper .form-control {
      width: 100%;
    }
    .slider-wrapper {
      margin-bottom: 0px;
      padding-bottom: 0px;
    }
    .button-container {
      margin-top: 0px; /* Add this line to reduce space above buttons */
      width: 60%;
    }
    .sidebar-panel hr {
      margin-top: 10px;
      margin-bottom: 0px;
    }
    .runs-wrapper {
      font-family: Poppins, sans-serif;
      font-weight: normal;
    }
    "))
  ),
  
  # Navigation Bar with "About" tab
  navbarPage(
    title = div("A Breast Cancer Screening Simulator"),
    
    # About Tab
    tabPanel(
      "About",
      h3("About the Simulator"),
      p("This simulator models breast cancer screening in rural areas, taking into account various socio-economic and demographic factors.")
    ),
    
    # Main simulation content tab
    tabPanel(
      "Simulation",
      sidebarLayout(
        sidebarPanel(
          div(class = "sidebar-panel",
              chooseSliderSkin("Modern"),
              
              div(class = "slider-wrapper",
                  sliderInput("p_racialized", "Proportion of Racialized Agents",
                              min = 0, max = 1, value = 0.25, step = 0.01)
              ),
              
              div(class = "slider-wrapper",
                  sliderInput("p_poverty", "Proportion of Agents in Poverty",
                              min = 0, max = 1, value = 0.19, step = 0.01)
              ),
              
              div(class = "slider-wrapper",
                  sliderInput("p_uninsured", "Proportion of Uninsured Agents",
                              min = 0, max = 1, value = 0.113, step = 0.01)
              ),
              
              div(class = "slider-wrapper",
                  sliderInput("p_remote", "Proportion Living Remotely",
                              min = 0, max = 1, value = 0.75, step = 0.01)
              ),
              
              div(class = "slider-wrapper",
                  sliderInput("screening_rate", "Screening Rate",
                              min = 0, max = 1, value = 0.733, step = 0.01)
              ),
              
              div(class = "runs-wrapper",
                  numericInput("num_runs", "Number of Simulation Runs", value = 1, min = 1, max = 100, step = 1)
                  ),

              hr(),
              
              div(class = "button-container",
                  fluidRow(
                    column(6,
                           actionButton("run_sim", "Run", class = "btn btn-success")
                    ),
                    column(6,
                           actionButton("clear_plot", "Clear", class = "btn btn-danger")
                    )
                  )
              )
          ),
          width = 4
        ),
        
        mainPanel(
          # Navigation Bar for selecting plot type
          tabsetPanel(
            type = "tabs",
            tabPanel("Total Screened", plotOutput("screeningPlot", height = "500px")),
            tabPanel("Proportion Newly Screened", plotOutput("proportionPlot", height = "500px"))
          ),
          
          div(
            style = "margin-top: 20px; padding: 2px;",
            div(
              style = "text-align: center; margin-bottom: 20px; margin-top: 0px",  # Add margin-bottom for spacing
              h5(em("Optional: Select a preset corresponding to one type of rural area."))
            ),
            div(
              style = "text-align: center; display: flex; justify-content: space-around; flex-wrap: wrap; gap: 5px;",  # Reduced gap for closer buttons
              actionButton("preset_1", "Outlying", class = "btn btn-secondary", style = "font-size: 16px; padding: 10px 20px; width: 170px;"),
              actionButton("preset_2", "Developed", class = "btn btn-secondary", style = "font-size: 16px; padding: 10px 20px; width: 170px;"),
              actionButton("preset_3", "Well-Resourced", class = "btn btn-secondary", style = "font-size: 16px; padding: 10px 20px; width: 170px;"),
              actionButton("preset_4", "Adaptable", class = "btn btn-secondary", style = "font-size: 16px; padding: 10px 20px; width: 170px;")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  simulation_results <- reactiveValues(data_list = list())
  
  observeEvent(input$run_sim, {
    if (length(simulation_results$data_list) >= 4) {
      showNotification("Max. 4 plots. Please clear plot to plot again.", type = "warning", duration = 5)
      return(NULL)
    }
    
    result <- run_multiple_models(
      n_runs = input$num_runs,
      p_racialized = input$p_racialized,
      p_poverty = input$p_poverty,
      p_uninsured = input$p_uninsured,
      p_remote = input$p_remote,
      screening_rate = input$screening_rate,
      avg_degree = 3
    )
    
    combined_logs <- do.call(rbind, lapply(seq_along(result$logs), function(run) {
      log <- result$logs[[run]]
      log$Run <- paste0("Run_", run)
      return(log)
    }))
    
    screened_avg <- combined_logs %>%
      group_by(Time) %>%
      summarise(
        total_screened = mean(Total_Screened),
        avg_white_screened = mean(White_Total_Screened),
        avg_racialized_screened = mean(Racialized_Total_Screened),
        avg_newly_screened = mean(Newly_Screened)  # Ensure this column is available in your logs
      ) %>%
      mutate(
        Run = paste0("Run_", as.character(Sys.time())),
        prop_newly_screened = avg_newly_screened / total_screened
      )
    
    simulation_results$data_list <- append(simulation_results$data_list, list(screened_avg))
  })
  
  observeEvent(input$clear_plot, {
    simulation_results$data_list <- list()
  })
  
  observeEvent(input$preset_1, {
    updateSliderInput(inputId = "p_racialized", value = 0.25)
    updateSliderInput(inputId = "p_poverty", value = 0.19)
    updateSliderInput(inputId = "p_uninsured", value = 0.113)
    updateSliderInput(inputId = "p_remote", value = 0.75)
    updateSliderInput(inputId = "screening_rate", value = 0.733)
  })
  
  observeEvent(input$preset_2, {
    updateSliderInput(inputId = "p_racialized", value = 0.21)
    updateSliderInput(inputId = "p_poverty", value = 0.13)
    updateSliderInput(inputId = "p_uninsured", value = 0.083)
    updateSliderInput(inputId = "p_remote", value = 0.5)
    updateSliderInput(inputId = "screening_rate", value = 0.74)
  })
  
  observeEvent(input$preset_3, {
    updateSliderInput(inputId = "p_racialized", value = 0.14)
    updateSliderInput(inputId = "p_poverty", value = 0.11)
    updateSliderInput(inputId = "p_uninsured", value = 0.08)
    updateSliderInput(inputId = "p_remote", value = 0.75)
    updateSliderInput(inputId = "screening_rate", value = 0.74)
  })
  
  observeEvent(input$preset_4, {
    updateSliderInput(inputId = "p_racialized", value = 0.30)
    updateSliderInput(inputId = "p_poverty", value = 0.23)
    updateSliderInput(inputId = "p_uninsured", value = 0.107)
    updateSliderInput(inputId = "p_remote", value = 0.25)
    updateSliderInput(inputId = "screening_rate", value = 0.738)
  })
  
  # Plot for total number of agents screened
  output$screeningPlot <- renderPlot({
    if (length(simulation_results$data_list) == 0) return(NULL)
    
    all_data <- bind_rows(simulation_results$data_list, .id = "run_id")
    
    plot_colors <- c("Run_1" = "aquamarine", "Run_2" = "darkgoldenrod1", "Run_3" = "red", "Run_4" = "purple")
    run_ids <- unique(all_data$run_id)
    names(plot_colors) <- run_ids
    
    ggplot(all_data, aes(x = Time, group = run_id)) +
      geom_line(aes(y = total_screened, color = run_id), size = 1.2) +
      labs(
        title = "Total Agents Screened Over Time",
        x = "Time Step",
        y = "Agents Screened"
      ) +
      scale_color_manual(
        name = "Simulation Runs",
        values = plot_colors[run_ids]
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(
          size = 16,
          color = "black",
          hjust = 0.5,
          family = "Roboto"
        ),
        axis.title.x = element_text(
          color = "black",
          size = 14,
          family = "Roboto",
          margin = margin(t = 10)
        ),
        axis.title.y = element_text(
          color = "black",
          size = 14,
          family = "Roboto",
          margin = margin(r = 10)
        ),
        axis.text = element_text(
          color = "black",
          size = 12,
          family = "Roboto"
        ),
        plot.margin = margin(20, 20, 20, 20)
      )
  })
  
  # Plot for proportion of newly-screened agents
  output$proportionPlot <- renderPlot({
    if (length(simulation_results$data_list) == 0) return(NULL)
    
    all_data <- bind_rows(simulation_results$data_list, .id = "run_id")
    
    plot_colors <- c("Run_1" = "aquamarine", "Run_2" = "darkgoldenrod1", "Run_3" = "red", "Run_4" = "purple")
    run_ids <- unique(all_data$run_id)
    names(plot_colors) <- run_ids
    
    ggplot(all_data, aes(x = Time, y = prop_newly_screened, group = run_id)) +
      geom_line(aes(color = run_id), size = 1.2) +
      labs(
        title = "Proportion of Newly Screened Agents Over Time",
        x = "Time Step",
        y = "Proportion of Newly Screened Agents"
      ) +
      scale_color_manual(
        name = "Simulation Runs",
        values = plot_colors[run_ids]
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(
          size = 16,
          color = "black",
          hjust = 0.5,
          family = "Roboto"
        ),
        axis.title.x = element_text(
          color = "black",
          size = 14,
          family = "Roboto",
          margin = margin(t = 10)
        ),
        axis.title.y = element_text(
          color = "black",
          size = 14,
          family = "Roboto",
          margin = margin(r = 10)
        ),
        axis.text = element_text(
          color = "black",
          size = 12,
          family = "Roboto"
        ),
        plot.margin = margin(20, 20, 20, 20)
      )
  })
}

shinyApp(ui = ui, server = server)
