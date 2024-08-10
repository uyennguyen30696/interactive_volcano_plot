#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(data.table)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)

# Import generate volcano plot function
source('generate_volcano_plot.R')

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to hold the dataframe
  df <- reactiveVal(NULL)
  
  # Load demo data when "Load Demo Data" is selected
  observeEvent(input$data_source, {
    if (input$data_source == "Load Demo Data") {
      demo_data_path <- file.path("data", "dge_brain_9m_vs_24m.csv")
      if (file.exists(demo_data_path)) {
        demo_data <- read.csv(demo_data_path, header = TRUE, sep = ",")
        colnames(demo_data)[1] <- "Genes" # Ensure the gene column is named "Genes"
        df(demo_data)
      } else {
        showNotification("Demo data file not found", type = "error")
      }
    }
  }, ignoreInit = TRUE)
  
  # Observe file input changes and update the dataframe
  observeEvent(input$file1, {
    req(input$file1)
    inFile <- input$file1
    data <- read.csv(inFile$datapath, header = input$header, sep = input$sep)
    colnames(data)[1] <- "Genes" # Ensure the gene column is named "Genes"
    df(data)
  })
  
  # Reset inputs when the data source is changed
  observeEvent(input$data_source, {
    updateTextInput(session, "gene_column", value = "Genes")
    updateTextInput(session, "x_axis_column", value = "logFC")
    updateTextInput(session, "y_axis_column", value = "FDR")
    updateSliderInput(session, "point_size", value = 1)
    updateSliderInput(session, "fold_change_threshold", value = 1)
    updateSliderInput(session, "p_value_threshold", value = 0.05)
    updateTextInput(session, "plot_name", value = "Volcano plot")
    updateTextInput(session, "x_axis_name", value = "LogFC")
    updateTextInput(session, "y_axis_name", value = "-Log10(FDR)")
    updateCheckboxInput(session, "sig_points_label", value = FALSE)
  })
  
  # Function to generate the volcano plot
  generate_plot_data <- reactive({
    req(df())
    req(input$gene_column, input$x_axis_column, input$y_axis_column)
    
    generate_volcano_plot(
      df = df(),
      genes = input$gene_column,
      logFC = input$x_axis_column,
      FDR = input$y_axis_column,
      fold_change_threshold = input$fold_change_threshold,
      p_value_threshold = input$p_value_threshold,
      point_size = input$point_size,
      plot_name = input$plot_name,
      x_axis_name = input$x_axis_name,
      y_axis_name = input$y_axis_name,
      sig_points_label = input$sig_points_label
    )
  })
  
  # Render data table
  output$data1 <- renderDT({
    req(df())
    datatable(df())
  })
  
  # Render volcano plot
  output$volcano_plot <- renderPlotly({
    plot_data <- generate_plot_data()
    ggplotly(plot_data$plot)
  })
  
  # Download plot
  output$download_plot <- downloadHandler(
    filename = function() {
      # Get the current date
      date <- Sys.Date()
      paste0(input$plot_name, "_", date, input$format)
    },
    content = function(file) {
      plot_data <- generate_plot_data()
      ggsave(file, plot = plot_data$plot, width = 8, height = 6, dpi = 300, bg = "white")
    }
  )
  
  # Render up-regulated genes data table
  output$up_genes_table <- renderDT({
    plot_data <- generate_plot_data()
    datatable(plot_data$up_genes_df)
  })
  
  # Render down-regulated genes data table
  output$down_genes_table <- renderDT({
    plot_data <- generate_plot_data()
    datatable(plot_data$down_genes_df)
  })
  
  # Download up-regulated genes
  output$download_up_genes <- downloadHandler(
    filename = function() {
      # Get the current date
      date <- Sys.Date()
      paste0("up_regulated_genes", "_", date, ".csv")
    },
    content = function(file) {
      plot_data <- generate_plot_data()
      write.csv(plot_data$up_genes_df, file, row.names = FALSE)
    }
  )
  
  # Download down-regulated genes
  output$download_down_genes <- downloadHandler(
    filename = function() {
      # Get the current date
      date <- Sys.Date()
      paste0("down_regulated_genes", "_", date, ".csv")
    },
    content = function(file) {
      plot_data <- generate_plot_data()
      write.csv(plot_data$down_genes_df, file, row.names = FALSE)
    }
  )
  
}
