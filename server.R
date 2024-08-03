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

# Function to generate volcano plot
generate_volcano_plot <- function(df,
                                  genes,
                                  logFC, # x_axis_column
                                  FDR, # y_axis_column
                                  fold_change_threshold, 
                                  p_value_threshold, 
                                  point_size,
                                  plot_name, 
                                  x_axis_name, 
                                  y_axis_name,
                                  sig_points_label) {
  
  # Error handle when the user entered column names that do not exist in the input file
  if (!(genes %in% colnames(df))) {
    stop("Specified column for gene names not found.")
  }
  if (!(logFC %in% colnames(df))) {
    stop("Specified column for x-axis not found.")
  }
  if (!(FDR %in% colnames(df))) {
    stop("Specified column for y-axis not found.")
  }
  
  # Add significant level of each gene
  DE_res <- df %>%
    mutate(significance = case_when(
      .data[[logFC]] >= fold_change_threshold & .data[[FDR]] <= p_value_threshold ~ 'Upregulated',
      .data[[logFC]] <= -fold_change_threshold & .data[[FDR]] <= p_value_threshold ~ 'Downregulated',
      abs(.data[[logFC]]) < fold_change_threshold | .data[[FDR]] > p_value_threshold ~ 'Not significant'
    ))
  
  # Generate the volcano plot
  p <- ggplot(DE_res, aes(x = .data[[logFC]], y = -log10(.data[[FDR]]), color = significance, label = .data[[genes]])) +
    geom_point(size = point_size) +
    geom_vline(xintercept = c(-fold_change_threshold, fold_change_threshold), color = "grey", linetype = "dashed") +
    geom_hline(yintercept = -log10(p_value_threshold), color = "grey", linetype = "dashed") +
    labs(x = x_axis_name, y = y_axis_name, color = 'Significance', title = plot_name) +
    scale_color_manual(values = c('blue', 'grey90', 'red')) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) # Centered title
  
  # Subset for down-regulated and up-regulated genes
  up <- DE_res[DE_res[[logFC]] >= fold_change_threshold & DE_res[[FDR]] <= p_value_threshold,]
  down <- DE_res[DE_res[[logFC]] <= -fold_change_threshold & DE_res[[FDR]] <= p_value_threshold,]
  
  # Label significant genes (up-regulated and down-regulated)
  if (sig_points_label) {
    p <- p +
      geom_text(data = up, aes(label = .data[[genes]]), size = 3, vjust = -1, hjust = 0, color = "red") +
      geom_text(data = down, aes(label = .data[[genes]]), size = 3, vjust = -1, hjust = 1, color = "blue")
  }
  
  return(list(plot = p, 
              down_genes = down[[genes]], 
              up_genes = up[[genes]]))
}

# Define server logic
server <- function(input, output, session) {
  
  # Read the input CSV file once from user upload
  df <- reactiveVal(NULL)
  
  # Handle file upload
  observeEvent(input$file1, {
    req(input$file1)
    inFile <- input$file1
    data <- read.csv(inFile$datapath, header = input$header, sep = input$sep)
    colnames(data)[1] <- "Genes" # Ensure the gene column is named "Genes"
    df(data)
  })
  
  # Handle "Load Demo Data" 
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
  })
  
  # Render data table
  output$data1 <- renderDT({
    # Ensure the data is loaded before display
    req(df())
    datatable(df())
  })
  
  # Render volcano plot
  output$volcano_plot <- renderPlotly({
    # Silent the error of empty object before the user upload a file
    # Ensure the data is loaded before display
    req(df())
    req(input$gene_column, input$x_axis_column, input$y_axis_column)
    
    # Generate the plot when the user uploads a file
    plot_data <- generate_volcano_plot(
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
    
    # Use this function from plotly to create interactive plot
    ggplotly(plot_data$plot)
  })
  
  # For download, the plot has to be generated again this function, can not use the plot in the previous function
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(input$plot_name, input$format)
    },
    content = function(file) {
      # Re-render the volcano plot
      plot_data <- generate_volcano_plot(
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
      
      # Save the plot as a chosen file type
      ggsave(file, plot = plot_data$plot, width = 8, height = 6, dpi = 300, bg = "white")
    }
  )
  
  # Render up-regulated genes as a comma-separated string
  output$up_genes <- renderText({
    req(df())
    plot_data <- generate_volcano_plot(
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
    
    # Join the gene names with commas and return as plain text
    paste(plot_data$up_genes, collapse = ", ")
  })
  
  # Render down-regulated genes as a comma-separated string
  output$down_genes <- renderText({
    req(df())
    plot_data <- generate_volcano_plot(
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
    
    # Join the gene names with commas and return as plain text
    paste(plot_data$down_genes, collapse = ", ")
  })
  
}
