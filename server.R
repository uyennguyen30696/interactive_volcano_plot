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
      # .data is the syntax from the "dplyr" library
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
  # Reactive value to store up-regulated genes
  up_genes <- reactiveVal(NULL)
  # Reactive value to store down-regulated genes
  down_genes <- reactiveVal(NULL)
  
  # Observe changes in the reactive input values from the user and execute corresponding actions
  observeEvent(input$file1, {
    data <- read.csv(input$file1$datapath, header = input$header, sep = input$sep)
    # Name the gene column (because the test files do not have gene column name)
    colnames(data)[1] <- "Genes"
    df(list(data = data))
  })
  
  # Display data
  output$data1 <- renderDT({
    # Ensure the data is loaded before display
    req(df()$data)
    datatable(df()$data)
  })
  
  # Render volcano plot
  output$volcano_plot <- renderPlotly({
    # Silent the error of empty object before the user upload a file
    # Ensure the data is loaded before display
    req(df()$data)
    
    # Generate the plot when the user uploads a file
    p <- generate_volcano_plot(df()$data,
                               input$gene_column,
                               input$x_axis_column,
                               input$y_axis_column,
                               input$fold_change_threshold, 
                               input$p_value_threshold, 
                               input$point_size,
                               input$plot_name, 
                               input$x_axis_name, 
                               input$y_axis_name,
                               input$sig_points_label)
    
    # Store up-regulated genes
    up_genes(p$up_genes)
    # Store down-regulated genes
    down_genes(p$down_genes)
    
    # Use this function from plotly to create interactive plot
    ggplotly(p$plot)
    
  })
  
  # Display up-regulated genes
  output$up_genes <- renderText({
    if (length(up_genes()) > 0) {
      paste(up_genes(), collapse = ", ")
    } else {
      " "
    }
  })
  
  # Display down-regulated genes
  output$down_genes <- renderText({
    if (length(down_genes()) > 0) {
      paste(down_genes(), collapse = ", ")
    } else {
      " "
    }
  })
  
  # For download, the plot has to be generated again this function, can not use the plot in the previous function
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(input$plot_name, input$format)
    },
    content = function(file) {
      # Ensure the data is loaded 
      req(df()$data)
      
      # Re-render the volcano plot
      p <- generate_volcano_plot(df()$data,
                                 input$gene_column,
                                 input$x_axis_column,
                                 input$y_axis_column,
                                 input$fold_change_threshold, 
                                 input$p_value_threshold, 
                                 input$point_size,
                                 input$plot_name, 
                                 input$x_axis_name, 
                                 input$y_axis_name,
                                 input$sig_points_label)
      
      # Save the plot as a chosen file type
      ggsave(file, plot = p$plot, width = 8, height = 6, dpi = 300, bg = "white")
    }
  )
}

