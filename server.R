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
library(ggplot2)

# Function to generate volcano plot
generate_volcano_plot <- function(df, 
                                  condition,
                                  gene_names,
                                  x_axis_column,
                                  y_axis_column,
                                  fold_change_threshold, 
                                  p_value_threshold, 
                                  point_size, 
                                  plot_name, 
                                  x_axis_name, 
                                  y_axis_name, 
                                  sig_points_label) {
  # Error handle when the user entered column names that do not exist in the input file
  validate(
    need(gene_names %in% colnames(df()), "Error: Specified column for gene names not found."),
    need(condition %in% colnames(df()), "Error: Specified column for conditions not found."),
    need(x_axis_column %in% colnames(df()), "Error: Specified column for x-axis not found."),
    
    # TODO
    # Not working, why?
    # need(y_axis_column != "-log10(pvals_adj)"), "Error: Specified y-axis column not found.")
  )

  # TODO: The format of the "-log10(pvals_adj)" became "X.log10.pvals_adj.", can not check if the column name entered by user is in the file or not
  # Check if (y_axis_column != "-log10(pvals_adj)") for now, but need to find a different way
  if (y_axis_column != "-log10(pvals_adj)")  {
    showNotification("Error: Specified column for y-axis not found.", type = "error")
  }
  
  # Subset data for the 'in' group 
  b_in <- subset(df(), get(condition) == 'in')
  
  # Create volcano plot
  p <- ggplot(b_in, aes_string(x = x_axis_column, y = y_axis_column)) +
    geom_point(aes(color = ifelse(get(x_axis_column) <= -fold_change_threshold & pvals_adj <= p_value_threshold, "Down-regulated",
                                  ifelse(get(x_axis_column) >= fold_change_threshold & pvals_adj <= p_value_threshold, "Up-regulated", "Other"))), 
               size = point_size) +
    labs(x = x_axis_name, y = y_axis_name, title = plot_name) +
    theme_minimal() +
    scale_color_manual(values = c("Down-regulated" = "blue", "Up-regulated" = "red"), labels = c("Down-regulated", "Up-regulated")) +
    theme(plot.title = element_text(hjust = 0.5)) # Centered title
  
  # Add threshold lines
  p <- p + geom_vline(xintercept = c(-fold_change_threshold, fold_change_threshold), color = "grey", linetype = "dashed") +
    geom_hline(yintercept = -log10(p_value_threshold), color = "grey", linetype = "dashed") +
    theme(legend.title = element_blank())
  
  # Subset for down-regulated and up-regulated genes
  down <- subset(b_in, get(x_axis_column) <= -fold_change_threshold & pvals_adj <= p_value_threshold)
  up <- subset(b_in, get(x_axis_column) >= fold_change_threshold & pvals_adj <= p_value_threshold)
  
  # Label significant genes (up-regulated and down-regulated)
  if (sig_points_label) {
    p <- p +
      geom_text(data = down, aes(label = down[[gene_names]]), size = 4, vjust = -1, hjust = 1, color = "blue") +
      geom_text(data = up, aes(label = up[[gene_names]]), size = 4, vjust = -1, hjust = 0, color = "red")
  }

  return(p)
  
}

# Define server logic
server <- function(input, output, session) {
  
  # Read the input CSV file once from user upload
  df <- reactiveVal(NULL)
  
  # Observe changes in the reactive input values from the user and execute corresponding actions
  observeEvent(input$file1, {
    df(read.csv(input$file1$datapath, header = input$header, sep = input$sep))
  })
  
  # Display data
  output$data1 <- renderDT({
    datatable(df())
  })
  
  # Render volcano plot
  output$volcano_plot <- renderPlot({
    # Silent the error of empty object before the user upload a file
    req(df())

    # Then generate the plot when the user uploads a file
    generate_volcano_plot(df, 
                          input$condition,
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
  })
  
  # Function to generate plot and serve for download
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(input$plot_name, ".png")
    },
    content = function(file) {
      # Generate the plot
      p <- generate_volcano_plot(df, 
                                 input$condition,
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
      
      # Save the plot as PNG file
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
    }
  )
  
}
