library(dplyr)
library(ggplot2)

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
  
  # Filter out NA values 
  DE_res <- DE_res %>%
    filter(!is.na(.data[[genes]]) & !is.na(.data[[logFC]]) & !is.na(.data[[FDR]]) & !is.na(.data[["PValue"]]))
  
  # Subset for down-regulated and up-regulated genes
  up <- DE_res[DE_res[[logFC]] >= fold_change_threshold & DE_res[[FDR]] <= p_value_threshold,]
  down <- DE_res[DE_res[[logFC]] <= -fold_change_threshold & DE_res[[FDR]] <= p_value_threshold,]
  
  # Label significant genes (up-regulated and down-regulated)
  if (sig_points_label) {
    p <- p +
      geom_text(data = up, aes(label = .data[[genes]]), size = 3, vjust = -1, hjust = 0, color = "red") +
      geom_text(data = down, aes(label = .data[[genes]]), size = 3, vjust = -1, hjust = 1, color = "blue")
  }
  
  # Save the up and down genes along with their statistical values
  down_genes_df <- down[, c(genes, logFC, "PValue", FDR)]
  colnames(down_genes_df) <- c("Genes", "logFC", "PValue", "FDR")
  
  up_genes_df <- up[, c(genes, logFC, "PValue", FDR)]
  colnames(up_genes_df) <- c("Genes", "logFC", "PValue", "FDR")
  
  return(list(plot = p, 
              down_genes_df = down_genes_df, 
              up_genes_df = up_genes_df))
}
