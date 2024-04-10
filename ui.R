#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(DT)

# Define UI dashboard
dashboardPage(
  dashboardHeader(title = "Interactive Volcano Plot"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "upload_data", icon = icon("upload")),
      menuItem("Volcano Plot", tabName = "volcano_plot", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "upload_data",
        fluidRow(
          column(width = 4,
                 fileInput("file1", "Upload a csv file",
                           multiple = FALSE,
                           accept = c(".csv"))),
          column(width = 4, 
                 textInput("sep", label = "Enter the separator character:", value = ",")),
          column(width = 4,
                 textInput("condition", label = "Enter column name for comparing conditions:", value = "group")),
        ),
        checkboxInput("header", label = "File contains a header", value = TRUE),
        fluidRow(
          column(width = 4,
                 textInput("gene_column", label = "Enter column name for genes:", value = "names")),
          column(width = 4,
                 textInput("x_axis_column", label = "Enter column name for x axis:", value = "logfoldchanges")),
          column(width = 4,
                 textInput("y_axis_column", label = "Enter column name for y axis:", value = "-log10(pvals_adj)"))
        ),
        DTOutput(outputId = "data1")
      ),
      tabItem(
        tabName = "volcano_plot",
        plotOutput(outputId = "volcano_plot"),
        fluidRow(
          column(width = 8, ""),
          column(width = 4,
                 align = "right",
                 downloadButton("download_plot", "Download Plot"))
        ),
        fluidRow(
          column(width = 4, 
                 sliderInput("point_size", "Adjust point size:", min = 0.5, max = 3, value = 1, step = 0.5)),
          column(width = 4, 
                 sliderInput("fold_change_threshold", "Fold Change Threshold:", min = 0, max = 5, value = 2, step = 0.1)),
          column(width = 4, 
                 sliderInput("p_value_threshold", "P-value Threshold:", min = 0, max = 0.1, value = 0.05, step = 0.01))
        ),
        fluidRow(
          column(width = 4,
                 textInput("plot_name", "Enter plot name:", value = "Volcano plot")),
          column(width = 4,
                 textInput("x_axis_name", "Enter x-axis name:", value = "Log Fold Changes")),
          column(width = 4,
                 textInput("y_axis_name", "Enter y-axis name:", value = "-log10(Adjusted P-values)"))
        ),
        fluidRow(
          column(width = 4,
                 checkboxInput("sig_points_label", label = "Show labels for significant genes", value = TRUE))
        )
        
      )
    )
  )
  
)
