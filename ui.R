#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(DT)
library(shinydashboard)

# Define UI dashboard
dashboardPage(
  dashboardHeader(title = "Interactive Volcano Plot"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload_data", icon = icon("upload")),
      menuItem("Volcano Plot", tabName = "volcano_plot", icon = icon("chart-line")),
      menuItem("Significant Genes", tabName = "sig_genes", icon = icon("bolt"))
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
        ),
        checkboxInput("header", label = "File contains a header", value = TRUE),
        fluidRow(
          column(width = 4,
                 textInput("gene_column", label = "Enter column name for genes:", value = "Genes")),
          column(width = 4,
                 textInput("x_axis_column", label = "Enter column name for x axis:", value = "logFC")),
          column(width = 4,
                 textInput("y_axis_column", label = "Enter column name for y axis:", value = "FDR"))
        ),
        DTOutput(outputId = "data1")
      ),
      
      tabItem(
        tabName = "volcano_plot",
        # Use plotlyOutput instead of plotOutput to render interactive plot instead of static plot
        plotlyOutput(outputId = "volcano_plot"),
        fluidRow(
          column(width = 6, ""),
          column(width = 4,
                 align = "right",
                 downloadButton("download_plot", "Download Plot")),
          column(width = 2,
                 selectInput("format", "Select format:", choices = c(".png", ".jpg", ".jpeg", ".pdf"), selected = ".png")
          )
        ),
        fluidRow(
          column(width = 4,
                 sliderInput("point_size", "Adjust point size:", min = 0.5, max = 3, value = 1, step = 0.5)),
          column(width = 4,
                 sliderInput("fold_change_threshold", "Fold Change Threshold:", min = 0, max = 5, value = 1, step = 0.1)),
          column(width = 4,
                 sliderInput("p_value_threshold", "P-value Threshold:", min = 0, max = 0.1, value = 0.05, step = 0.01))
        ),
        fluidRow(
          column(width = 4,
                 textInput("plot_name", "Enter plot name:", value = "Volcano plot")),
          column(width = 4,
                 textInput("x_axis_name", "Enter x-axis name:", value = "LogFC")),
          column(width = 4,
                 textInput("y_axis_name", "Enter y-axis name:", value = "-Log10(FDR)"))
        ),
        fluidRow(
          column(width = 4,
                 checkboxInput("sig_points_label", label = "Show labels for significant genes", value = FALSE))
        )
      ),
      
      tabItem(
        tabName = "sig_genes",
        fluidRow(
          column(
            width = 12,
            tags$p("Up-regulated Genes"),
            verbatimTextOutput("up_genes")
          )
        ),
        fluidRow(
          column(
            width = 12,
            tags$p("Down-regulated Genes"),
            verbatimTextOutput("down_genes")
          )
        )
      )
      
    )
  )
  
)
