# ============================================================
# BioData Explorer
# General Biological Dataset Exploration Software
# ============================================================

library(shiny)
library(ggplot2)
library(DT)
library(readxl)

# Allow uploads up to 200 MB
options(shiny.maxRequestSize = 200 * 1024^2)

required_packages <- c(
  "shiny",
  "ggplot2",
  "DT",
  "readxl"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (
  length(
    missing_packages
  ) > 0
) {
  stop(
    paste0(
      "Install the following required package(s) before running BioData Explorer: ",
      paste(
        missing_packages,
        collapse = ", "
      )
    )
  )
}



# ============================================================
# USER-FRIENDLY HELPER FUNCTIONS
# ============================================================

figure_guide <- function(what, how, look_for, limitation = NULL) {
  tags$details(
    class = "figure-guide",
    open = "open",
    tags$summary("How to interpret this figure"),
    div(
      class = "figure-guide-body",
      tags$p(tags$strong("What this plot shows: "), what),
      tags$p(tags$strong("How to read it: "), how),
      tags$p(tags$strong("What to look for: "), look_for),
      if (!is.null(limitation)) {
        tags$p(
          class = "figure-limit",
          tags$strong("Important limitation: "),
          limitation
        )
      }
    )
  )
}

help_note <- function(...) {
  div(class = "help-note", ...)
}

normalize_names <- function(x) {
  gsub("[^a-z0-9]", "", tolower(x))
}

guess_column <- function(column_names, patterns) {
  if (length(column_names) == 0) return(NULL)
  normalized <- normalize_names(column_names)
  for (pattern in patterns) {
    hit <- which(grepl(pattern, normalized))
    if (length(hit) > 0) return(column_names[hit[1]])
  }
  NULL
}


# ============================================================
# USER INTERFACE
# ============================================================

ui <- navbarPage(
  
  title = "BioData Explorer",
  
  header = tags$head(
    
    tags$style(
      
      HTML("

        :root {
          --background: #f6f8fb;
          --surface: #ffffff;
          --text: #1f2937;
          --muted: #6b7280;
          --border: #e5e7eb;
          --accent: #1f4e79;
          --accent-soft: #eef4fa;
        }

        html,
        body {
          background-color: var(--background);
        }

        body {
          font-family:
            -apple-system,
            BlinkMacSystemFont,
            'Segoe UI',
            Roboto,
            'Helvetica Neue',
            Arial,
            sans-serif;

          color: var(--text);
          font-size: 15px;
          line-height: 1.55;
        }

        .navbar {
          background-color: #ffffff;
          border: none;
          box-shadow: 0 1px 8px rgba(0, 0, 0, 0.08);
        }

        .navbar-brand {
          font-weight: 700;
          font-size: 20px;
          color: var(--accent) !important;
        }

        .navbar-nav > li > a {
          font-weight: 500;
        }

        .container-fluid {
          padding-left: 24px;
          padding-right: 24px;
        }

        .tab-content {
          padding-top: 22px;
          padding-bottom: 36px;
        }

        h1, h2, h3, h4 {
          color: var(--text);
          letter-spacing: -0.2px;
        }

        h2 {
          font-weight: 700;
        }

        h3 {
          font-weight: 650;
        }

        h4 {
          font-weight: 600;
        }

        .well {
          background-color: var(--surface);
          border: 1px solid var(--border);
          border-radius: 12px;
          box-shadow: 0 2px 10px rgba(0, 0, 0, 0.04);
        }

        .section-card {
          background-color: var(--surface);
          border: 1px solid var(--border);
          border-radius: 12px;
          padding: 20px;
          margin-bottom: 18px;
          box-shadow: 0 2px 10px rgba(0, 0, 0, 0.035);
        }

        .section-card h2,
        .section-card h3 {
          margin-top: 0;
        }

        .subtle-text {
          color: var(--muted);
          font-size: 13px;
        }

        .dataset-description {
          background-color: var(--accent-soft);
          border: 1px solid #dbe7f2;
          border-left: 4px solid var(--accent);
          border-radius: 8px;
          padding: 14px 16px;
          margin-top: 10px;
          margin-bottom: 15px;
        }

        .dataset-description strong {
          font-size: 14px;
          font-weight: 650;
        }

        .dataset-description p {
          margin-top: 6px;
          margin-bottom: 6px;
          font-size: 13px;
          line-height: 1.5;
        }

        .dataset-description small {
          color: var(--muted);
        }

        label {
          font-weight: 600;
          font-size: 14px;
        }

        .form-control {
          border-radius: 8px;
          border-color: #d6dbe3;
          box-shadow: none;
        }

        .btn {
          font-weight: 600;
          border-radius: 8px;
        }

        pre {
          font-family:
            -apple-system,
            BlinkMacSystemFont,
            'Segoe UI',
            Roboto,
            'Helvetica Neue',
            Arial,
            sans-serif;

          font-size: 14px;
          line-height: 1.65;
          color: var(--text);
          background: transparent;
          border: none;
          padding: 0;
          margin: 0;
          white-space: pre-wrap;
        }

        table.dataTable {
          font-size: 14px;
        }

        table.dataTable thead th {
          font-weight: 650;
        }

        .hero-card {
          background: linear-gradient(135deg, #ffffff 0%, var(--accent-soft) 100%);
          border: 1px solid #dbe7f2;
          border-radius: 12px;
          padding: 20px;
          margin-bottom: 18px;
        }

        .hero-card h2 {
          margin-top: 0;
          color: var(--accent);
        }

        .help-note {
          background: #f9fafb;
          border: 1px solid var(--border);
          border-radius: 8px;
          padding: 11px 13px;
          margin: 10px 0 14px 0;
          color: var(--muted);
          font-size: 13px;
        }

        .figure-guide {
          background: var(--accent-soft);
          border: 1px solid #d7e5f1;
          border-radius: 9px;
          margin-top: 14px;
          margin-bottom: 14px;
          overflow: hidden;
        }

        .figure-guide summary {
          cursor: pointer;
          font-weight: 700;
          color: var(--accent);
          padding: 12px 14px;
        }

        .figure-guide-body {
          padding: 0 14px 12px 14px;
        }

        .figure-guide-body p {
          margin: 7px 0;
          font-size: 13px;
        }

        .figure-limit {
          background: white;
          border-radius: 7px;
          padding: 9px 10px;
          margin-top: 10px !important;
        }

        .status-badge {
          display: inline-block;
          padding: 5px 9px;
          margin: 3px 5px 3px 0;
          border-radius: 999px;
          background: var(--accent-soft);
          border: 1px solid #d7e5f1;
          font-size: 12px;
          font-weight: 650;
          color: var(--accent);
        }


        .metric-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
          gap: 12px;
          margin: 10px 0 16px 0;
        }

        .metric-card {
          background: #ffffff;
          border: 1px solid var(--border);
          border-radius: 10px;
          padding: 14px;
        }

        .metric-value {
          font-size: 23px;
          font-weight: 750;
          color: var(--accent);
          line-height: 1.2;
        }

        .metric-label {
          margin-top: 4px;
          color: var(--muted);
          font-size: 12px;
        }

      ")
      
    )
    
  ),
  
  
  # ==========================================================
  # TAB 1 — UPLOAD & INSPECT
  # ==========================================================
  
  tabPanel(
    
    "Upload & Inspect",
    
    sidebarLayout(
      
      sidebarPanel(
        
        h3("Upload Dataset"),
        
        fileInput(
          "dataset",
          "Choose a dataset",
          accept = c(
            ".csv",
            ".tsv",
            ".txt",
            ".xlsx",
            ".gz",
            ".csv.gz",
            ".tsv.gz",
            ".txt.gz"
          )
        ),
        
        tags$p(
          class = "subtle-text",
          "Supported: CSV, TSV, TXT, XLSX, and gzip-compressed text files."
        ),
        
        hr(),
        
        h4("Optional Sample Metadata"),
        
        fileInput(
          "metadata_file",
          "Choose a metadata file",
          accept = c(
            ".csv", ".tsv", ".txt", ".xlsx", ".gz",
            ".csv.gz", ".tsv.gz", ".txt.gz"
          )
        ),
        
        tags$p(
          class = "subtle-text",
          paste(
            "Optional metadata can contain sample IDs, conditions,",
            "treatments, batches, replicates, or time points."
          )
        ),
        
        hr(),
        
        selectInput(
          "dataset_type",
          "What does this dataset represent?",
          choices = c(
            "Auto-detect only",
            "General numeric data",
            "RNA-seq count matrix",
            "Differential-expression results",
            "Time-series / growth curve",
            "Gene list",
            "Other biological assay"
          ),
          selected = "Auto-detect only"
        ),
        
        uiOutput("dataset_type_description")
        
      ),
      
      
      mainPanel(
        
        div(
          class = "hero-card",
          h2("Explore biological data without losing the biology"),
          tags$p(
            paste(
              "Upload a dataset, inspect its structure, build figures, and use",
              "the explanation boxes beneath each diagram to understand what",
              "the visualization means and what its limitations are."
            )
          ),
          tags$span(class = "status-badge", "General datasets"),
          tags$span(class = "status-badge", "RNA-seq"),
          tags$span(class = "status-badge", "Differential expression"),
          tags$span(class = "status-badge", "Gene search"),
          tags$span(class = "status-badge", "Heatmaps")
        ),
        
        div(
          class = "section-card",
          h2("Dataset Inspector"),
          tags$p(
            class = "subtle-text",
            paste(
              "Upload a dataset to inspect its structure,",
              "quality, and possible analysis options."
            )
          )
        ),
        
        div(
          class = "section-card",
          h3("File Information"),
          verbatimTextOutput("file_info")
        ),
        
        div(
          class = "section-card",
          h3("Dataset Summary"),
          verbatimTextOutput("dataset_summary")
        ),
        
        div(
          class = "section-card",
          h3("Data Quality"),
          verbatimTextOutput("quality_warnings")
        ),
        
        div(
          class = "section-card",
          h3("Column Information"),
          DTOutput("column_info")
        ),
        
        div(
          class = "section-card",
          h3("Dataset Recognition"),
          verbatimTextOutput("dataset_recognition")
        ),
        
        div(
          class = "section-card",
          h3("Possible Analyses"),
          verbatimTextOutput("analysis_recommendations")
        ),
        
        div(
          class = "section-card",
          h3("Dataset Preview"),
          DTOutput("preview")
        )
        
      )
      
    )
    
  ),
  
  
  # ==========================================================
  # TAB 2 — EXPLORE & GRAPH
  # ==========================================================
  
  tabPanel(
    
    "Explore & Graph",
    
    sidebarLayout(
      
      sidebarPanel(
        
        h3("Plot Settings"),
        
        selectInput(
          "plot_type",
          "Plot type",
          choices = c(
            "Scatterplot",
            "Histogram",
            "Boxplot",
            "Line plot"
          )
        ),
        
        selectInput(
          "x_variable",
          "X variable",
          choices = NULL
        ),
        
        conditionalPanel(
          condition = "input.plot_type == 'Scatterplot' || input.plot_type == 'Line plot'",
          
          selectInput(
            "y_variable",
            "Y variable",
            choices = NULL
          )
        ),
        
        selectInput(
          "group_variable",
          "Group / color variable",
          choices = c("None")
        ),
        
        conditionalPanel(
          condition = "input.plot_type == 'Scatterplot'",
          
          selectInput(
            "correlation_method",
            "Correlation method",
            choices = c(
              "Pearson" = "pearson",
              "Spearman" = "spearman"
            ),
            selected = "pearson"
          ),
          
          checkboxInput(
            "show_regression",
            "Show linear regression line",
            value = TRUE
          ),
          
          checkboxInput(
            "log_x",
            "Log10 X axis",
            value = FALSE
          ),
          
          checkboxInput(
            "log_y",
            "Log10 Y axis",
            value = FALSE
          )
        )
        
      ),
      
      
      mainPanel(
        
        div(
          class = "section-card",
          h2("Data Visualization"),
          plotOutput(
            "general_plot",
            height = "520px"
          ),
          uiOutput("general_plot_guide"),
          downloadButton(
            "download_plot",
            "Download Current Plot"
          )
        ),
        
        conditionalPanel(
          condition = "input.plot_type == 'Scatterplot'",
          
          div(
            class = "section-card",
            h3("Scatterplot Statistics"),
            verbatimTextOutput("scatter_statistics")
          )
        ),
        
        div(
          class = "section-card",
          h3("Numeric Summary"),
          DTOutput("numeric_summary")
        )
        
      )
      
    )
    
  ),
  
  
  # ==========================================================
  # TAB 3 — MULTIVARIATE ANALYSIS
  # ==========================================================
  
  tabPanel(
    
    "Multivariate Analysis",
    
    sidebarLayout(
      
      sidebarPanel(
        
        h3("Variables"),
        
        selectizeInput(
          "multi_variables",
          "Choose numeric variables",
          choices = NULL,
          multiple = TRUE
        ),
        
        selectInput(
          "pca_orientation",
          "PCA orientation",
          choices = c(
            "Rows are observations" = "rows",
            "Columns are samples / observations" = "columns"
          ),
          selected = "rows"
        ),
        
        checkboxInput(
          "pca_log1p",
          "Apply log1p transformation before PCA",
          value = FALSE
        ),
        
        tags$p(
          class = "subtle-text",
          paste(
            "For a gene-by-sample count matrix,",
            "choose 'Columns are samples / observations'."
          )
        )
        
      ),
      
      
      mainPanel(
        
        div(
          class = "section-card",
          h2("Principal Component Analysis"),
          plotOutput(
            "pca_plot",
            height = "500px"
          ),
          uiOutput("pca_guide"),
          verbatimTextOutput("pca_information")
        ),
        
        div(
          class = "section-card",
          h2("Correlation Matrix"),
          plotOutput(
            "correlation_plot",
            height = "600px"
          ),
          uiOutput("correlation_guide")
        )
        
      )
      
    )
    
  ),
  
  
  # ==========================================================
  # TAB 4 — RNA-SEQ QC
  # ==========================================================
  
  tabPanel(
    "RNA-seq QC",
    
    sidebarLayout(
      sidebarPanel(
        h3("RNA-seq Settings"),
        selectInput("rna_gene_column", "Gene / feature ID column", choices = NULL),
        selectizeInput(
          "rna_sample_columns",
          "Sample count columns",
          choices = NULL,
          multiple = TRUE
        ),
        numericInput(
          "rna_detect_threshold",
          "Detected-gene threshold",
          value = 1,
          min = 0,
          step = 1
        ),
        checkboxInput(
          "rna_log1p",
          "Use log1p transformation for PCA",
          value = TRUE
        ),
        checkboxInput(
          "rna_label_samples",
          "Label PCA points",
          value = FALSE
        ),
        
        numericInput(
          "rna_top_variable_genes",
          "Variable genes used for clustering",
          value = 1000,
          min = 50,
          max = 5000,
          step = 50
        ),
        
        hr(),
        h4("Optional Metadata Mapping"),
        selectInput("metadata_sample_id", "Metadata sample-ID column", choices = NULL),
        selectInput("metadata_group", "Metadata grouping column", choices = NULL),
        help_note(
          "Sample names in the count matrix should match the selected metadata sample-ID column if you want PCA points grouped by metadata."
        )
      ),
      
      mainPanel(
        div(
          class = "section-card",
          h2("RNA-seq QC Overview"),
          verbatimTextOutput("rna_qc_summary")
        ),
        div(
          class = "section-card",
          h3("Library Size"),
          plotOutput("rna_library_plot", height = "430px"),
          uiOutput("rna_library_guide")
        ),
        div(
          class = "section-card",
          h3("Detected Genes / Features"),
          plotOutput("rna_detected_plot", height = "430px"),
          uiOutput("rna_detected_guide")
        ),
        div(
          class = "section-card",
          h3("Sample Correlation"),
          plotOutput("rna_correlation_plot", height = "650px"),
          uiOutput("rna_correlation_guide")
        ),
        div(
          class = "section-card",
          h3("Sample PCA"),
          plotOutput("rna_pca_plot", height = "520px"),
          uiOutput("rna_pca_guide")
        ),
        div(
          class = "section-card",
          h3("Sample Clustering"),
          plotOutput("rna_dendrogram_plot", height = "520px"),
          uiOutput("rna_dendrogram_guide")
        ),
        div(
          class = "section-card",
          h3("Metadata Match Check"),
          verbatimTextOutput("metadata_match_summary")
        )
      )
    )
  ),
  
  
  # ==========================================================
  # TAB 5 — DIFFERENTIAL EXPRESSION
  # ==========================================================
  
  tabPanel(
    "Differential Expression",
    
    sidebarLayout(
      sidebarPanel(
        h3("Column Mapping"),
        selectInput("de_gene_col", "Gene / feature column", choices = NULL),
        selectInput("de_lfc_col", "Log2 fold-change column", choices = NULL),
        selectInput("de_p_col", "Adjusted p-value / p-value column", choices = NULL),
        selectInput("de_mean_col", "Mean expression column for MA plot", choices = NULL),
        hr(),
        numericInput(
          "de_alpha",
          "P-value / adjusted p-value cutoff",
          value = 0.05,
          min = 0,
          max = 1,
          step = 0.01
        ),
        numericInput(
          "de_lfc_cutoff",
          "Absolute log2 fold-change cutoff",
          value = 1,
          min = 0,
          step = 0.1
        ),
        help_note(
          "Use an adjusted p-value or FDR column when available. BioData Explorer does not retroactively correct raw p-values for multiple testing."
        )
      ),
      
      mainPanel(
        div(
          class = "section-card",
          h2("Differential-expression Summary"),
          verbatimTextOutput("de_summary")
        ),
        div(
          class = "section-card",
          h3("Volcano Plot"),
          plotOutput("de_volcano", height = "560px"),
          uiOutput("de_volcano_guide")
        ),
        div(
          class = "section-card",
          h3("MA Plot"),
          plotOutput("de_ma_plot", height = "540px"),
          uiOutput("de_ma_guide")
        ),
        div(
          class = "section-card",
          h3("Significant Features"),
          DTOutput("de_table"),
          br(),
          downloadButton("download_de", "Download Significant Features")
        )
      )
    )
  ),
  
  
  # ==========================================================
  # TAB 6 — HEATMAP & GENE SEARCH
  # ==========================================================
  
  tabPanel(
    "Heatmap & Gene Search",
    
    fluidRow(
      column(
        width = 4,
        div(
          class = "section-card",
          h3("Heatmap Settings"),
          selectInput("heatmap_gene_col", "Gene / feature ID column", choices = NULL),
          selectizeInput(
            "heatmap_columns",
            "Numeric sample columns",
            choices = NULL,
            multiple = TRUE
          ),
          numericInput(
            "heatmap_top_n",
            "Top variable features",
            value = 30,
            min = 5,
            max = 100,
            step = 5
          ),
          checkboxInput("heatmap_zscore", "Scale each feature to a z-score", TRUE),
          checkboxInput("heatmap_cluster_rows", "Cluster features", TRUE),
          checkboxInput("heatmap_cluster_columns", "Cluster samples", TRUE)
        ),
        div(
          class = "section-card",
          h3("Gene / Feature Search"),
          selectInput("gene_id_column", "Identifier column", choices = NULL),
          textInput(
            "gene_search_text",
            "Search identifier",
            placeholder = "Example: ATF4"
          ),
          selectizeInput(
            "gene_expression_columns",
            "Expression / measurement columns",
            choices = NULL,
            multiple = TRUE
          ),
          tags$p(
            class = "subtle-text",
            "Search is case-insensitive and supports partial matches."
          )
        )
      ),
      
      column(
        width = 8,
        div(
          class = "section-card",
          h2("Feature Heatmap"),
          plotOutput("heatmap_plot", height = "700px"),
          uiOutput("heatmap_guide")
        ),
        div(
          class = "section-card",
          h2("Gene / Feature Search Results"),
          DTOutput("gene_search_table")
        ),
        div(
          class = "section-card",
          h3("Selected Feature Profile"),
          plotOutput("gene_profile_plot", height = "430px"),
          uiOutput("gene_profile_guide")
        )
      )
    )
  ),
  
  
  # ==========================================================
  # TAB 7 — QUALITY DASHBOARD
  # ==========================================================
  
  tabPanel(
    "Quality Dashboard",
    fluidRow(
      column(
        width = 4,
        div(
          class = "section-card",
          h3("Quality Settings"),
          selectInput("quality_outlier_variable", "Numeric variable for outlier screening", choices = NULL),
          numericInput(
            "quality_iqr_multiplier",
            "Outlier IQR multiplier",
            value = 1.5,
            min = 0.5,
            max = 5,
            step = 0.25
          ),
          help_note(
            "Flags are exploratory. BioData Explorer never removes observations automatically."
          )
        )
      ),
      column(
        width = 8,
        div(
          class = "section-card",
          h2("Dataset Quality Dashboard"),
          uiOutput("quality_metric_cards"),
          verbatimTextOutput("quality_interpretation")
        ),
        div(
          class = "section-card",
          h3("Missing Data by Column"),
          plotOutput("missingness_plot", height = "500px"),
          uiOutput("missingness_guide")
        ),
        div(
          class = "section-card",
          h3("Outlier Screening"),
          plotOutput("outlier_plot", height = "430px"),
          verbatimTextOutput("outlier_summary"),
          uiOutput("outlier_guide")
        )
      )
    )
  ),
  
  
  # ==========================================================
  # TAB 8 — TIME SERIES
  # ==========================================================
  
  tabPanel(
    "Time Series",
    sidebarLayout(
      sidebarPanel(
        h3("Time-series Settings"),
        selectInput("time_x", "Time / ordered variable", choices = NULL),
        selectInput("time_y", "Measurement", choices = NULL),
        selectInput("time_group", "Group", choices = c("None")),
        checkboxInput("time_summarize", "Summarize replicates as mean ± SE", value = TRUE),
        help_note(
          "Useful for growth curves, fluorescence trajectories, OD measurements, dose-response time courses, and other ordered experiments."
        )
      ),
      mainPanel(
        div(
          class = "section-card",
          h2("Time-series / Growth Curve"),
          plotOutput("time_plot", height = "520px"),
          uiOutput("time_plot_guide")
        ),
        div(
          class = "section-card",
          h3("Time-series Summary"),
          DTOutput("time_summary_table")
        )
      )
    )
  ),
  
  
  # ==========================================================
  # TAB 9 — GENE SET EXPLORER
  # ==========================================================
  
  tabPanel(
    "Gene Set Explorer",
    sidebarLayout(
      sidebarPanel(
        h3("Gene-set Settings"),
        selectInput("geneset_id_column", "Gene identifier column", choices = NULL),
        selectizeInput(
          "geneset_sample_columns",
          "Expression / measurement columns",
          choices = NULL,
          multiple = TRUE
        ),
        textAreaInput(
          "geneset_text",
          "Paste gene identifiers",
          placeholder = "ATF4\nDDIT3\nASNS\nTRIB3",
          rows = 8
        ),
        help_note(
          "This exploratory score standardizes each matched gene across selected columns and averages the gene-wise z-scores."
        )
      ),
      mainPanel(
        div(
          class = "section-card",
          h2("Gene-set Match Summary"),
          verbatimTextOutput("geneset_summary")
        ),
        div(
          class = "section-card",
          h3("Exploratory Gene-set Score"),
          plotOutput("geneset_plot", height = "460px"),
          uiOutput("geneset_guide")
        )
      )
    )
  ),
  
  
  # ==========================================================
  # TAB 10 — EXPORT
  # ==========================================================
  
  tabPanel(
    
    "Export",
    
    div(
      class = "section-card",
      h2("Export Dataset"),
      tags$p(
        "Download the dataset currently loaded into BioData Explorer."
      ),
      downloadButton(
        "download_data",
        "Download Dataset"
      )
    ),
    
    div(
      class = "section-card",
      h3("Analysis Summary Report"),
      tags$p(
        paste(
          "Download a lightweight HTML summary of the uploaded dataset,",
          "basic quality metrics, selected dataset type, and interpretation guidance."
        )
      ),
      downloadButton(
        "download_report",
        "Download HTML Summary"
      )
    ),
    
    div(
      class = "section-card",
      h3("Dataset Classification"),
      verbatimTextOutput("confirmed_dataset_type")
    ),
    
    div(
      class = "section-card",
      h3("Methods Notes"),
      tags$p(
        class = "subtle-text",
        "Summarizes important current settings for a notebook, report, or methods draft."
      ),
      verbatimTextOutput("methods_notes")
    ),
    
    div(
      class = "section-card",
      h3("Reproducible R Snippet"),
      tags$p(
        class = "subtle-text",
        "A starter script reflecting the current dataset type and major analysis settings."
      ),
      verbatimTextOutput("reproducible_code"),
      downloadButton("download_repro_code", "Download R Snippet")
    )
    
  )
  
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # DATASET-TYPE DESCRIPTION
  # ==========================================================
  
  output$dataset_type_description <- renderUI({
    
    req(input$dataset_type)
    
    description <- switch(
      
      input$dataset_type,
      
      "Auto-detect only" = tagList(
        strong("Automatic dataset inspection"),
        p(
          paste(
            "BioData Explorer will inspect the uploaded file and",
            "suggest possible dataset types and analyses."
          )
        ),
        p(
          paste(
            "No specialized biological assumptions will be made",
            "automatically."
          )
        ),
        tags$small(
          "Recommended when you are unsure what kind of biological table you have."
        )
      ),
      
      "General numeric data" = tagList(
        strong("General numerical dataset"),
        p(
          paste(
            "Use this for numerical measurements that are not",
            "necessarily gene-expression data."
          )
        ),
        p(
          paste(
            "Possible analyses include descriptive statistics,",
            "distributions, scatterplots, correlations, and PCA."
          )
        )
      ),
      
      "RNA-seq count matrix" = tagList(
        strong("RNA-seq raw count matrix"),
        p(
          paste(
            "Use this when rows represent genes or transcripts and",
            "columns represent sequencing samples containing raw integer counts."
          )
        ),
        p(
          paste(
            "Potential analyses include library-size QC, sample correlation,",
            "PCA, clustering, heatmaps, and differential expression."
          )
        ),
        tags$small(
          "Do not select this for TPM, FPKM, or already normalized expression values."
        )
      ),
      
      "Differential-expression results" = tagList(
        strong("Differential-expression result table"),
        p(
          paste(
            "Use this for processed results containing fields such as",
            "log2 fold change, p-values, adjusted p-values, or FDR."
          )
        ),
        p(
          paste(
            "Potential analyses include volcano plots, significant-feature",
            "filtering, up/down-regulated summaries, and pathway analysis."
          )
        )
      ),
      
      "Time-series / growth curve" = tagList(
        strong("Time-series or growth dataset"),
        p(
          "Use this when measurements were collected across ordered time points."
        ),
        p(
          paste(
            "Potential analyses include line plots, growth curves, rate of change,",
            "maximum response, and area under the curve."
          )
        )
      ),
      
      "Gene list" = tagList(
        strong("Gene or transcript list"),
        p(
          paste(
            "Use this when the file primarily contains identifiers such as",
            "gene symbols, Ensembl IDs, Entrez IDs, or transcript IDs."
          )
        ),
        p(
          paste(
            "Potential analyses include identifier annotation, GO enrichment,",
            "pathway enrichment, and gene-set analysis."
          )
        )
      ),
      
      "Other biological assay" = tagList(
        strong("Other biological assay"),
        p(
          paste(
            "Use this for experiments that do not match the other categories,",
            "such as luciferase assays, APEX measurements, fluorescence,",
            "protein quantification, or other laboratory assays."
          )
        ),
        p(
          paste(
            "BioData Explorer will inspect the numerical and categorical",
            "structure and suggest general analyses."
          )
        )
      )
      
    )
    
    div(
      class = "dataset-description",
      description
    )
    
  })
  
  
  # ==========================================================
  # READ UPLOADED DATASET
  # ==========================================================
  
  dataset <- reactive({
    
    req(input$dataset)
    
    filename <- tolower(input$dataset$name)
    filepath <- input$dataset$datapath
    
    data <- tryCatch(
      
      {
        
        if (grepl("\\.csv$", filename)) {
          
          read.csv(
            filepath,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          
        } else if (grepl("\\.tsv$", filename)) {
          
          read.delim(
            filepath,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          
        } else if (grepl("\\.txt$", filename)) {
          
          read.delim(
            filepath,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          
        } else if (grepl("\\.tsv\\.gz$", filename)) {
          
          con <- gzfile(filepath, open = "rt")
          on.exit(close(con), add = TRUE)
          
          read.delim(
            con,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          
        } else if (grepl("\\.csv\\.gz$", filename)) {
          
          con <- gzfile(filepath, open = "rt")
          on.exit(close(con), add = TRUE)
          
          read.csv(
            con,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          
        } else if (
          grepl("\\.txt\\.gz$", filename) ||
          grepl("\\.gz$", filename)
        ) {
          
          con <- gzfile(filepath, open = "rt")
          on.exit(close(con), add = TRUE)
          
          read.delim(
            con,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          
        } else if (grepl("\\.xlsx$", filename)) {
          
          as.data.frame(
            read_excel(filepath)
          )
          
        } else {
          
          stop(
            "Unsupported file format."
          )
          
        }
        
      },
      
      error = function(e) {
        
        validate(
          need(
            FALSE,
            paste(
              "The file could not be read:",
              e$message
            )
          )
        )
        
      }
      
    )
    
    validate(
      need(
        !is.null(data),
        "The dataset could not be read."
      ),
      need(
        nrow(data) > 0,
        "The dataset contains no rows."
      ),
      need(
        ncol(data) > 0,
        "The dataset contains no columns."
      )
    )
    
    data
    
  })
  
  
  # ==========================================================
  # OPTIONAL SAMPLE METADATA
  # ==========================================================
  
  metadata <- reactive({
    if (is.null(input$metadata_file)) {
      return(NULL)
    }
    
    filename <- tolower(input$metadata_file$name)
    filepath <- input$metadata_file$datapath
    
    data <- tryCatch({
      if (grepl("\\.csv$", filename)) {
        read.csv(filepath, stringsAsFactors = FALSE, check.names = FALSE)
      } else if (grepl("\\.tsv$", filename) || grepl("\\.txt$", filename)) {
        read.delim(filepath, stringsAsFactors = FALSE, check.names = FALSE)
      } else if (grepl("\\.csv\\.gz$", filename)) {
        con <- gzfile(filepath, open = "rt")
        on.exit(close(con), add = TRUE)
        read.csv(con, stringsAsFactors = FALSE, check.names = FALSE)
      } else if (grepl("\\.tsv\\.gz$", filename) || grepl("\\.txt\\.gz$", filename) || grepl("\\.gz$", filename)) {
        con <- gzfile(filepath, open = "rt")
        on.exit(close(con), add = TRUE)
        read.delim(con, stringsAsFactors = FALSE, check.names = FALSE)
      } else if (grepl("\\.xlsx$", filename)) {
        as.data.frame(read_excel(filepath))
      } else {
        stop("Unsupported metadata file format.")
      }
    }, error = function(e) {
      validate(need(FALSE, paste("Metadata could not be read:", e$message)))
    })
    
    validate(
      need(!is.null(data), "Metadata could not be read."),
      need(nrow(data) > 0, "Metadata contains no rows."),
      need(ncol(data) > 0, "Metadata contains no columns.")
    )
    
    data
  })
  
  
  # ==========================================================
  # COLUMN HELPERS
  # ==========================================================
  
  numeric_columns <- reactive({
    
    data <- dataset()
    
    numeric_check <- vapply(
      data,
      is.numeric,
      logical(1)
    )
    
    names(data)[numeric_check]
    
  })
  
  
  categorical_columns <- reactive({
    
    data <- dataset()
    
    categorical_check <- vapply(
      data,
      function(x) {
        is.character(x) ||
          is.factor(x) ||
          is.logical(x)
      },
      logical(1)
    )
    
    names(data)[categorical_check]
    
  })
  
  
  # ==========================================================
  # UPDATE VARIABLE MENUS
  # ==========================================================
  
  observeEvent(dataset(), {
    
    numeric_names <- numeric_columns()
    categorical_names <- categorical_columns()
    
    if (length(numeric_names) == 0) {
      
      updateSelectInput(
        session,
        "x_variable",
        choices = character(0)
      )
      
      updateSelectInput(
        session,
        "y_variable",
        choices = character(0)
      )
      
      updateSelectizeInput(
        session,
        "multi_variables",
        choices = character(0),
        selected = character(0)
      )
      
    } else {
      
      updateSelectInput(
        session,
        "x_variable",
        choices = numeric_names,
        selected = numeric_names[1]
      )
      
      y_selected <- if (length(numeric_names) >= 2) {
        numeric_names[2]
      } else {
        numeric_names[1]
      }
      
      updateSelectInput(
        session,
        "y_variable",
        choices = numeric_names,
        selected = y_selected
      )
      
      updateSelectizeInput(
        session,
        "multi_variables",
        choices = numeric_names,
        selected = head(
          numeric_names,
          6
        )
      )
      
    }
    
    # Biological-analysis menus
    all_names <- names(dataset())
    
    gene_guess <- guess_column(
      all_names,
      c("^gene$", "genesymbol", "symbol", "ensembl", "geneid", "feature", "transcript")
    )
    
    if (is.null(gene_guess)) {
      gene_guess <- all_names[1]
    }
    
    lfc_guess <- guess_column(
      numeric_names,
      c("log2foldchange", "log2fc", "logfc", "foldchange")
    )
    
    p_guess <- guess_column(
      numeric_names,
      c("^padj$", "adjustedp", "adjp", "fdr", "qvalue", "pvalue")
    )
    
    gene_choices <- c("None", all_names)
    
    updateSelectInput(session, "rna_gene_column", choices = gene_choices, selected = gene_guess)
    updateSelectInput(session, "de_gene_col", choices = gene_choices, selected = gene_guess)
    updateSelectInput(session, "heatmap_gene_col", choices = gene_choices, selected = gene_guess)
    updateSelectInput(session, "gene_id_column", choices = gene_choices, selected = gene_guess)
    
    updateSelectizeInput(
      session,
      "rna_sample_columns",
      choices = numeric_names,
      selected = numeric_names
    )
    
    updateSelectizeInput(
      session,
      "heatmap_columns",
      choices = numeric_names,
      selected = head(numeric_names, min(12, length(numeric_names)))
    )
    
    updateSelectizeInput(
      session,
      "gene_expression_columns",
      choices = numeric_names,
      selected = head(numeric_names, min(12, length(numeric_names)))
    )
    
    updateSelectInput(
      session,
      "de_lfc_col",
      choices = c("None", numeric_names),
      selected = if (!is.null(lfc_guess)) lfc_guess else "None"
    )
    
    updateSelectInput(
      session,
      "de_p_col",
      choices = c("None", numeric_names),
      selected = if (!is.null(p_guess)) p_guess else "None"
    )
    
    updateSelectInput(
      session,
      "group_variable",
      choices = c(
        "None",
        categorical_names
      ),
      selected = "None"
    )
    
  })
  
  
  # ==========================================================
  # UPDATE METADATA MENUS
  # ==========================================================
  
  observeEvent(metadata(), {
    md <- metadata()
    
    if (is.null(md)) {
      updateSelectInput(session, "metadata_sample_id", choices = character(0))
      updateSelectInput(session, "metadata_group", choices = character(0))
    } else {
      md_names <- names(md)
      
      sample_guess <- guess_column(
        md_names,
        c("samplename", "sampleid", "^sample$", "run", "accession")
      )
      
      group_guess <- guess_column(
        md_names,
        c("condition", "treatment", "group", "phenotype", "batch")
      )
      
      updateSelectInput(
        session,
        "metadata_sample_id",
        choices = md_names,
        selected = if (!is.null(sample_guess)) sample_guess else md_names[1]
      )
      
      updateSelectInput(
        session,
        "metadata_group",
        choices = c("None", md_names),
        selected = if (!is.null(group_guess)) group_guess else "None"
      )
    }
  })
  
  
  # ==========================================================
  # UPDATE ENHANCED ANALYSIS MENUS
  # ==========================================================
  
  observeEvent(dataset(), {
    data <- dataset()
    all_names <- names(data)
    numeric_names <- numeric_columns()
    categorical_names <- categorical_columns()
    
    gene_guess <- guess_column(
      all_names,
      c("^gene$", "genesymbol", "symbol", "ensembl", "geneid", "feature", "transcript")
    )
    
    if (is.null(gene_guess)) {
      gene_guess <- all_names[1]
    }
    
    mean_guess <- guess_column(
      numeric_names,
      c("^basemean$", "meanexpression", "meanexpr", "avgexpression", "averageexpression", "^mean$")
    )
    
    time_guess <- guess_column(
      all_names,
      c("^time$", "timepoint", "hour", "minute", "day")
    )
    
    updateSelectInput(
      session,
      "de_mean_col",
      choices = c("None", numeric_names),
      selected = if (!is.null(mean_guess)) mean_guess else "None"
    )
    
    updateSelectInput(
      session,
      "quality_outlier_variable",
      choices = numeric_names,
      selected = if (length(numeric_names) > 0) numeric_names[1] else character(0)
    )
    
    updateSelectInput(
      session,
      "time_x",
      choices = all_names,
      selected = if (!is.null(time_guess)) time_guess else all_names[1]
    )
    
    updateSelectInput(
      session,
      "time_y",
      choices = numeric_names,
      selected = if (length(numeric_names) >= 2) {
        numeric_names[2]
      } else if (length(numeric_names) == 1) {
        numeric_names[1]
      } else {
        character(0)
      }
    )
    
    updateSelectInput(
      session,
      "time_group",
      choices = c("None", categorical_names),
      selected = "None"
    )
    
    updateSelectInput(
      session,
      "geneset_id_column",
      choices = c("None", all_names),
      selected = gene_guess
    )
    
    updateSelectizeInput(
      session,
      "geneset_sample_columns",
      choices = numeric_names,
      selected = head(numeric_names, min(12, length(numeric_names)))
    )
  })
  
  
  # ==========================================================
  # FILE INFORMATION
  # ==========================================================
  
  output$file_info <- renderText({
    
    req(input$dataset)
    
    size_mb <- input$dataset$size / 1024^2
    
    paste0(
      "File: ",
      input$dataset$name,
      "\nSize: ",
      round(size_mb, 3),
      " MB",
      "\nFormat: ",
      toupper(
        tools::file_ext(
          input$dataset$name
        )
      )
    )
    
  })
  
  
  # ==========================================================
  # DATASET SUMMARY
  # ==========================================================
  
  output$dataset_summary <- renderText({
    
    data <- dataset()
    
    numeric_count <- sum(
      vapply(
        data,
        is.numeric,
        logical(1)
      )
    )
    
    text_count <- sum(
      vapply(
        data,
        is.character,
        logical(1)
      )
    )
    
    logical_count <- sum(
      vapply(
        data,
        is.logical,
        logical(1)
      )
    )
    
    paste0(
      "Rows: ",
      format(
        nrow(data),
        big.mark = ","
      ),
      "\nColumns: ",
      ncol(data),
      "\n\nNumeric columns: ",
      numeric_count,
      "\nText columns: ",
      text_count,
      "\nLogical columns: ",
      logical_count,
      "\n\nMissing values: ",
      format(
        sum(is.na(data)),
        big.mark = ","
      ),
      "\nDuplicate rows: ",
      sum(
        duplicated(data)
      )
    )
    
  })
  
  
  # ==========================================================
  # DATA QUALITY
  # ==========================================================
  
  output$quality_warnings <- renderText({
    
    data <- dataset()
    warnings <- c()
    
    missing_count <- sum(
      is.na(data)
    )
    
    if (missing_count > 0) {
      
      warnings <- c(
        warnings,
        paste(
          "Warning:",
          missing_count,
          "missing values detected."
        )
      )
      
    }
    
    duplicate_count <- sum(
      duplicated(data)
    )
    
    if (duplicate_count > 0) {
      
      warnings <- c(
        warnings,
        paste(
          "Warning:",
          duplicate_count,
          "duplicate rows detected."
        )
      )
      
    }
    
    if (anyDuplicated(names(data)) > 0) {
      
      warnings <- c(
        warnings,
        "Warning: duplicate column names detected."
      )
      
    }
    
    constant_columns <- names(data)[
      
      vapply(
        data,
        function(x) {
          length(
            unique(
              x[!is.na(x)]
            )
          ) <= 1
        },
        logical(1)
      )
      
    ]
    
    if (length(constant_columns) > 0) {
      
      warnings <- c(
        warnings,
        paste(
          "Warning: constant columns:",
          paste(
            constant_columns,
            collapse = ", "
          )
        )
      )
      
    }
    
    numeric_names <- numeric_columns()
    
    if (length(numeric_names) > 0) {
      
      numeric_values <- unlist(
        data[numeric_names],
        use.names = FALSE
      )
      
      negative_count <- sum(
        numeric_values < 0,
        na.rm = TRUE
      )
      
      if (negative_count > 0) {
        
        warnings <- c(
          warnings,
          paste(
            "Note:",
            negative_count,
            "negative numeric values detected."
          )
        )
        
      }
      
    }
    
    if (length(warnings) == 0) {
      
      warnings <- c(
        "No obvious structural data-quality problems detected.",
        "This does not guarantee biological or statistical validity."
      )
      
    }
    
    paste(
      warnings,
      collapse = "\n"
    )
    
  })
  
  
  # ==========================================================
  # COLUMN INFORMATION
  # ==========================================================
  
  output$column_info <- renderDT({
    
    data <- dataset()
    
    column_table <- data.frame(
      
      Column = names(data),
      
      Type = vapply(
        data,
        function(x) {
          class(x)[1]
        },
        character(1)
      ),
      
      Missing = vapply(
        data,
        function(x) {
          sum(
            is.na(x)
          )
        },
        numeric(1)
      ),
      
      Unique_Values = vapply(
        data,
        function(x) {
          length(
            unique(x)
          )
        },
        numeric(1)
      ),
      
      stringsAsFactors = FALSE
      
    )
    
    datatable(
      column_table,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
    
  })
  
  
  # ==========================================================
  # DATASET RECOGNITION
  # ==========================================================
  
  output$dataset_recognition <- renderText({
    
    data <- dataset()
    
    normalized_names <- gsub(
      "[^a-z0-9]",
      "",
      tolower(
        names(data)
      )
    )
    
    numeric_names <- numeric_columns()
    number_numeric <- length(numeric_names)
    
    recognition <- c()
    
    has_gene <- any(
      grepl(
        "gene|symbol|ensembl|feature|transcript",
        normalized_names
      )
    )
    
    if (has_gene) {
      
      recognition <- c(
        recognition,
        "Possible gene or feature identifier column detected."
      )
      
    }
    
    has_time <- any(
      grepl(
        "time|timepoint|hour|minute|day",
        normalized_names
      )
    )
    
    if (has_time) {
      
      recognition <- c(
        recognition,
        "Possible time-series variable detected."
      )
      
    }
    
    has_fold_change <- any(
      grepl(
        "log2foldchange|log2fc|foldchange",
        normalized_names
      )
    )
    
    has_pvalue <- any(
      grepl(
        "pvalue|padj|adjustedp|fdr",
        normalized_names
      )
    )
    
    if (
      has_fold_change &&
      has_pvalue
    ) {
      
      recognition <- c(
        recognition,
        "Possible differential-expression result table detected."
      )
      
    }
    
    count_like <- FALSE
    
    if (number_numeric > 0) {
      
      values <- unlist(
        data[numeric_names],
        use.names = FALSE
      )
      
      values <- values[
        is.finite(values)
      ]
      
      if (length(values) > 0) {
        
        non_negative <- all(
          values >= 0
        )
        
        mostly_integer <- mean(
          abs(
            values -
              round(values)
          ) < 0.000001
        ) >= 0.95
        
        count_like <- non_negative &&
          mostly_integer
        
      }
      
    }
    
    if (
      has_gene &&
      count_like &&
      number_numeric >= 2
    ) {
      
      recognition <- c(
        recognition,
        "Dataset may contain biological count-like measurements."
      )
      
    }
    
    if (number_numeric >= 1) {
      
      recognition <- c(
        recognition,
        paste(
          number_numeric,
          "numeric variable(s) detected."
        )
      )
      
    }
    
    if (length(recognition) == 0) {
      
      recognition <- c(
        "No specific biological dataset structure was confidently recognized."
      )
      
    }
    
    paste(
      recognition,
      collapse = "\n"
    )
    
  })
  
  
  # ==========================================================
  # ANALYSIS RECOMMENDATIONS
  # ==========================================================
  
  output$analysis_recommendations <- renderText({
    
    data <- dataset()
    
    normalized_names <- gsub(
      "[^a-z0-9]",
      "",
      tolower(
        names(data)
      )
    )
    
    number_numeric <- length(
      numeric_columns()
    )
    
    recommendations <- c()
    
    if (number_numeric >= 1) {
      
      recommendations <- c(
        recommendations,
        "Descriptive statistics",
        "Distribution analysis",
        "Histogram",
        "Boxplot"
      )
      
    }
    
    if (number_numeric >= 2) {
      
      recommendations <- c(
        recommendations,
        "Scatterplot",
        "Correlation analysis",
        "Missing-data and exploratory outlier dashboard"
      )
      
    }
    
    if (number_numeric >= 3) {
      
      recommendations <- c(
        recommendations,
        "Principal component analysis (PCA)",
        "Clustering may be possible",
        "Heatmap visualization may be possible"
      )
      
    }
    
    has_time <- any(
      grepl(
        "time|timepoint|hour|minute|day",
        normalized_names
      )
    )
    
    if (
      has_time ||
      input$dataset_type ==
      "Time-series / growth curve"
    ) {
      
      recommendations <- c(
        recommendations,
        "Time-series / line-plot analysis",
        "Rate-of-change analysis may be possible",
        "Area-under-the-curve analysis may be possible"
      )
      
    }
    
    has_gene <- any(
      grepl(
        "gene|symbol|ensembl|feature|transcript",
        normalized_names
      )
    )
    
    if (
      has_gene ||
      input$dataset_type %in% c(
        "Gene list",
        "RNA-seq count matrix",
        "Differential-expression results"
      )
    ) {
      
      recommendations <- c(
        recommendations,
        "Gene annotation may be possible after confirming identifier type",
        "GO/pathway analysis may be possible after confirming organism"
      )
      
    }
    
    has_fold_change <- any(
      grepl(
        "log2foldchange|log2fc|foldchange",
        normalized_names
      )
    )
    
    has_pvalue <- any(
      grepl(
        "pvalue|padj|adjustedp|fdr",
        normalized_names
      )
    )
    
    if (
      (
        has_fold_change &&
        has_pvalue
      ) ||
      input$dataset_type ==
      "Differential-expression results"
    ) {
      
      recommendations <- c(
        recommendations,
        "Volcano plot",
        "MA plot when mean-expression values are available",
        "Significant-feature filtering",
        "Upregulated/downregulated feature analysis"
      )
      
    }
    
    if (
      input$dataset_type ==
      "RNA-seq count matrix"
    ) {
      
      recommendations <- c(
        recommendations,
        "Sample library-size comparison",
        "Sample correlation",
        "Sample PCA",
        "Sample clustering",
        "Metadata sample-name validation",
        "Differential expression may be possible after defining sample groups"
      )
      
    }
    
    if (length(recommendations) == 0) {
      
      recommendations <- c(
        "No quantitative analyses can currently be recommended."
      )
      
    }
    
    paste(
      unique(recommendations),
      collapse = "\n"
    )
    
  })
  
  
  # ==========================================================
  # DATASET PREVIEW
  # ==========================================================
  
  output$preview <- renderDT({
    
    data <- dataset()
    
    datatable(
      data,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
    
  })
  
  
  # ==========================================================
  # NUMERIC SUMMARY
  # ==========================================================
  
  output$numeric_summary <- renderDT({
    
    data <- dataset()
    numeric_names <- numeric_columns()
    
    validate(
      need(
        length(numeric_names) > 0,
        "No numeric variables were detected."
      )
    )
    
    summary_list <- lapply(
      
      numeric_names,
      
      function(variable) {
        
        x <- data[, variable, drop = TRUE]
        
        valid_x <- x[
          is.finite(x)
        ]
        
        if (length(valid_x) == 0) {
          
          return(
            data.frame(
              Variable = variable,
              N = 0,
              Missing = length(x),
              Mean = NA,
              SD = NA,
              Median = NA,
              Minimum = NA,
              Maximum = NA
            )
          )
          
        }
        
        data.frame(
          Variable = variable,
          N = length(valid_x),
          Missing = sum(
            is.na(x)
          ),
          Mean = mean(valid_x),
          SD = if (
            length(valid_x) > 1
          ) {
            sd(valid_x)
          } else {
            NA
          },
          Median = median(valid_x),
          Minimum = min(valid_x),
          Maximum = max(valid_x)
        )
        
      }
      
    )
    
    summary_table <- do.call(
      rbind,
      summary_list
    )
    
    datatable(
      summary_table,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
    
  })
  
  
  # ==========================================================
  # SCATTERPLOT STATISTICS
  # ==========================================================
  
  output$scatter_statistics <- renderText({
    
    req(
      input$x_variable,
      input$y_variable
    )
    
    data <- dataset()
    
    x <- data[, input$x_variable, drop = TRUE]
    y <- data[, input$y_variable, drop = TRUE]
    
    complete <- complete.cases(
      x,
      y
    )
    
    x_clean <- x[complete]
    y_clean <- y[complete]
    
    validate(
      need(
        length(x_clean) >= 3,
        "At least three complete observations are required."
      )
    )
    
    validate(
      need(
        length(unique(x_clean)) >= 2,
        "The X variable must contain at least two unique values."
      ),
      need(
        length(unique(y_clean)) >= 2,
        "The Y variable must contain at least two unique values."
      )
    )
    
    method <- input$correlation_method
    
    cor_test <- suppressWarnings(
      cor.test(
        x_clean,
        y_clean,
        method = method
      )
    )
    
    model <- lm(
      y_clean ~ x_clean
    )
    
    model_summary <- summary(model)
    
    correlation <- unname(
      cor_test$estimate
    )
    
    r_squared <- model_summary$r.squared
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    regression_p <- coef(model_summary)[2, 4]
    
    total_rows <- length(x)
    used_rows <- length(x_clean)
    removed_rows <- total_rows -
      used_rows
    
    abs_r <- abs(correlation)
    
    strength <- if (
      abs_r < 0.3
    ) {
      "weak"
    } else if (
      abs_r < 0.7
    ) {
      "moderate"
    } else {
      "strong"
    }
    
    direction <- if (
      correlation > 0
    ) {
      "positive"
    } else if (
      correlation < 0
    ) {
      "negative"
    } else {
      "no"
    }
    
    coefficient_name <- if (
      method == "pearson"
    ) {
      "Pearson r"
    } else {
      "Spearman rho"
    }
    
    paste0(
      "X variable: ",
      input$x_variable,
      "\nY variable: ",
      input$y_variable,
      
      "\n\nObservations used: ",
      used_rows,
      
      "\nIncomplete observations removed: ",
      removed_rows,
      
      "\n\nCorrelation method: ",
      tools::toTitleCase(method),
      
      "\n",
      coefficient_name,
      ": ",
      round(correlation, 4),
      
      "\nCorrelation p-value: ",
      format.pval(
        cor_test$p.value,
        digits = 4
      ),
      
      "\n\nLinear regression slope: ",
      round(slope, 4),
      
      "\nLinear regression intercept: ",
      round(intercept, 4),
      
      "\nR-squared: ",
      round(r_squared, 4),
      
      "\nRegression slope p-value: ",
      format.pval(
        regression_p,
        digits = 4
      ),
      
      "\n\nInterpretation: ",
      tools::toTitleCase(strength),
      " ",
      direction,
      " association between the selected variables.",
      
      "\n\nCorrelation describes association and does not by itself establish causation."
    )
    
  })
  
  
  # ==========================================================
  # GENERAL PLOT BUILDER
  # ==========================================================
  
  current_plot <- reactive({
    
    data <- dataset()
    
    req(input$x_variable)
    
    validate(
      need(
        input$x_variable %in%
          names(data),
        "Select a valid X variable."
      )
    )
    
    group_name <- input$group_variable
    
    use_group <- !is.null(group_name) &&
      group_name != "None" &&
      group_name %in%
      names(data)
    
    
    # --------------------------------------------------------
    # HISTOGRAM
    # --------------------------------------------------------
    
    if (
      input$plot_type ==
      "Histogram"
    ) {
      
      plot_data <- data.frame(
        X = data[, input$x_variable, drop = TRUE]
      )
      
      p <- ggplot(
        plot_data,
        aes(x = X)
      ) +
        geom_histogram(
          bins = 30
        ) +
        labs(
          title = paste(
            "Distribution of",
            input$x_variable
          ),
          x = input$x_variable,
          y = "Count"
        ) +
        theme_minimal(
          base_family = "sans"
        )
      
      
      # --------------------------------------------------------
      # BOXPLOT
      # --------------------------------------------------------
      
    } else if (
      input$plot_type ==
      "Boxplot"
    ) {
      
      if (use_group) {
        
        plot_data <- data.frame(
          X = data[, input$x_variable, drop = TRUE],
          Group = as.factor(
            data[, group_name, drop = TRUE]
          )
        )
        
        p <- ggplot(
          plot_data,
          aes(
            x = Group,
            y = X
          )
        ) +
          geom_boxplot() +
          labs(
            title = paste(
              input$x_variable,
              "by",
              group_name
            ),
            x = group_name,
            y = input$x_variable
          ) +
          theme_minimal(
            base_family = "sans"
          ) +
          theme(
            axis.text.x = element_text(
              angle = 45,
              hjust = 1
            )
          )
        
      } else {
        
        plot_data <- data.frame(
          X = data[, input$x_variable, drop = TRUE]
        )
        
        p <- ggplot(
          plot_data,
          aes(
            x = "",
            y = X
          )
        ) +
          geom_boxplot() +
          labs(
            title = paste(
              "Boxplot of",
              input$x_variable
            ),
            x = NULL,
            y = input$x_variable
          ) +
          theme_minimal(
            base_family = "sans"
          )
        
      }
      
      
      # --------------------------------------------------------
      # LINE PLOT
      # --------------------------------------------------------
      
    } else if (
      input$plot_type ==
      "Line plot"
    ) {
      
      req(input$y_variable)
      
      plot_data <- data.frame(
        X = data[, input$x_variable, drop = TRUE],
        Y = data[, input$y_variable, drop = TRUE]
      )
      
      if (use_group) {
        
        plot_data$Group <- as.factor(
          data[, group_name, drop = TRUE]
        )
        
        p <- ggplot(
          plot_data,
          aes(
            x = X,
            y = Y,
            group = Group,
            linetype = Group
          )
        ) +
          geom_line(
            linewidth = 0.8
          ) +
          geom_point(
            size = 2
          )
        
      } else {
        
        plot_data <- plot_data[
          order(plot_data$X),
          ,
          drop = FALSE
        ]
        
        p <- ggplot(
          plot_data,
          aes(
            x = X,
            y = Y
          )
        ) +
          geom_line(
            linewidth = 0.8
          ) +
          geom_point(
            size = 2
          )
        
      }
      
      p <- p +
        labs(
          title = paste(
            input$y_variable,
            "over",
            input$x_variable
          ),
          x = input$x_variable,
          y = input$y_variable
        ) +
        theme_minimal(
          base_family = "sans"
        )
      
      
      # --------------------------------------------------------
      # SCATTERPLOT
      # --------------------------------------------------------
      
    } else {
      
      req(input$y_variable)
      
      validate(
        need(
          input$y_variable %in%
            names(data),
          "Select a valid Y variable."
        )
      )
      
      plot_data <- data.frame(
        X = data[, input$x_variable, drop = TRUE],
        Y = data[, input$y_variable, drop = TRUE]
      )
      
      if (use_group) {
        
        plot_data$Group <- as.factor(
          data[, group_name, drop = TRUE]
        )
        
        p <- ggplot(
          plot_data,
          aes(
            x = X,
            y = Y,
            shape = Group
          )
        ) +
          geom_point(
            alpha = 0.65,
            size = 2
          )
        
      } else {
        
        p <- ggplot(
          plot_data,
          aes(
            x = X,
            y = Y
          )
        ) +
          geom_point(
            alpha = 0.65,
            size = 2
          )
        
      }
      
      if (
        isTRUE(
          input$show_regression
        )
      ) {
        
        p <- p +
          geom_smooth(
            method = "lm",
            se = TRUE
          )
        
      }
      
      if (
        isTRUE(
          input$log_x
        )
      ) {
        
        p <- p +
          scale_x_log10()
        
      }
      
      if (
        isTRUE(
          input$log_y
        )
      ) {
        
        p <- p +
          scale_y_log10()
        
      }
      
      p <- p +
        labs(
          title = paste(
            input$y_variable,
            "vs",
            input$x_variable
          ),
          subtitle = paste(
            sum(
              complete.cases(
                plot_data[, c("X", "Y")]
              )
            ),
            "complete observations"
          ),
          x = input$x_variable,
          y = input$y_variable
        ) +
        theme_minimal(
          base_family = "sans"
        )
      
    }
    
    p
    
  })
  
  
  output$general_plot <- renderPlot({
    
    current_plot()
    
  })
  
  
  # ==========================================================
  # FIGURE EXPLANATIONS
  # ==========================================================
  
  output$general_plot_guide <- renderUI({
    req(input$plot_type)
    
    if (input$plot_type == "Scatterplot") {
      figure_guide(
        what = "A scatterplot compares two numerical variables and shows how individual observations are distributed relative to one another.",
        how = "Each point is one observation. The X variable is shown horizontally and the Y variable vertically. If enabled, the fitted line summarizes the linear trend.",
        look_for = "Look for a positive or negative trend, clusters, unusual observations, curved relationships, or differences between groups.",
        limitation = "Correlation and regression describe association. They do not establish causation, and a linear fit can miss non-linear relationships."
      )
    } else if (input$plot_type == "Histogram") {
      figure_guide(
        what = "A histogram shows the distribution of one numerical variable by dividing its values into bins.",
        how = "The X-axis shows ranges of the selected variable and the Y-axis shows how many observations fall within each range.",
        look_for = "Look for the center, spread, skew, multiple peaks, gaps, and unusually extreme values.",
        limitation = "The apparent distribution can change depending on the number and width of bins."
      )
    } else if (input$plot_type == "Boxplot") {
      figure_guide(
        what = "A boxplot summarizes the distribution of a numerical variable and can compare that distribution across groups.",
        how = "The center line is the median, the box contains the middle 50% of values, and the whiskers extend toward the broader data range.",
        look_for = "Compare medians, spread, overlap between groups, and isolated observations.",
        limitation = "A boxplot is a summary and can hide details such as multimodal structure or very small sample sizes."
      )
    } else {
      figure_guide(
        what = "A line plot shows how a numerical measurement changes across an ordered X variable such as time, dose, or experimental progression.",
        how = "Points show measured values and connecting lines make the direction of change easier to follow.",
        look_for = "Look for increases, decreases, plateaus, peaks, group-specific patterns, and abrupt changes.",
        limitation = "Connecting points implies an order. A line plot should not be used when X-axis categories have no meaningful sequence."
      )
    }
  })
  
  output$pca_guide <- renderUI({
    observation_text <- if (input$pca_orientation == "columns") {
      "Each point represents one selected column after the matrix is transposed."
    } else {
      "Each point represents one dataset row."
    }
    
    figure_guide(
      what = paste("Principal component analysis summarizes variation across multiple numerical variables.", observation_text),
      how = "Points that are close together have more similar multivariable profiles. PC1 captures the largest source of variation, followed by PC2.",
      look_for = "Look for clusters, gradients, separated observations, and potential outliers.",
      limitation = "PCA depends on which variables were included and how the data were transformed or scaled. It is descriptive rather than causal."
    )
  })
  
  output$correlation_guide <- renderUI({
    figure_guide(
      what = "The correlation matrix summarizes pairwise linear relationships between the selected numerical variables.",
      how = "Every square compares two variables. Correlations near +1 are strongly positive, values near -1 are strongly negative, and values near 0 show little linear association.",
      look_for = "Look for groups of variables with consistently high correlations, strong inverse relationships, and variables that behave differently from the rest.",
      limitation = "Correlation does not establish causation and can be strongly affected by outliers or non-linear relationships."
    )
  })
  
  
  # ==========================================================
  # DOWNLOAD CURRENT PLOT
  # ==========================================================
  
  output$download_plot <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "BioDataExplorer_",
        gsub(
          "[^A-Za-z0-9]+",
          "_",
          input$plot_type
        ),
        "_",
        Sys.Date(),
        ".png"
      )
      
    },
    
    content = function(file) {
      
      ggsave(
        filename = file,
        plot = current_plot(),
        width = 8,
        height = 6,
        dpi = 300
      )
      
    }
    
  )
  
  
  # ==========================================================
  # PCA CALCULATION
  # ==========================================================
  
  pca_results <- reactive({
    
    data <- dataset()
    selected <- input$multi_variables
    
    validate(
      need(
        length(selected) >= 2,
        "Choose at least two numeric variables."
      )
    )
    
    pca_data <- data[selected]
    
    pca_data <- pca_data[
      complete.cases(pca_data),
      ,
      drop = FALSE
    ]
    
    if (
      isTRUE(
        input$pca_log1p
      )
    ) {
      
      validate(
        need(
          all(
            as.matrix(pca_data) >= 0,
            na.rm = TRUE
          ),
          "log1p PCA requires non-negative values."
        )
      )
      
      pca_data <- log1p(
        pca_data
      )
      
    }
    
    if (
      input$pca_orientation ==
      "columns"
    ) {
      
      pca_matrix <- t(
        as.matrix(
          pca_data
        )
      )
      
    } else {
      
      pca_matrix <- as.matrix(
        pca_data
      )
      
    }
    
    validate(
      need(
        nrow(pca_matrix) >= 3,
        "At least three observations are required for PCA."
      ),
      need(
        ncol(pca_matrix) >= 2,
        "At least two variables are required for PCA."
      )
    )
    
    variable_sd <- apply(
      pca_matrix,
      2,
      sd,
      na.rm = TRUE
    )
    
    keep <- is.finite(variable_sd) &
      variable_sd > 0
    
    pca_matrix <- pca_matrix[
      ,
      keep,
      drop = FALSE
    ]
    
    validate(
      need(
        ncol(pca_matrix) >= 2,
        "At least two non-constant variables are required for PCA."
      )
    )
    
    prcomp(
      pca_matrix,
      center = TRUE,
      scale. = TRUE
    )
    
  })
  
  
  # ==========================================================
  # PCA PLOT
  # ==========================================================
  
  output$pca_plot <- renderPlot({
    
    result <- pca_results()
    
    scores <- as.data.frame(
      result$x
    )
    
    validate(
      need(
        all(
          c("PC1", "PC2") %in%
            names(scores)
        ),
        "PC1 and PC2 could not be calculated."
      )
    )
    
    variance <- result$sdev^2
    
    variance_percent <- variance /
      sum(variance) *
      100
    
    ggplot(
      scores,
      aes(
        x = PC1,
        y = PC2
      )
    ) +
      geom_point(
        size = 3,
        alpha = 0.7
      ) +
      labs(
        title = "Principal Component Analysis",
        x = paste0(
          "PC1 (",
          round(
            variance_percent[1],
            1
          ),
          "%)"
        ),
        y = paste0(
          "PC2 (",
          round(
            variance_percent[2],
            1
          ),
          "%)"
        )
      ) +
      theme_minimal(
        base_family = "sans"
      )
    
  })
  
  
  # ==========================================================
  # PCA INFORMATION
  # ==========================================================
  
  output$pca_information <- renderText({
    
    selected <- input$multi_variables
    
    if (
      length(selected) < 2
    ) {
      
      return(
        "Choose at least two numeric variables."
      )
      
    }
    
    orientation_text <- if (
      input$pca_orientation ==
      "columns"
    ) {
      
      "Selected columns are treated as observations/samples."
      
    } else {
      
      "Dataset rows are treated as observations."
      
    }
    
    transform_text <- if (
      isTRUE(
        input$pca_log1p
      )
    ) {
      
      "log1p transformation: ON"
      
    } else {
      
      "log1p transformation: OFF"
      
    }
    
    paste0(
      orientation_text,
      "\n",
      transform_text,
      "\n\nSelected numeric columns:\n",
      paste(
        selected,
        collapse = "\n"
      )
    )
    
  })
  
  
  # ==========================================================
  # CORRELATION MATRIX
  # ==========================================================
  
  output$correlation_plot <- renderPlot({
    
    data <- dataset()
    selected <- input$multi_variables
    
    validate(
      need(
        length(selected) >= 2,
        "Choose at least two numeric variables."
      )
    )
    
    correlation_data <- data[selected]
    
    correlation_matrix <- cor(
      correlation_data,
      use = "pairwise.complete.obs"
    )
    
    correlation_df <- as.data.frame(
      as.table(
        correlation_matrix
      )
    )
    
    names(
      correlation_df
    ) <- c(
      "Variable1",
      "Variable2",
      "Correlation"
    )
    
    ggplot(
      correlation_df,
      aes(
        x = Variable1,
        y = Variable2,
        fill = Correlation
      )
    ) +
      geom_tile() +
      geom_text(
        aes(
          label = round(
            Correlation,
            2
          )
        ),
        na.rm = TRUE
      ) +
      labs(
        title = "Correlation Matrix",
        x = NULL,
        y = NULL
      ) +
      theme_minimal(
        base_family = "sans"
      ) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
  })
  
  
  # ==========================================================
  # RNA-SEQ QC
  # ==========================================================
  
  rna_matrix <- reactive({
    data <- dataset()
    selected <- input$rna_sample_columns
    
    validate(need(length(selected) >= 2, "Choose at least two numeric sample columns."))
    
    mat <- as.matrix(data[, selected, drop = FALSE])
    storage.mode(mat) <- "numeric"
    mat
  })
  
  output$rna_qc_summary <- renderText({
    mat <- rna_matrix()
    library_sizes <- colSums(mat, na.rm = TRUE)
    detected <- colSums(mat >= input$rna_detect_threshold, na.rm = TRUE)
    
    paste0(
      "Samples selected: ", ncol(mat),
      "\nFeatures / genes: ", nrow(mat),
      "\n\nLibrary-size range: ",
      format(round(min(library_sizes, na.rm = TRUE)), big.mark = ","),
      " to ",
      format(round(max(library_sizes, na.rm = TRUE)), big.mark = ","),
      "\nDetected-feature range at threshold ≥ ", input$rna_detect_threshold, ": ",
      min(detected, na.rm = TRUE), " to ", max(detected, na.rm = TRUE),
      "\n\nInterpret these exploratory QC summaries together with sample metadata and the experimental design."
    )
  })
  
  output$rna_library_plot <- renderPlot({
    mat <- rna_matrix()
    d <- data.frame(
      Sample = colnames(mat),
      Library_Size = colSums(mat, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    
    ggplot(d, aes(x = reorder(Sample, Library_Size), y = Library_Size)) +
      geom_col() +
      coord_flip() +
      labs(title = "RNA-seq Library Size", x = "Sample", y = "Total counts") +
      theme_minimal(base_family = "sans")
  })
  
  output$rna_library_guide <- renderUI({
    figure_guide(
      what = "Library size is the total number of counts assigned to all measured features in each selected sample.",
      how = "Each bar represents one sample. Longer bars indicate a larger total count depth.",
      look_for = "Look for samples with much larger or smaller totals than the rest. Large differences are one reason RNA-seq workflows use normalization.",
      limitation = "Library size alone does not determine sample quality. A lower library size is not automatically a failed sample."
    )
  })
  
  output$rna_detected_plot <- renderPlot({
    mat <- rna_matrix()
    detected <- colSums(mat >= input$rna_detect_threshold, na.rm = TRUE)
    d <- data.frame(
      Sample = names(detected),
      Detected = as.numeric(detected),
      stringsAsFactors = FALSE
    )
    
    ggplot(d, aes(x = reorder(Sample, Detected), y = Detected)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Detected Features per Sample",
        subtitle = paste("Count threshold ≥", input$rna_detect_threshold),
        x = "Sample",
        y = "Detected features"
      ) +
      theme_minimal(base_family = "sans")
  })
  
  output$rna_detected_guide <- renderUI({
    figure_guide(
      what = "This plot counts how many genes or features reach the selected minimum count in each sample.",
      how = "Each bar is one sample. A feature is counted as detected when its value reaches the threshold shown above the plot.",
      look_for = "Samples with substantially fewer detected genes than comparable samples may deserve additional QC review.",
      limitation = "The result depends on the chosen threshold and does not by itself identify the cause of a low detected-gene count."
    )
  })
  
  output$rna_correlation_plot <- renderPlot({
    mat <- rna_matrix()
    cor_mat <- cor(mat, use = "pairwise.complete.obs", method = "spearman")
    cor_df <- as.data.frame(as.table(cor_mat))
    names(cor_df) <- c("Sample1", "Sample2", "Correlation")
    
    ggplot(cor_df, aes(x = Sample1, y = Sample2, fill = Correlation)) +
      geom_tile() +
      labs(
        title = "Sample-to-Sample Spearman Correlation",
        x = NULL, y = NULL, fill = "rho"
      ) +
      theme_minimal(base_family = "sans") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$rna_correlation_guide <- renderUI({
    figure_guide(
      what = "The sample-correlation heatmap compares whole-profile similarity between every pair of selected samples.",
      how = "Each square is a Spearman correlation. Values closer to 1 indicate more similar rank-order expression patterns.",
      look_for = "Biological replicates often show high similarity. A sample with lower correlations to its expected peers may be worth reviewing.",
      limitation = "High correlation does not prove biological equivalence, and low correlation can reflect either biology or technical variation."
    )
  })
  
  rna_pca_data <- reactive({
    mat <- rna_matrix()
    validate(need(ncol(mat) >= 3, "At least three samples are required for RNA-seq PCA."))
    
    if (isTRUE(input$rna_log1p)) {
      validate(need(all(mat >= 0, na.rm = TRUE), "log1p transformation requires non-negative values."))
      mat <- log1p(mat)
    }
    
    complete_rows <- apply(mat, 1, function(x) all(is.finite(x)))
    mat <- mat[complete_rows, , drop = FALSE]
    
    feature_variance <- apply(mat, 1, var)
    keep <- is.finite(feature_variance) & feature_variance > 0
    mat <- mat[keep, , drop = FALSE]
    feature_variance <- feature_variance[keep]
    
    validate(need(nrow(mat) >= 2, "Too few variable features remain for PCA."))
    
    top_n <- min(1000, nrow(mat))
    top_index <- order(feature_variance, decreasing = TRUE)[seq_len(top_n)]
    pca_mat <- t(mat[top_index, , drop = FALSE])
    pca <- prcomp(pca_mat, center = TRUE, scale. = FALSE)
    
    scores <- as.data.frame(pca$x)
    scores$Sample <- rownames(scores)
    scores$Group <- factor("All samples")
    
    md <- metadata()
    if (
      !is.null(md) &&
      !is.null(input$metadata_sample_id) &&
      input$metadata_sample_id %in% names(md) &&
      !is.null(input$metadata_group) &&
      input$metadata_group != "None" &&
      input$metadata_group %in% names(md)
    ) {
      idx <- match(scores$Sample, as.character(md[[input$metadata_sample_id]]))
      scores$Group <- as.factor(md[[input$metadata_group]][idx])
    }
    
    list(pca = pca, scores = scores)
  })
  
  output$rna_pca_plot <- renderPlot({
    result <- rna_pca_data()
    variance <- result$pca$sdev^2
    variance_percent <- variance / sum(variance) * 100
    
    p <- ggplot(result$scores, aes(x = PC1, y = PC2, color = Group)) +
      geom_point(size = 3, alpha = 0.8) +
      labs(
        title = "RNA-seq Sample PCA",
        x = paste0("PC1 (", round(variance_percent[1], 1), "%)"),
        y = paste0("PC2 (", round(variance_percent[2], 1), "%)"),
        color = if (!is.null(input$metadata_group) && input$metadata_group != "None") input$metadata_group else "Group"
      ) +
      theme_minimal(base_family = "sans")
    
    if (isTRUE(input$rna_label_samples)) {
      p <- p + geom_text(
        aes(label = Sample),
        vjust = -0.7,
        size = 3,
        check_overlap = TRUE
      )
    }
    
    p
  })
  
  output$rna_pca_guide <- renderUI({
    figure_guide(
      what = "RNA-seq PCA compresses variation across many genes into a small number of components so that overall sample relationships can be visualized.",
      how = "Each point is one sample. Samples closer together have more similar expression profiles across the variable features used for PCA. The axis percentages show how much variation each component explains.",
      look_for = "Look for biological replicates clustering together, separation between conditions, unusual samples, or possible batch effects.",
      limitation = "PCA is descriptive. Separation does not prove that a labeled condition caused the difference, and preprocessing choices can change the plot."
    )
  })
  
  
  # ==========================================================
  # DIFFERENTIAL EXPRESSION
  # ==========================================================
  
  de_result_data <- reactive({
    data <- dataset()
    
    validate(
      need(!is.null(input$de_lfc_col) && input$de_lfc_col != "None" && input$de_lfc_col %in% names(data),
           "Select a valid log2 fold-change column."),
      need(!is.null(input$de_p_col) && input$de_p_col != "None" && input$de_p_col %in% names(data),
           "Select a valid p-value or adjusted p-value column.")
    )
    
    lfc <- data[[input$de_lfc_col]]
    pval <- data[[input$de_p_col]]
    
    validate(
      need(is.numeric(lfc), "The fold-change column must be numeric."),
      need(is.numeric(pval), "The significance column must be numeric.")
    )
    
    status <- rep("Not significant", nrow(data))
    up <- !is.na(pval) & !is.na(lfc) & pval < input$de_alpha & lfc >= input$de_lfc_cutoff
    down <- !is.na(pval) & !is.na(lfc) & pval < input$de_alpha & lfc <= -input$de_lfc_cutoff
    status[up] <- "Up"
    status[down] <- "Down"
    
    out <- data
    out$.BDE_status <- status
    out$.BDE_neglog10p <- -log10(pmax(pval, .Machine$double.xmin))
    out
  })
  
  output$de_summary <- renderText({
    x <- de_result_data()
    up <- sum(x$.BDE_status == "Up", na.rm = TRUE)
    down <- sum(x$.BDE_status == "Down", na.rm = TRUE)
    
    paste0(
      "Rows evaluated: ", format(nrow(x), big.mark = ","),
      "\nSignificant upregulated features: ", format(up, big.mark = ","),
      "\nSignificant downregulated features: ", format(down, big.mark = ","),
      "\nTotal significant features: ", format(up + down, big.mark = ","),
      "\n\nThresholds: ", input$de_p_col, " < ", input$de_alpha,
      " and |", input$de_lfc_col, "| ≥ ", input$de_lfc_cutoff
    )
  })
  
  output$de_volcano <- renderPlot({
    x <- de_result_data()
    d <- data.frame(
      LFC = x[[input$de_lfc_col]],
      NegLog10P = x$.BDE_neglog10p,
      Status = x$.BDE_status
    )
    
    ggplot(d, aes(x = LFC, y = NegLog10P, color = Status)) +
      geom_point(alpha = 0.65, size = 1.8, na.rm = TRUE) +
      geom_vline(xintercept = c(-input$de_lfc_cutoff, input$de_lfc_cutoff), linetype = "dashed") +
      geom_hline(yintercept = -log10(max(input$de_alpha, .Machine$double.xmin)), linetype = "dashed") +
      labs(
        title = "Volcano Plot",
        x = input$de_lfc_col,
        y = paste0("-log10(", input$de_p_col, ")"),
        color = "Classification"
      ) +
      theme_minimal(base_family = "sans")
  })
  
  output$de_volcano_guide <- renderUI({
    figure_guide(
      what = "A volcano plot displays effect size and statistical evidence for many features at the same time.",
      how = "The X-axis is log2 fold change. The Y-axis increases as the selected p-value becomes smaller. Dashed lines show your current cutoffs.",
      look_for = "Features toward the upper left and upper right combine larger effect sizes with stronger statistical evidence.",
      limitation = "This plot visualizes an existing result table. It cannot determine whether the original model, contrast, normalization, or multiple-testing strategy was appropriate."
    )
  })
  
  output$de_table <- renderDT({
    x <- de_result_data()
    x <- x[x$.BDE_status != "Not significant", , drop = FALSE]
    display_columns <- setdiff(names(x), ".BDE_neglog10p")
    
    datatable(
      x[display_columns],
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  output$download_de <- downloadHandler(
    filename = function() {
      paste0("BioDataExplorer_significant_features_", Sys.Date(), ".csv")
    },
    content = function(file) {
      x <- de_result_data()
      x <- x[x$.BDE_status != "Not significant", , drop = FALSE]
      x$.BDE_neglog10p <- NULL
      write.csv(x, file, row.names = FALSE)
    }
  )
  
  
  # ==========================================================
  # HEATMAP
  # ==========================================================
  
  heatmap_data <- reactive({
    data <- dataset()
    selected <- input$heatmap_columns
    
    validate(need(length(selected) >= 2, "Choose at least two numeric columns for the heatmap."))
    
    mat <- as.matrix(data[, selected, drop = FALSE])
    storage.mode(mat) <- "numeric"
    
    enough_values <- apply(mat, 1, function(x) sum(is.finite(x)) >= 2)
    original_rows <- which(enough_values)
    mat <- mat[enough_values, , drop = FALSE]
    
    validate(need(nrow(mat) >= 2, "Too few usable rows remain for the heatmap."))
    
    for (i in seq_len(nrow(mat))) {
      missing <- !is.finite(mat[i, ])
      if (any(missing)) {
        mat[i, missing] <- median(mat[i, ], na.rm = TRUE)
      }
    }
    
    row_var <- apply(mat, 1, var)
    keep <- is.finite(row_var)
    mat <- mat[keep, , drop = FALSE]
    original_rows <- original_rows[keep]
    row_var <- row_var[keep]
    
    top_n <- min(input$heatmap_top_n, nrow(mat))
    idx <- order(row_var, decreasing = TRUE)[seq_len(top_n)]
    mat <- mat[idx, , drop = FALSE]
    original_rows <- original_rows[idx]
    
    gene_col <- input$heatmap_gene_col
    if (!is.null(gene_col) && gene_col != "None" && gene_col %in% names(data)) {
      labels <- as.character(data[[gene_col]][original_rows])
      labels[is.na(labels) | labels == ""] <- paste0("Row_", original_rows[is.na(labels) | labels == ""])
      rownames(mat) <- make.unique(labels)
    } else {
      rownames(mat) <- paste0("Row_", original_rows)
    }
    
    if (isTRUE(input$heatmap_zscore)) {
      row_mean <- rowMeans(mat, na.rm = TRUE)
      row_sd <- apply(mat, 1, sd, na.rm = TRUE)
      keep_sd <- is.finite(row_sd) & row_sd > 0
      mat <- mat[keep_sd, , drop = FALSE]
      row_mean <- row_mean[keep_sd]
      row_sd <- row_sd[keep_sd]
      mat <- sweep(mat, 1, row_mean, "-")
      mat <- sweep(mat, 1, row_sd, "/")
    }
    
    validate(need(nrow(mat) >= 2, "Too few rows remain after scaling."))
    
    if (isTRUE(input$heatmap_cluster_rows) && nrow(mat) >= 2) {
      mat <- mat[hclust(dist(mat))$order, , drop = FALSE]
    }
    
    if (isTRUE(input$heatmap_cluster_columns) && ncol(mat) >= 2) {
      mat <- mat[, hclust(dist(t(mat)))$order, drop = FALSE]
    }
    
    mat
  })
  
  output$heatmap_plot <- renderPlot({
    mat <- heatmap_data()
    d <- expand.grid(
      Gene = rownames(mat),
      Sample = colnames(mat),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    d$Value <- as.vector(mat)
    d$Gene <- factor(d$Gene, levels = rev(rownames(mat)))
    d$Sample <- factor(d$Sample, levels = colnames(mat))
    
    ggplot(d, aes(x = Sample, y = Gene, fill = Value)) +
      geom_tile() +
      scale_fill_gradient2(low = "#3b82f6", mid = "white", high = "#ef4444", midpoint = 0) +
      labs(
        title = paste("Top", nrow(mat), "Variable Features"),
        x = "Sample / measurement",
        y = "Feature",
        fill = if (isTRUE(input$heatmap_zscore)) "Z-score" else "Value"
      ) +
      theme_minimal(base_family = "sans") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$heatmap_guide <- renderUI({
    figure_guide(
      what = "The heatmap displays patterns across the most variable selected features and measurement columns.",
      how = if (isTRUE(input$heatmap_zscore)) {
        "Each row is standardized to a z-score, so the colors show whether a value is relatively high or low for that feature across the selected columns."
      } else {
        "The colors represent the original numerical values in the selected columns."
      },
      look_for = "Look for groups of features with similar patterns, samples that share a profile, and features that behave differently across subsets of samples.",
      limitation = "Heatmaps are sensitive to scaling, feature selection, missing-value handling, and clustering choices. A visible cluster is a pattern to investigate, not proof of a mechanism."
    )
  })
  
  
  # ==========================================================
  # GENE / FEATURE SEARCH
  # ==========================================================
  
  gene_search_results <- reactive({
    data <- dataset()
    id_col <- input$gene_id_column
    search <- trimws(input$gene_search_text)
    
    validate(
      need(!is.null(id_col) && id_col != "None" && id_col %in% names(data), "Choose a valid identifier column."),
      need(nchar(search) > 0, "Enter a gene or feature identifier to search.")
    )
    
    ids <- as.character(data[[id_col]])
    hit <- grepl(search, ids, ignore.case = TRUE, fixed = TRUE)
    data[hit, , drop = FALSE]
  })
  
  output$gene_search_table <- renderDT({
    x <- gene_search_results()
    validate(need(nrow(x) > 0, "No matching genes or features were found."))
    
    datatable(
      x,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$gene_profile_plot <- renderPlot({
    x <- gene_search_results()
    validate(need(nrow(x) > 0, "No matching feature is available to plot."))
    
    selected <- input$gene_expression_columns
    validate(need(length(selected) >= 1, "Choose at least one numeric expression or measurement column."))
    
    first_row <- x[1, , drop = FALSE]
    values <- as.numeric(first_row[1, selected, drop = TRUE])
    id_value <- as.character(first_row[1, input$gene_id_column, drop = TRUE])
    
    d <- data.frame(Measurement = selected, Value = values, stringsAsFactors = FALSE)
    
    ggplot(d, aes(x = Measurement, y = Value)) +
      geom_col() +
      labs(title = paste("Profile:", id_value), x = NULL, y = "Value") +
      theme_minimal(base_family = "sans") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$gene_profile_guide <- renderUI({
    figure_guide(
      what = "This bar chart displays the selected numerical measurements for the first matching gene or feature returned by the search.",
      how = "Each bar corresponds to one selected sample or measurement column.",
      look_for = "Look for samples or conditions where the selected feature is relatively higher or lower.",
      limitation = "The biological meaning depends on whether the source values are raw counts, normalized expression, assay measurements, or another quantity."
    )
  })
  
  
  # ==========================================================
  # RNA-SEQ SAMPLE CLUSTERING & METADATA CHECK
  # ==========================================================
  
  rna_cluster_matrix <- reactive({
    mat <- rna_matrix()
    
    validate(
      need(ncol(mat) >= 3, "At least three samples are required for clustering."),
      need(all(mat >= 0, na.rm = TRUE), "RNA-seq clustering expects non-negative count-like values.")
    )
    
    mat <- log1p(mat)
    complete_rows <- apply(mat, 1, function(x) all(is.finite(x)))
    mat <- mat[complete_rows, , drop = FALSE]
    
    row_var <- apply(mat, 1, var)
    keep <- is.finite(row_var) & row_var > 0
    mat <- mat[keep, , drop = FALSE]
    row_var <- row_var[keep]
    
    validate(need(nrow(mat) >= 2, "Too few variable features remain for clustering."))
    
    top_n <- min(input$rna_top_variable_genes, nrow(mat))
    idx <- order(row_var, decreasing = TRUE)[seq_len(top_n)]
    
    t(mat[idx, , drop = FALSE])
  })
  
  
  output$rna_dendrogram_plot <- renderPlot({
    sample_matrix <- rna_cluster_matrix()
    hc <- hclust(dist(sample_matrix))
    
    plot(
      hc,
      main = "Hierarchical Sample Clustering",
      xlab = "",
      sub = "",
      ylab = "Euclidean distance",
      cex = 0.8
    )
  })
  
  
  output$rna_dendrogram_guide <- renderUI({
    figure_guide(
      what = "Hierarchical clustering groups RNA-seq samples according to similarity across the most variable log1p-transformed features.",
      how = "Samples that join together lower in the tree are more similar under the selected distance and clustering procedure.",
      look_for = "Look for biological replicates clustering together, unexpected groupings, and samples that branch far from expected peers.",
      limitation = "Clustering depends on preprocessing and feature selection. A separated sample is a reason to investigate, not an automatic reason to remove it."
    )
  })
  
  
  output$metadata_match_summary <- renderText({
    md <- metadata()
    
    if (is.null(md)) {
      return("No metadata file is loaded. Upload metadata to check sample-name matching.")
    }
    
    validate(
      need(
        !is.null(input$metadata_sample_id) &&
          input$metadata_sample_id %in% names(md),
        "Choose the metadata column containing sample IDs."
      )
    )
    
    samples <- input$rna_sample_columns
    metadata_ids <- as.character(md[[input$metadata_sample_id]])
    
    matched <- intersect(samples, metadata_ids)
    missing_in_metadata <- setdiff(samples, metadata_ids)
    metadata_only <- setdiff(metadata_ids, samples)
    duplicate_ids <- unique(metadata_ids[duplicated(metadata_ids)])
    
    paste0(
      "Count-matrix samples selected: ", length(samples),
      "\nMetadata sample IDs: ", length(metadata_ids),
      "\nMatched samples: ", length(matched),
      "\n\nSelected samples missing from metadata: ",
      if (length(missing_in_metadata) == 0) "None" else paste(missing_in_metadata, collapse = ", "),
      "\n\nMetadata IDs not present among selected count columns: ",
      if (length(metadata_only) == 0) "None" else paste(metadata_only, collapse = ", "),
      "\n\nDuplicate metadata sample IDs: ",
      if (length(duplicate_ids) == 0) "None" else paste(duplicate_ids, collapse = ", ")
    )
  })
  
  
  # ==========================================================
  # DIFFERENTIAL-EXPRESSION MA PLOT
  # ==========================================================
  
  output$de_ma_plot <- renderPlot({
    x <- de_result_data()
    
    validate(
      need(
        !is.null(input$de_mean_col) &&
          input$de_mean_col != "None" &&
          input$de_mean_col %in% names(x),
        "Choose a mean-expression column such as baseMean to create an MA plot."
      )
    )
    
    mean_values <- x[[input$de_mean_col]]
    
    validate(
      need(is.numeric(mean_values), "The selected mean-expression column must be numeric."),
      need(any(mean_values > 0, na.rm = TRUE), "The selected mean-expression column must contain positive values.")
    )
    
    d <- data.frame(
      Mean = mean_values,
      LFC = x[[input$de_lfc_col]],
      Status = x$.BDE_status
    )
    
    d <- d[
      is.finite(d$Mean) & d$Mean > 0 & is.finite(d$LFC),
      ,
      drop = FALSE
    ]
    
    ggplot(d, aes(x = Mean, y = LFC, color = Status)) +
      geom_point(alpha = 0.6, size = 1.7) +
      geom_hline(yintercept = 0) +
      geom_hline(
        yintercept = c(-input$de_lfc_cutoff, input$de_lfc_cutoff),
        linetype = "dashed"
      ) +
      scale_x_log10() +
      labs(
        title = "MA Plot",
        x = paste0(input$de_mean_col, " (log10 scale)"),
        y = input$de_lfc_col,
        color = "Classification"
      ) +
      theme_minimal(base_family = "sans")
  })
  
  
  output$de_ma_guide <- renderUI({
    figure_guide(
      what = "An MA plot compares the size of an expression change with the average abundance of each feature.",
      how = "The X-axis shows mean expression on a log scale and the Y-axis shows log2 fold change.",
      look_for = "Look for large fold changes across the abundance range, asymmetry around zero, and whether extreme changes occur mainly among low-abundance genes.",
      limitation = "The plot depends on the mean-expression quantity supplied by the source differential-expression analysis."
    )
  })
  
  
  # ==========================================================
  # QUALITY DASHBOARD
  # ==========================================================
  
  output$quality_metric_cards <- renderUI({
    data <- dataset()
    total_cells <- nrow(data) * ncol(data)
    missing_n <- sum(is.na(data))
    duplicate_n <- sum(duplicated(data))
    constant_n <- sum(
      vapply(
        data,
        function(x) length(unique(x[!is.na(x)])) <= 1,
        logical(1)
      )
    )
    missing_pct <- if (total_cells > 0) 100 * missing_n / total_cells else 0
    
    div(
      class = "metric-grid",
      div(class = "metric-card",
          div(class = "metric-value", format(nrow(data), big.mark = ",")),
          div(class = "metric-label", "Rows")),
      div(class = "metric-card",
          div(class = "metric-value", ncol(data)),
          div(class = "metric-label", "Columns")),
      div(class = "metric-card",
          div(class = "metric-value", paste0(round(missing_pct, 2), "%")),
          div(class = "metric-label", "Missing cells")),
      div(class = "metric-card",
          div(class = "metric-value", duplicate_n),
          div(class = "metric-label", "Duplicate rows")),
      div(class = "metric-card",
          div(class = "metric-value", constant_n),
          div(class = "metric-label", "Constant columns"))
    )
  })
  
  
  output$quality_interpretation <- renderText({
    data <- dataset()
    total_cells <- nrow(data) * ncol(data)
    missing_n <- sum(is.na(data))
    missing_pct <- if (total_cells > 0) 100 * missing_n / total_cells else 0
    
    if (missing_pct == 0) {
      "No missing values were detected. Continue to inspect duplicates, constant columns, sample structure, and biological plausibility."
    } else if (missing_pct < 5) {
      paste0(round(missing_pct, 2), "% of table cells are missing. Check whether missingness is concentrated in important variables or samples.")
    } else {
      paste0(round(missing_pct, 2), "% of table cells are missing. Review the missingness pattern before applying statistical analyses or transformations.")
    }
  })
  
  
  output$missingness_plot <- renderPlot({
    data <- dataset()
    d <- data.frame(
      Column = names(data),
      Missing = vapply(data, function(x) sum(is.na(x)), numeric(1)),
      stringsAsFactors = FALSE
    )
    d$Percent <- 100 * d$Missing / max(1, nrow(data))
    
    ggplot(d, aes(x = reorder(Column, Percent), y = Percent)) +
      geom_col() +
      coord_flip() +
      labs(title = "Missing Data by Column", x = NULL, y = "Missing values (%)") +
      theme_minimal(base_family = "sans")
  })
  
  
  output$missingness_guide <- renderUI({
    figure_guide(
      what = "This plot shows the percentage of missing entries in each dataset column.",
      how = "Each bar represents one column. Longer bars indicate a larger fraction of missing observations.",
      look_for = "Look for variables with substantially more missing data than the rest and whether that pattern makes sense for the experiment.",
      limitation = "The plot describes missingness but cannot determine why values are missing."
    )
  })
  
  
  outlier_data <- reactive({
    data <- dataset()
    variable <- input$quality_outlier_variable
    
    validate(
      need(
        !is.null(variable) &&
          variable %in% names(data) &&
          is.numeric(data[[variable]]),
        "Choose a numeric variable for outlier screening."
      )
    )
    
    x <- data[[variable]]
    finite <- is.finite(x)
    x_valid <- x[finite]
    validate(need(length(x_valid) >= 4, "At least four finite values are required."))
    
    q1 <- unname(quantile(x_valid, 0.25, na.rm = TRUE))
    q3 <- unname(quantile(x_valid, 0.75, na.rm = TRUE))
    iqr_value <- q3 - q1
    lower <- q1 - input$quality_iqr_multiplier * iqr_value
    upper <- q3 + input$quality_iqr_multiplier * iqr_value
    
    flag <- rep(FALSE, length(x))
    flag[finite] <- x_valid < lower | x_valid > upper
    
    list(
      data = data.frame(
        Index = seq_along(x),
        Value = x,
        Flag = ifelse(flag, "Flagged", "Within range")
      ),
      lower = lower,
      upper = upper,
      n_flagged = sum(flag, na.rm = TRUE)
    )
  })
  
  
  output$outlier_plot <- renderPlot({
    result <- outlier_data()
    
    ggplot(result$data, aes(x = Index, y = Value, shape = Flag)) +
      geom_point(alpha = 0.75, size = 2.2, na.rm = TRUE) +
      geom_hline(yintercept = c(result$lower, result$upper), linetype = "dashed") +
      labs(
        title = paste("Exploratory Outlier Screen:", input$quality_outlier_variable),
        x = "Row index",
        y = input$quality_outlier_variable,
        shape = "IQR screen"
      ) +
      theme_minimal(base_family = "sans")
  })
  
  
  output$outlier_summary <- renderText({
    result <- outlier_data()
    paste0(
      "Flagged observations: ", result$n_flagged,
      "\nLower exploratory boundary: ", round(result$lower, 4),
      "\nUpper exploratory boundary: ", round(result$upper, 4),
      "\n\nNo observations are removed automatically."
    )
  })
  
  
  output$outlier_guide <- renderUI({
    figure_guide(
      what = "This is an exploratory IQR-based screen for unusually low or high values in one numerical variable.",
      how = "Dashed lines mark the lower and upper screening boundaries. Points beyond those boundaries are flagged for review.",
      look_for = "Inspect flagged rows in the context of sample identity, replicate structure, the experiment, and raw data.",
      limitation = "An outlier is not automatically an error. Real biology can produce extreme values."
    )
  })
  
  
  # ==========================================================
  # TIME SERIES
  # ==========================================================
  
  time_series_data <- reactive({
    data <- dataset()
    
    validate(
      need(!is.null(input$time_x) && input$time_x %in% names(data), "Choose a valid ordered variable."),
      need(!is.null(input$time_y) && input$time_y %in% names(data), "Choose a valid measurement."),
      need(is.numeric(data[[input$time_y]]), "The selected measurement must be numeric.")
    )
    
    use_group <- !is.null(input$time_group) &&
      input$time_group != "None" &&
      input$time_group %in% names(data)
    
    d <- data.frame(
      Time = data[[input$time_x]],
      Value = data[[input$time_y]],
      Group = if (use_group) as.factor(data[[input$time_group]]) else factor("All observations"),
      stringsAsFactors = FALSE
    )
    
    d <- d[complete.cases(d), , drop = FALSE]
    validate(need(nrow(d) >= 2, "At least two complete observations are required."))
    d
  })
  
  
  time_summary <- reactive({
    d <- time_series_data()
    mean_df <- aggregate(Value ~ Time + Group, data = d, FUN = mean)
    sd_df <- aggregate(Value ~ Time + Group, data = d, FUN = sd)
    n_df <- aggregate(Value ~ Time + Group, data = d, FUN = length)
    names(mean_df)[3] <- "Mean"
    names(sd_df)[3] <- "SD"
    names(n_df)[3] <- "N"
    out <- merge(mean_df, sd_df, by = c("Time", "Group"), all = TRUE)
    out <- merge(out, n_df, by = c("Time", "Group"), all = TRUE)
    out$SE <- out$SD / sqrt(out$N)
    out
  })
  
  
  output$time_plot <- renderPlot({
    d <- time_series_data()
    
    if (isTRUE(input$time_summarize)) {
      s <- time_summary()
      p <- ggplot(s, aes(x = Time, y = Mean, group = Group, linetype = Group)) +
        geom_line(linewidth = 0.9) +
        geom_point(size = 2.3) +
        geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.1, na.rm = TRUE) +
        labs(y = paste0(input$time_y, " (mean ± SE)"))
    } else {
      p <- ggplot(d, aes(x = Time, y = Value, group = Group, linetype = Group)) +
        geom_line(linewidth = 0.8) +
        geom_point(size = 2) +
        labs(y = input$time_y)
    }
    
    p +
      labs(
        title = paste(input$time_y, "across", input$time_x),
        x = input$time_x,
        linetype = if (input$time_group != "None") input$time_group else NULL
      ) +
      theme_minimal(base_family = "sans")
  })
  
  
  output$time_plot_guide <- renderUI({
    figure_guide(
      what = if (isTRUE(input$time_summarize)) {
        "This plot summarizes the selected measurement across the ordered variable using group means and standard errors."
      } else {
        "This plot shows individual observations across the selected ordered variable."
      },
      how = if (isTRUE(input$time_summarize)) {
        "Points and lines are group means. Error bars represent standard error when replicate measurements are available."
      } else {
        "Points are measured observations and connecting lines make the trajectory easier to follow."
      },
      look_for = "Look for increases, decreases, plateaus, peaks, delayed responses, and differences between groups.",
      limitation = "Mean ± SE does not show the full distribution, and connecting observations does not establish a mechanism."
    )
  })
  
  
  output$time_summary_table <- renderDT({
    datatable(
      time_summary(),
      rownames = FALSE,
      options = list(pageLength = 12, scrollX = TRUE)
    )
  })
  
  
  # ==========================================================
  # GENE SET EXPLORER
  # ==========================================================
  
  geneset_score_data <- reactive({
    data <- dataset()
    id_col <- input$geneset_id_column
    selected <- input$geneset_sample_columns
    
    validate(
      need(!is.null(id_col) && id_col != "None" && id_col %in% names(data), "Choose a valid gene identifier column."),
      need(length(selected) >= 2, "Choose at least two numeric measurement columns.")
    )
    
    requested <- unlist(strsplit(input$geneset_text, "[,;[:space:]]+"))
    requested <- unique(trimws(requested))
    requested <- requested[requested != ""]
    validate(need(length(requested) > 0, "Paste at least one gene identifier."))
    
    ids <- as.character(data[[id_col]])
    matched <- toupper(ids) %in% toupper(requested)
    validate(need(sum(matched, na.rm = TRUE) >= 1, "None of the pasted identifiers matched the selected gene column."))
    
    mat <- as.matrix(data[matched, selected, drop = FALSE])
    storage.mode(mat) <- "numeric"
    
    z_rows <- lapply(
      seq_len(nrow(mat)),
      function(i) {
        x <- mat[i, ]
        m <- mean(x, na.rm = TRUE)
        s <- sd(x, na.rm = TRUE)
        if (!is.finite(s) || s == 0) rep(NA_real_, length(x)) else (x - m) / s
      }
    )
    
    zmat <- do.call(rbind, z_rows)
    colnames(zmat) <- selected
    scores <- colMeans(zmat, na.rm = TRUE)
    valid <- is.finite(scores)
    validate(need(any(valid), "A stable gene-set score could not be calculated."))
    
    list(
      score_table = data.frame(
        Measurement = selected[valid],
        Score = scores[valid],
        stringsAsFactors = FALSE
      ),
      matched_ids = ids[matched],
      requested_ids = requested
    )
  })
  
  
  output$geneset_summary <- renderText({
    result <- geneset_score_data()
    paste0(
      "Requested identifiers: ", length(result$requested_ids),
      "\nMatched dataset rows: ", length(result$matched_ids),
      "\n\nMatched identifiers:\n",
      paste(unique(result$matched_ids), collapse = ", ")
    )
  })
  
  
  output$geneset_plot <- renderPlot({
    result <- geneset_score_data()
    ggplot(result$score_table, aes(x = reorder(Measurement, Score), y = Score)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Exploratory Gene-set Score",
        x = NULL,
        y = "Mean feature-wise z-score"
      ) +
      theme_minimal(base_family = "sans")
  })
  
  
  output$geneset_guide <- renderUI({
    figure_guide(
      what = "This exploratory score summarizes the relative behavior of a user-defined set of genes across selected samples or measurements.",
      how = "Each matched gene is standardized across the selected columns, then those standardized values are averaged for each column.",
      look_for = "Higher scores indicate the gene set tends to be relatively elevated in that sample or measurement column; lower scores indicate the opposite pattern.",
      limitation = "This is an exploratory signature score, not a validated pathway-activity model."
    )
  })
  
  
  # ==========================================================
  # METHODS / REPRODUCIBLE CODE
  # ==========================================================
  
  output$methods_notes <- renderText({
    data <- dataset()
    paste0(
      "Dataset type: ", input$dataset_type,
      "\nRows: ", nrow(data),
      "\nColumns: ", ncol(data),
      "\nMissing values: ", sum(is.na(data)),
      "\nDuplicate rows: ", sum(duplicated(data)),
      "\n\nDifferential-expression significance cutoff: ", input$de_alpha,
      "\nAbsolute log2 fold-change cutoff: ", input$de_lfc_cutoff,
      "\nRNA-seq detected-feature threshold: ", input$rna_detect_threshold,
      "\n\nRecord the original data source, preprocessing, organism, experimental design, and software/package versions separately."
    )
  })
  
  
  reproducible_code_text <- reactive({
    if (input$dataset_type == "RNA-seq count matrix") {
      paste(
        "# BioData Explorer reproducibility starter",
        'counts <- read.csv("counts.csv", check.names = FALSE)',
        "sample_columns <- c(...)",
        "count_matrix <- as.matrix(counts[, sample_columns, drop = FALSE])",
        "library_size <- colSums(count_matrix, na.rm = TRUE)",
        paste0("detected_genes <- colSums(count_matrix >= ", input$rna_detect_threshold, ", na.rm = TRUE)"),
        "log_counts <- log1p(count_matrix)",
        "gene_variance <- apply(log_counts, 1, var, na.rm = TRUE)",
        "top_index <- order(gene_variance, decreasing = TRUE)[seq_len(min(1000, nrow(log_counts)))]",
        "pca <- prcomp(t(log_counts[top_index, , drop = FALSE]), center = TRUE, scale. = FALSE)",
        sep = "\n"
      )
    } else if (input$dataset_type == "Differential-expression results") {
      paste(
        "# BioData Explorer reproducibility starter",
        'de <- read.csv("differential_expression.csv", check.names = FALSE)',
        paste0("alpha <- ", input$de_alpha),
        paste0("lfc_cutoff <- ", input$de_lfc_cutoff),
        "# Replace example column names with your actual mapped columns.",
        "sig <- subset(de, !is.na(padj) & padj < alpha & abs(log2FoldChange) >= lfc_cutoff)",
        sep = "\n"
      )
    } else {
      paste(
        "# BioData Explorer reproducibility starter",
        'data <- read.csv("dataset.csv", check.names = FALSE)',
        "summary(data)",
        "numeric_columns <- names(data)[vapply(data, is.numeric, logical(1))]",
        "correlation_matrix <- cor(data[numeric_columns], use = 'pairwise.complete.obs')",
        sep = "\n"
      )
    }
  })
  
  
  output$reproducible_code <- renderText({
    reproducible_code_text()
  })
  
  
  output$download_repro_code <- downloadHandler(
    filename = function() {
      paste0("BioDataExplorer_reproducibility_", Sys.Date(), ".R")
    },
    content = function(file) {
      writeLines(reproducible_code_text(), file)
    }
  )
  
  
  # ==========================================================
  # DOWNLOAD ANALYSIS SUMMARY REPORT
  # ==========================================================
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0(
        "BioDataExplorer_Summary_",
        Sys.Date(),
        ".html"
      )
    },
    
    content = function(file) {
      data <- dataset()
      
      numeric_n <- sum(
        vapply(data, is.numeric, logical(1))
      )
      
      missing_n <- sum(is.na(data))
      duplicate_n <- sum(duplicated(data))
      
      file_name <- if (is.null(input$dataset)) {
        "Unknown"
      } else {
        input$dataset$name
      }
      
      esc <- htmltools::htmlEscape
      
      html <- paste0(
        "<!doctype html><html><head><meta charset='utf-8'>",
        "<title>BioData Explorer Summary</title>",
        "<style>",
        "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Arial,sans-serif;max-width:900px;margin:40px auto;padding:0 20px;color:#1f2937;line-height:1.6;}",
        "h1,h2{color:#1f4e79}.card{border:1px solid #e5e7eb;border-radius:10px;padding:18px;margin:16px 0;background:#fff}",
        "</style></head><body>",
        "<h1>BioData Explorer Analysis Summary</h1>",
        "<p><strong>Generated:</strong> ", esc(as.character(Sys.time())), "</p>",
        "<div class='card'><h2>Dataset</h2>",
        "<p><strong>File:</strong> ", esc(file_name), "</p>",
        "<p><strong>Selected dataset type:</strong> ", esc(input$dataset_type), "</p>",
        "<p><strong>Rows:</strong> ", nrow(data),
        "<br><strong>Columns:</strong> ", ncol(data),
        "<br><strong>Numeric columns:</strong> ", numeric_n,
        "<br><strong>Missing values:</strong> ", missing_n,
        "<br><strong>Duplicate rows:</strong> ", duplicate_n, "</p></div>",
        "<div class='card'><h2>Interpretation guidance</h2>",
        "<p>BioData Explorer provides exploratory visualization and statistical summaries. Biological conclusions should be based on the experimental design, preprocessing history, appropriate statistical model, and relevant validation.</p>",
        "</div>",
        "<div class='card'><h2>Reproducibility</h2>",
        "<p>Record the source dataset, preprocessing steps, sample metadata, software versions, filtering decisions, transformations, and statistical thresholds used during analysis.</p>",
        "</div></body></html>"
      )
      
      writeLines(html, file)
    }
  )
  
  
  # ==========================================================
  # SELECTED DATASET TYPE
  # ==========================================================
  
  output$confirmed_dataset_type <- renderText({
    
    paste0(
      "Selected dataset type: ",
      input$dataset_type,
      "\n\n",
      paste(
        "Dataset recognition is advisory.",
        "Specialized biological analyses should only be run",
        "after confirming what the measurements represent."
      )
    )
    
  })
  
  
  # ==========================================================
  # DOWNLOAD DATA
  # ==========================================================
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "BioDataExplorer_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file) {
      
      write.csv(
        dataset(),
        file,
        row.names = FALSE
      )
      
    }
    
  )
  
}


# ============================================================
# START APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
