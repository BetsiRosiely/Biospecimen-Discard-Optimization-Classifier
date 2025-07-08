# Set maximum upload size to 200 MB
options(shiny.maxRequestSize = 200 * 1024^2)

# Load required packages
library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(openxlsx)
library(tidyr)
library(ggplot2)
library(janitor)  # for clean_names()

# Define UI
ui <- fluidPage(
  titlePanel("Sample Type Optimization Classifier"),
  
  tabsetPanel(
    tabPanel("Classifier",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Upload Excel File (.xlsx)", accept = ".xlsx"),
                 uiOutput("replicate_ui"),
                 actionButton("apply_remove", "Remove Duplicates"),
                 hr(),
                 uiOutput("classification_ui"),
                 uiOutput("manual_keep_ui"),
                 actionButton("classify", "Apply Classification"),
                 downloadButton("download_data", "Download Updated Excel")
               ),
               mainPanel(
                 h4("Preview of Uploaded Data"),
                 DTOutput("preview"),
                 h4("Replicate Rows"),
                 DTOutput("replicates"),
                 h4("Summary of Optimization Classification"),
                 DTOutput("summary_table")
               )
             )
    ),
    
    tabPanel("Summary Report",
             h4("Pivot Summary by Sample Type _ Screen ID _ Visit Name"),
             DTOutput("report_table")
    ),
    
    tabPanel("Summary & Instructions",
             h4("📊 Total Samples Kept vs Discarded by Sample Type"),
             plotOutput("summary_plot"),
             hr(),
             h4("📘 How to Use This App"),
             p("1. Upload an Excel file containing biospecimen records with columns like sample type, screen ID, visit name, etc."),
             p("2. The app automatically identifies replicate rows."),
             p("3. Use the dropdowns to classify each sample type as 'keep', 'discard', or 'manual select'."),
             p("4. For 'manual select', define how many samples per visit per screen ID should be kept."),
             p("5. Click 'Apply Classification' to label rows with 'keep' or 'discard'."),
             p("6. Navigate to the 'Summary Report' tab to view a grouped table by sample_type_screenID_visit."),
             p("7. Use 'Download Updated Excel' to export the classified data and summary table."),
             p("All processing happens locally — your data is not stored.")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  data_original   <- reactiveVal()
  data_classified <- reactiveVal()
  report_data     <- reactiveVal()
  plot_data       <- reactiveVal()
  
  # Known column variants (all snake_case)
  synonyms <- list(
    shipped_date   = c("shipped_date",   "shippeddate"),
    sample_comment = c("sample_comment", "comment"),
    sample_type    = c("sample_type",    "type", "stype", "sample_types2"),
    screen_id      = c("screen_id",      "screen", "sid", "subject"),
    visit_name     = c("visit_name",     "visit",  "vname")
  )
  
  # On file upload: clean, auto-detect, then rename only detected cols
  observeEvent(input$file, {
    req(input$file)
    raw   <- read_excel(input$file$datapath)
    clean <- raw %>% clean_names()  # snake_case, lowercase
    
    # build actual→canonical map
    col_map <- lapply(names(synonyms), function(field) {
      variants <- synonyms[[field]]
      found    <- intersect(variants, names(clean))
      if (length(found)) found[1] else NA_character_
    })
    names(col_map) <- names(synonyms)
    
    # check required (except sample_comment)
    required <- setdiff(names(synonyms), "sample_comment")
    missing  <- required[is.na(col_map[required])]
    if (length(missing)) {
      stop("Could not detect required columns: ", paste(missing, collapse=", "))
    }
    
    # only keep the mappings we actually found
    valid_map <- col_map[!is.na(col_map)]
    
    # rename
    df <- clean %>%
      rename( !!!setNames(valid_map, names(valid_map)) )
    
    # if sample_comment missing, create
    if (is.na(col_map["sample_comment"])) {
      df$sample_comment <- NA_character_
    }
    
    # coerce types
    df <- df %>%
      mutate(
        shipped_date   = as.character(shipped_date),
        sample_comment = as.character(sample_comment)
      )
    
    df$optimization <- NA_character_
    data_original(df)
    data_classified(df)
  })
  
  # Preview & replicates
  output$preview <- renderDT({
    req(data_classified())
    datatable(data_classified(), options = list(pageLength=10))
  })
  output$replicates <- renderDT({
    req(data_original())
    dupes <- data_original()[duplicated(data_original()) | duplicated(data_original(),fromLast=TRUE), ]
    datatable(dupes, options = list(pageLength=10))
  })
  output$replicate_ui <- renderUI({
    req(data_original())
    dupes <- data_original()[duplicated(data_original()) | duplicated(data_original(),fromLast=TRUE), ]
    p(if (nrow(dupes)>0) "Replicate rows detected." else "No replicate rows found.")
  })
  observeEvent(input$apply_remove, {
    df <- data_classified()
    data_classified(df[!duplicated(df), ])
  })
  
  # Classification UI
  output$classification_ui <- renderUI({
    req(data_classified())
    types <- unique(data_classified()$sample_type)
    lapply(types, function(t) {
      selectInput(
        paste0("type_", gsub(" ", "_", t)),
        paste("Classify sample type:", t),
        choices = c("skip","discard","keep","manual select"),
        selected = "skip"
      )
    })
  })
  output$manual_keep_ui <- renderUI({
    req(data_classified())
    types <- unique(data_classified()$sample_type)
    manual <- types[sapply(types, function(t)
      input[[paste0("type_", gsub(" ", "_", t))]]=="manual select"
    )]
    if (!length(manual)) return(NULL)
    lapply(manual, function(t) {
      numericInput(
        paste0("manual_keep_", gsub(" ", "_", t)),
        paste("How many to KEEP per Visit per Screen ID for", t, "?"),
        value=1, min=1
      )
    })
  })
  
  # Apply classification & build summaries
  observeEvent(input$classify, {
    df <- data_classified()
    df$optimization[] <- NA_character_
    
    # by sample_type
    for (t in unique(df$sample_type)) {
      choice <- input[[paste0("type_", gsub(" ", "_", t))]]
      if (choice %in% c("keep","discard")) {
        mask <- df$sample_type==t & is.na(df$optimization)
        df$optimization[mask] <- choice
      }
    }
    # manual select
    for (t in unique(df$sample_type)) {
      if (input[[paste0("type_", gsub(" ", "_", t))]]=="manual select") {
        n_keep <- input[[paste0("manual_keep_", gsub(" ", "_", t))]]
        grp_df <- df %>% filter(sample_type==t)
        for (grp in grp_df %>% group_by(screen_id, visit_name) %>% group_split()) {
          idx      <- which(
            df$sample_type==t &
              df$screen_id==grp$screen_id[1] &
              df$visit_name==grp$visit_name[1]
          )
          ord      <- df[idx, ] %>% mutate(orig=idx) %>% arrange(desc(shipped_date))
          keep_idx <- head(ord$orig, n_keep)
          df$optimization[keep_idx] <- "keep"
          df$optimization[ setdiff(idx, keep_idx) ] <- "discard"
        }
      }
    }
    data_classified(df)
    
    # pivot-style summary
    sum_df <- df %>%
      mutate(group_id = paste(sample_type, screen_id, visit_name, sep="_")) %>%
      count(group_id, optimization) %>%
      pivot_wider(names_from=optimization, values_from=n, values_fill=0)
    report_data(sum_df)
    
    # plot data
    p_df <- df %>%
      group_by(sample_type, optimization) %>%
      summarise(n=n(), .groups="drop")
    plot_data(p_df)
  })
  
  # Render tables & plot
  output$summary_table <- renderDT({
    req(data_classified())
    datatable(as.data.frame(table(data_classified()$optimization, useNA="ifany")))
  })
  output$report_table <- renderDT({
    req(report_data())
    datatable(report_data(), options = list(pageLength=25))
  })
  output$summary_plot <- renderPlot({
    req(plot_data())
    ggplot(plot_data(), aes(x=sample_type, y=n, fill=optimization)) +
      geom_col(position="dodge") +
      labs(title="Samples by Type and Optimization",
           x="Sample Type", y="Count", fill="Status") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45,hjust=1))
  })
  
  # Download all sheets + instructions & plot
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file$name), "_Optimized_Report.xlsx")
    },
    content = function(file) {
      full   <- data_original()
      full$Optimization <- data_classified()$optimization
      sum_df <- report_data()
      
      wb <- createWorkbook()
      addWorksheet(wb, "Full_Data");        writeData(wb, "Full_Data", full)
      addWorksheet(wb, "Summary_by_Group"); writeData(wb, "Summary_by_Group", sum_df)
      
      addWorksheet(wb, "Summary_Instructions")
      instr <- c(
        "How to Use This App:", "",
        "1. Upload Excel with sample_type, screen_id, visit_name, etc.",
        "2. Remove or detect duplicates.",
        "3. Classify via dropdowns (keep/discard/manual).",
        "4. For manual select, specify how many to keep.",
        "5. Click 'Apply Classification'.",
        "6. View 'Summary Report' tab for the pivot summary.",
        "7. Use this sheet & 'Full_Data' to review results."
      )
      writeData(wb, "Summary_Instructions", instr)
      img <- tempfile(fileext=".png")
      ggsave(img, plot=last_plot(), width=6, height=4, dpi=150)
      insertImage(wb, "Summary_Instructions", img,
                  startCol=1, startRow=length(instr)+2,
                  width=6, height=4, units="in")
      saveWorkbook(wb, file, overwrite=TRUE)
    }
  )
}

# Run the app
shinyApp(ui, server)
