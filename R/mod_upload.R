# Data Upload Module
# ------------------------------------------------------------------------------

#' Data Upload Module UI
#'
#' @param id Namespace ID for the module
#' @return Shiny UI elements
mod_upload_ui <- function(id) {
    ns <- NS(id)
    tagList(
        glassCard(
            inputId = ns("upload_card"),
            title = "Upload Biological Variation Data File",
            width = "100%",
            collapsed = FALSE,
            disabled = FALSE,
            toolbar = NULL,
            attached = TRUE,
            nested = TRUE,
            glassFileInput(
                inputId = ns("file_upload"),
                label = "Choose CSV or Excel file",
                label_icon = icon("file-upload"),
                accept = c(".csv", ".xlsx"),
                button_label = "Get that file ...",
                button_label_icon = icon("folder-open"),
                tooltip_empty = "What are you waiting for? Upload a file!",
                tooltip_filled = "Nice! File selected",
                help_text = "Upload a CSV or Excel file containing your biological variation data."
            ),
            glassRadioButtons(
                inputId = ns("sheet_name"),
                label = "Select Sheet (for Excel files):",
                label_icon = icon("file-excel"),
                choices = list("No sheets before upload ..." = "none"),
                selected = "none",
                disabled = TRUE # Initially disabled until uploaded
            )
        )
    )
}

#' Data Upload Module Server
#'
#' @param id Namespace ID for the module
#' @return Reactive value containing uploaded data
mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helper to read data safely
    read_data_safely <- function(file_path, file_ext) {
      tryCatch({
        df <- if (grepl("xls|xlsx", file_ext, ignore.case = TRUE)) {
          readxl::read_excel(file_path)
        } else {
          data.table::fread(file_path, data.table = FALSE)
        }
        
        if (nrow(df) == 0 || ncol(df) == 0) {
          stop("The uploaded file is empty.")
        }
        return(df)
      }, error = function(e) {
        showGlassToast(
          message = paste("Error processing file:", e$message),
          title = "Upload Failed",
          type = "error",
          duration = 5000
        )
        return(NULL)
      })
    }
    
    # Reactive to read ALL data (all sheets for xlsx, or single dataframe for csv)
    uploaded_data_full <- reactive({
      req(input$file_upload)
      
      # Process the raw glassFileInput input
      processed <- processGlassFile(input$file_upload)
      if (is.null(processed)) return(NULL)
      
      ext <- tools::file_ext(processed$name)
      
      if (ext == "xlsx") {
        # Read all sheets
        sheet_names <- readxl::excel_sheets(path = processed$datapath)
        out <- sapply(
          X = sheet_names,
          FUN = function(sheet_name) {
            readxl::read_excel(
              path = processed$datapath,
              sheet = sheet_name
            )
          },
          simplify = FALSE
        )
        names(out) <- sheet_names
        return(out)
      } else if (ext == "csv") {
        return(data.table::fread(processed$datapath))
      } else {
        showGlassToast(
          message = "Please upload a CSV or Excel file.",
          title = "Invalid Format",
          type = "warning"
        )
        return(NULL)
      }
    })
    
    # Reactive to get sheet names from uploaded excel file
    names_of_excel_sheets <- reactive({
      req(input$file_upload)
      
      processed <- processGlassFile(input$file_upload)
      if (is.null(processed)) return("No sheets here ...")
      
      ext <- tools::file_ext(processed$name)
      
      if (ext == "xlsx") {
        readxl::excel_sheets(path = processed$datapath)
      } else {
        return("No sheets here ...")
      }
    })
    
    # Observer to update the sheet selection buttons
    observe({
      req(names_of_excel_sheets())
      sheet_names <- names_of_excel_sheets()
      enable_sheet_selector <- length(sheet_names) > 1
      
      # Update glass radio buttons
      choices_list <- setNames(as.list(sheet_names), sheet_names)
      updateGlassRadio(
        session = session,
        inputId = "sheet_name",
        choices = choices_list,
        selected = sheet_names[1],
        disabled = !enable_sheet_selector
      )
    })
    
    # Reactive to extract the sheet of the uploaded file (if .xlsx)
    uploaded_data <- reactive({
      cat("\n=== UPLOADED_DATA REACTIVE TRIGGERED ===\n")
      
      if (is.null(uploaded_data_full())) {
        cat("uploaded_data_full() is NULL\n")
        return(NULL)
      }
      
      cat("uploaded_data_full() exists\n")
      
      processed <- processGlassFile(input$file_upload)
      if (is.null(processed)) {
        cat("processGlassFile returned NULL\n")
        return(NULL)
      }
      
      ext <- tools::file_ext(processed$name)
      cat("File extension:", ext, "\n")
      
      if (ext == "xlsx") {
        cat("Excel file - checking sheet selection\n")
        cat("input$sheet_name:", input$sheet_name, "\n")
        
        # If sheet_name is still "none", use the first sheet
        sheet_to_use <- input$sheet_name
        if (is.null(sheet_to_use) || sheet_to_use == "none") {
          sheet_names <- names(uploaded_data_full())
          if (length(sheet_names) > 0) {
            sheet_to_use <- sheet_names[1]
            cat("Using default first sheet:", sheet_to_use, "\n")
          } else {
            cat("No sheets found in Excel file\n")
            return(NULL)
          }
        }
        
        result <- uploaded_data_full()[[sheet_to_use]]
        cat("Retrieved sheet:", sheet_to_use, "Rows:", nrow(result), "Cols:", ncol(result), "\n")
        return(result)
      } else {
        cat("CSV file - returning data directly\n")
        result <- uploaded_data_full()
        cat("Retrieved data. Rows:", nrow(result), "Cols:", ncol(result), "\n")
        return(result)
      }
    })
    
    # Return reactive values
    return_list <- list(
      data = uploaded_data,
      file_uploaded = reactive(!is.null(input$file_upload))
    )
    
    cat("\n=== MOD_UPLOAD RETURN ===\n")
    cat("Returning list with data reactive and file_uploaded reactive\n")
    
    return(return_list)
  })
}
