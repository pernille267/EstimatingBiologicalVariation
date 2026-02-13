# Column Mapping Module
# ------------------------------------------------------------------------------

#' Column Mapping Module UI
#'
#' @param id Namespace ID for the module
#' @return Shiny UI elements
mod_column_map_ui <- function(id) {
  ns <- shiny::NS(id)

  glassRow(
    glassCol(
      width = 12,
      glassCard(
        inputId = ns("column_map_card"),
        title = "Select Columns",
        icon = icon("tasks"),
        attached = TRUE,
        nested = TRUE,
        width = "100%",
        toolbar = tagList(
          glassButton(
            inputId = ns("guess_cols_btn"),
            label = "Guess",
            icon = icon("wand-magic-sparkles"),
            color = "purple"
          ),
          glassButton(
            inputId = ns("reset_cols_btn"),
            label = "Reset",
            icon = icon("undo"),
            color = "purple"
          )
        ),
        glassRow(
          glassCol(
            width = 6,
            label = "Mandatory Selections",
            label_icon = icon("arrow-pointer"),
            label_position = "center",
            label_underline = TRUE,
            glassDropdown(
              ns("measurement_col"),
              label = "Measurement Column (y)",
              label_icon = icon("chart-line"),
              help_text = "Select the column containing the numeric measurement values",
              choices = c("None" = ""),
              selected = "",
              color = "green"
            ),
            glassDropdown(
              ns("subject_id_col"),
              label = "Subject ID Column",
              label_icon = icon("user"),
              help_text = "Select the column that uniquely identifies each subject",
              choices = c("None" = ""),
              selected = "",
              color = "green"
            ),
            glassDropdown(
              ns("sample_id_col"),
              label = "Sample ID Column",
              label_icon = icon("vial"),
              help_text = "Select the column that identifies each sample within a subject",
              choices = c("None" = ""),
              selected = "",
              color = "green"
            ),
            glassDropdown(
              ns("replicate_id_col"),
              label = "Replicate ID Column",
              label_icon = icon("copy"),
              help_text = "Select the column that identifies replicates within a sample",
              choices = c("None" = ""),
              selected = "",
              color = "green"
            )
          ),
          glassCol(
            width = 6,
            label = "Group & Population Selections",
            label_icon = icon("layer-group"),
            label_position = "center",
            label_underline = TRUE,
            glassDropdown(
              ns("analyte_col"),
              label = "Analyte Column",
              label_icon = icon("flask"),
              help_text = "Select the column that specifies the analyte or biomarker being measured",
              choices = c("None" = ""),
              selected = "",
              color = "green"
            ),
            glassDropdown(
              ns("material_col"),
              label = "Material / Matrix Column",
              label_icon = icon("droplet"),
              help_text = "Select the column that indicates the sample material (e.g., serum, plasma, urine)",
              choices = c("None" = ""),
              selected = "",
              color = "green"
            ),
            glassDropdown(
              ns("sex_col"),
              label = "Sex Column",
              label_icon = icon("venus-mars"),
              help_text = "Select the column that indicates biological sex of subjects",
              choices = c("None" = ""),
              selected = "",
              color = "green"
            ),
            glassDropdown(
              ns("deseases_col"),
              label = "Diseases Column",
              label_icon = icon("heart-pulse"),
              help_text = "Select the column that indicates disease status or health condition",
              choices = c("None" = ""),
              selected = "",
              color = "green"
            )
          )
        ),
        glassRow(
          glassCol(
            width = 6,
            label = "Custom Group 1",
            label_icon = icon("tag"),
            label_position = "center",
            label_underline = TRUE,
            glassDropdown(
              ns("group_1_col"),
              label = "Custom Group 1 Column",
              label_icon = icon("tag"),
              help_text = "Select a custom grouping variable (e.g., age, region, sampling interval)",
              choices = c("None" = ""),
              selected = "",
              color = "green"
            )
          ),
          glassCol(
            width = 6,
            label = "Custom Group 2",
            label_icon = icon("tags"),
            label_position = "center",
            label_underline = TRUE,
            glassDropdown(
              ns("group_2_col"),
              label = "Custom Group 2 Column",
              label_icon = icon("tags"),
              help_text = "Select a second custom grouping variable",
              choices = c("None" = ""),
              selected = "",
              color = "green"
            )
          )
        )
      )
    )
  )
}

#' Column Mapping Module Server
#'
#' @param id Namespace ID for the module
#' @param uploaded_data Reactive containing uploaded data
#' @return List of reactive values for column selections
mod_column_map_server <- function(id, uploaded_data) {
  shiny::moduleServer(id, function(input, output, session) {

    # Update column selectors when data is uploaded
    shiny::observe({
      raw_uploaded_data <- uploaded_data()
      shiny::req(raw_uploaded_data)

      # Prepare named vector for glassDropdown
      col_names <- names(raw_uploaded_data)
      col_choices <- c("None" = "")
      for (col_name in col_names) {
        col_choices[col_name] <- col_name
      }

      # Update all dropdowns with new column choices
      purrr::walk(COLUMN_SELECTORS, function(selector) {
        updateGlassDropdown(
          session = session,
          inputId = selector,
          choices = col_choices,
          selected = ""
        )
      })
    })

    # Guess column mappings
    shiny::observeEvent(input$guess_cols_btn,
      {
        # Check if raw_uploaded_data is available
        raw_uploaded_data <- NULL
        tryCatch(
          {
            raw_uploaded_data <- uploaded_data()
          },
          error = function(e) {
            raw_uploaded_data <<- NULL
          }
        )

        if (is.null(raw_uploaded_data)) {
          showGlassToast(
            message = paste0(
              "I guess you forgot to upload valid data. ",
              "How about that guess?"
            ),
            title = "AI",
            type = "warning",
            duration = 4000
          )
          return()
        }

        col_names <- names(raw_uploaded_data)
        guesses <- guess_columns(col_names)

        for (selector_name in names(guesses)) {
          if (!is.null(guesses[[selector_name]])) {
            updateGlassDropdown(
              session = session,
              inputId = selector_name,
              selected = guesses[[selector_name]]
            )
          }
        }

        showGlassToast(
          message = "Mappings are guessed. Please double-check!",
          title = "The AI has spoken",
          type = "message",
          duration = 3000
        )
      },
      ignoreInit = TRUE
    )

    # Reset column selections
    shiny::observeEvent(input$reset_cols_btn,
      {
        # Check if raw_uploaded_data is available
        raw_uploaded_data <- NULL
        tryCatch(
          {
            raw_uploaded_data <- uploaded_data()
          },
          error = function(e) {
            raw_uploaded_data <<- NULL
          }
        )

        if (is.null(raw_uploaded_data)) {
          showGlassToast(
            message = paste0(
              "Reset to what? I found no data. ",
              "Upload a file first!"
            ),
            title = "AI",
            type = "warning",
            duration = 3000
          )
          return()
        }

        purrr::walk(COLUMN_SELECTORS, function(selector) {
          updateGlassDropdown(
            session = session,
            inputId = selector,
            selected = ""
          )
        })
        showGlassToast(
          message = "Column selections have been reset.",
          title = "The AI has spoken",
          type = "message",
          duration = 3000
        )
      },
      ignoreInit = TRUE
    )

    # Update dropdown colors based on selection state
    
    # --- Mandatory columns ---
    observe({
      if (!is.null(input$measurement_col) && input$measurement_col != "") {
        updateGlassDropdown(
          session = session,
          inputId = "measurement_col",
          color = "purple"
        )
      } else {
        updateGlassDropdown(
          session = session,
          inputId = "measurement_col",
          color = "green"
        )
      }
    })

    observe({
      if (!is.null(input$subject_id_col) && input$subject_id_col != "") {
        updateGlassDropdown(
          session = session,
          inputId = "subject_id_col",
          color = "purple"
        )
      } else {
        updateGlassDropdown(
          session = session,
          inputId = "subject_id_col",
          color = "green"
        )
      }
    })

    observe({
      if (!is.null(input$sample_id_col) && input$sample_id_col != "") {
        updateGlassDropdown(
          session = session,
          inputId = "sample_id_col",
          color = "purple"
        )
      } else {
        updateGlassDropdown(
          session = session,
          inputId = "sample_id_col",
          color = "green"
        )
      }
    })

    observe({
      if (!is.null(input$replicate_id_col) && input$replicate_id_col != "") {
        updateGlassDropdown(
          session = session,
          inputId = "replicate_id_col",
          color = "purple"
        )
      } else {
        updateGlassDropdown(
          session = session,
          inputId = "replicate_id_col",
          color = "green"
        )
      }
    })

    # --- Optional columns ---
    observe({
      if (!is.null(input$analyte_col) && input$analyte_col != "") {
        updateGlassDropdown(
          session = session,
          inputId = "analyte_col",
          color = "purple"
        )
      } else {
        updateGlassDropdown(
          session = session,
          inputId = "analyte_col",
          color = "green"
        )
      }
    })

    observe({
      if (!is.null(input$material_col) && input$material_col != "") {
        updateGlassDropdown(
          session = session,
          inputId = "material_col",
          color = "purple"
        )
      } else {
        updateGlassDropdown(
          session = session,
          inputId = "material_col",
          color = "green"
        )
      }
    })

    observe({
      if (!is.null(input$sex_col) && input$sex_col != "") {
        updateGlassDropdown(
          session = session,
          inputId = "sex_col",
          color = "purple"
        )
      } else {
        updateGlassDropdown(
          session = session,
          inputId = "sex_col",
          color = "green"
        )
      }
    })

    observe({
      if (!is.null(input$deseases_col) && input$deseases_col != "") {
        updateGlassDropdown(
          session = session,
          inputId = "deseases_col",
          color = "purple"
        )
      } else {
        updateGlassDropdown(
          session = session,
          inputId = "deseases_col",
          color = "green"
        )
      }
    })

    observe({
      if (!is.null(input$group_1_col) && input$group_1_col != "") {
        updateGlassDropdown(
          session = session,
          inputId = "group_1_col",
          color = "purple"
        )
      } else {
        updateGlassDropdown(
          session = session,
          inputId = "group_1_col",
          color = "green"
        )
      }
    })

    observe({
      if (!is.null(input$group_2_col) && input$group_2_col != "") {
        updateGlassDropdown(
          session = session,
          inputId = "group_2_col",
          color = "purple"
        )
      } else {
        updateGlassDropdown(
          session = session,
          inputId = "group_2_col",
          color = "green"
        )
      }
    })

    # Return list of column selections as reactives
    return(
      list(
        measurement_col = shiny::reactive(input$measurement_col),
        subject_id_col = shiny::reactive(input$subject_id_col),
        sample_id_col = shiny::reactive(input$sample_id_col),
        replicate_id_col = shiny::reactive(input$replicate_id_col),
        analyte_col = shiny::reactive(input$analyte_col),
        material_col = shiny::reactive(input$material_col),
        sex_col = shiny::reactive(input$sex_col),
        deseases_col = shiny::reactive(input$deseases_col),
        group_1_col = shiny::reactive(input$group_1_col),
        group_2_col = shiny::reactive(input$group_2_col),
        mandatory_selected = shiny::reactive({
          !is.null(input$measurement_col) && input$measurement_col != "" &&
            !is.null(input$subject_id_col) && input$subject_id_col != "" &&
            !is.null(input$sample_id_col) && input$sample_id_col != "" &&
            !is.null(input$replicate_id_col) && input$replicate_id_col != ""
        })
      )
    )
  })
}