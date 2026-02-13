# Filter & Name Module
# ------------------------------------------------------------------------------

#' Filter & Name Module UI
#'
#' Creates a card interface for filtering data by various categories and
#' assigning custom names to groups.
#'
#' @param id Namespace ID for the module
#' @return Shiny UI elements
mod_filter_ui <- function(id) {
  ns <- shiny::NS(id)
  glassCard(
    inputId = ns("filter_and_name_card"),
    title = "Filter Data & Assign Names to Groups",
    icon = icon("filter"),
    collapsible = FALSE,
    collapsed = FALSE,
    disabled = FALSE,
    attached = TRUE,
    nested = TRUE,
    width = "100%",
    glassRow(
      glassCol(
        width = 6,
        label = "Filter by Groups",
        label_icon = icon("filter"),
        label_position = "center",
        label_underline = TRUE,
        shiny::uiOutput(ns("analyte_filter_ui")),
        shiny::uiOutput(ns("material_filter_ui")),
        shiny::uiOutput(ns("sex_filter_ui")),
        shiny::uiOutput(ns("group_1_filter_ui")),
        shiny::uiOutput(ns("group_2_filter_ui"))
      ),
      glassCol(
        width = 6,
        label = "Set Names to Groups",
        label_icon = icon("pen"),
        label_position = "center",
        label_underline = TRUE,
        glassTextInput(
          ns("analyte_name"),
          "Analyte Name",
          label_icon = icon("flask"),
          placeholder = "e.g., Glucose",
          color = "green"
        ),
        glassTextInput(
          ns("analyte_material"),
          "Material Name",
          label_icon = icon("droplet"),
          placeholder = "e.g., Plasma",
          color = "green"
        ),
        glassTextInput(
          ns("sex_name"),
          "Sex Name",
          label_icon = icon("venus-mars"),
          placeholder = "e.g., Females",
          color = "green"
        ),
        glassTextInput(
          ns("group_name"),
          "Group Name",
          label_icon = icon("tag"),
          placeholder = "e.g., Healthy",
          color = "green"
        )
      )
    )
  )
}

#' Filter & Name Module Server
#'
#' Server logic for the filter and name module. Creates dynamic filter inputs
#' based on uploaded data and column selections, and manages user-defined names.
#'
#' @param id Namespace ID for the module
#' @param uploaded_data Reactive containing uploaded data frame
#' @param col_selections List of reactive column selections from
#'  column mapping module
#' @return List of reactive values for filters and names
mod_filter_server <- function(id, uploaded_data, col_selections) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Enable debug mode by setting options(shiny.debug = TRUE)
    debug_mode <- getOption("shiny.debug", default = FALSE)

    # --- Safe data accessor (single reactive, evaluated once per change) ---
    safe_data <- shiny::reactive({
      tryCatch(
        uploaded_data(),
        error = function(e) {
          if (debug_mode) message("Error getting data: ", e$message)
          NULL
        }
      )
    })

    # --- Cached choice reactives ---
    analyte_choices <- shiny::reactive({
      current_data <- safe_data()
      column_name <- tryCatch(
        col_selections$analyte_col(),
        error = function(e) NULL
      )
      if (is.null(current_data) ||
          is.null(column_name) ||
          column_name == "" ||
          !column_name %in% names(current_data)) {
        return(character(0))
      }
      unique(current_data[[column_name]])
    })

    material_choices <- shiny::reactive({
      current_data <- safe_data()
      column_name <- tryCatch(
        col_selections$material_col(),
        error = function(e) NULL
      )
      if (is.null(current_data) ||
          is.null(column_name) ||
          column_name == "" ||
          !column_name %in% names(current_data)) {
        return(character(0))
      }
      # Filter by current analyte selection
      selected_analyte <- input$filter_analyte
      if (!is.null(selected_analyte)) {
        analyte_column <- tryCatch(
          col_selections$analyte_col(),
          error = function(e) NULL
        )
        has_analyte_col <-
          !is.null(analyte_column) &&
          analyte_column != "" &&
          analyte_column %in% names(current_data)
        if (has_analyte_col) {
          matching_rows <-
            current_data[[analyte_column]] ==
            selected_analyte
          return(
            unique(
              current_data[[column_name]][
                matching_rows
              ]
            )
          )
        }
      }
      unique(current_data[[column_name]])
    })

    sex_choices <- shiny::reactive({
      current_data <- safe_data()
      column_name <- tryCatch(
        col_selections$sex_col(),
        error = function(e) NULL
      )
      if (is.null(current_data) ||
          is.null(column_name) ||
          column_name == "" ||
          !column_name %in% names(current_data)) {
        return(character(0))
      }
      unique(current_data[[column_name]])
    })

    group_1_choices <- shiny::reactive({
      current_data <- safe_data()
      column_name <- tryCatch(
        col_selections$group_1_col(),
        error = function(e) NULL
      )
      if (is.null(current_data) ||
          is.null(column_name) ||
          column_name == "" ||
          !column_name %in% names(current_data)) {
        return(character(0))
      }
      unique(current_data[[column_name]])
    })

    group_2_choices <- shiny::reactive({
      current_data <- safe_data()
      column_name <- tryCatch(
        col_selections$group_2_col(),
        error = function(e) NULL
      )
      if (is.null(current_data) ||
          is.null(column_name) ||
          column_name == "" ||
          !column_name %in% names(current_data)) {
        return(character(0))
      }
      unique(current_data[[column_name]])
    })

    # Helper: build a filter widget (UI only, no data
    # fetching).
    create_filter_widget <- function(
      mapped_column,
      input_id,
      label,
      icon_name,
      help_text,
      initial_choices,
      multiple = TRUE,
      placeholder = "Choose..."
    ) {
      if (is.null(mapped_column) ||
          mapped_column == "") {
        return(NULL)
      }

      glassSelectizeInput(
        inputId = session$ns(input_id),
        label = label,
        label_icon = shiny::icon(icon_name),
        help_text = help_text,
        choices = initial_choices,
        multiple = multiple,
        placeholder = placeholder
      )
    }

    # --- Render UI ---
    # Only dependent on column mapping (not data).
    # Uses isolate() on choice reactives so data/filter
    # changes don't trigger re-renders (DOM destruction/
    # recreation). Subsequent choice updates are handled
    # by the observer section below.

    output$analyte_filter_ui <- shiny::renderUI({
      .t <- bv_timer_start("filter::render_analyte_ui")
      mapped_column <- tryCatch(
        col_selections$analyte_col(),
        error = function(e) NULL
      )
      result <- create_filter_widget(
        mapped_column = mapped_column,
        input_id = "filter_analyte",
        label = "Filter by Analyte",
        icon_name = "flask",
        help_text = "Select the analyte to analyze",
        initial_choices = isolate(
          analyte_choices()
        ),
        multiple = FALSE,
        placeholder = "Choose an analyte..."
      )
      bv_timer_end(.t)
      result
    })

    output$material_filter_ui <- shiny::renderUI({
      .t <- bv_timer_start("filter::render_material_ui")
      mapped_column <- tryCatch(
        col_selections$material_col(),
        error = function(e) NULL
      )
      result <- create_filter_widget(
        mapped_column = mapped_column,
        input_id = "filter_material",
        label = "Filter by Material",
        icon_name = "droplet",
        help_text = paste(
          "Select one or more sample",
          "materials (e.g., serum, plasma)"
        ),
        initial_choices = isolate(
          material_choices()
        ),
        multiple = TRUE,
        placeholder = "Choose materials..."
      )
      bv_timer_end(.t)
      result
    })

    output$sex_filter_ui <- shiny::renderUI({
      .t <- bv_timer_start("filter::render_sex_ui")
      mapped_column <- tryCatch(
        col_selections$sex_col(),
        error = function(e) NULL
      )
      result <- create_filter_widget(
        mapped_column = mapped_column,
        input_id = "filter_sex",
        label = "Filter by Sex",
        icon_name = "venus-mars",
        help_text = "Select biological sex categories",
        initial_choices = isolate(
          sex_choices()
        ),
        multiple = TRUE,
        placeholder = "Choose sex categories..."
      )
      bv_timer_end(.t)
      result
    })

    output$group_1_filter_ui <- shiny::renderUI({
      .t <- bv_timer_start("filter::render_group1_ui")
      mapped_column <- tryCatch(
        col_selections$group_1_col(),
        error = function(e) NULL
      )
      group_label <- htmltools::HTML(
        paste0(
          "Filter by Group &mdash; ",
          "<span style='",
          "color: #605CA8; ",
          "font-weight: 600;",
          "'>",
          mapped_column,
          "</span>"
        )
      )
      result <- create_filter_widget(
        mapped_column = mapped_column,
        input_id = "filter_group_1",
        label = group_label,
        icon_name = "tag",
        help_text = paste(
          "Select values for",
          mapped_column
        ),
        initial_choices = isolate(
          group_1_choices()
        ),
        multiple = TRUE,
        placeholder = "Choose options..."
      )
      bv_timer_end(.t)
      result
    })

    output$group_2_filter_ui <- shiny::renderUI({
      .t <- bv_timer_start("filter::render_group2_ui")
      mapped_column <- tryCatch(
        col_selections$group_2_col(),
        error = function(e) NULL
      )
      group_label <- htmltools::HTML(
        paste0(
          "Filter by Group &mdash; ",
          "<span style='",
          "color: #605CA8; ",
          "font-weight: 600;",
          "'>",
          mapped_column,
          "</span>"
        )
      )
      result <- create_filter_widget(
        mapped_column = mapped_column,
        input_id = "filter_group_2",
        label = group_label,
        icon_name = "tags",
        help_text = paste(
          "Select values for",
          mapped_column
        ),
        initial_choices = isolate(
          group_2_choices()
        ),
        multiple = TRUE,
        placeholder = "Choose options..."
      )
      bv_timer_end(.t)
      result
    })

    # --- Choice update observers ---
    # When choices change (data updates or analyte filter
    # changes), update widgets in-place via
    # updateGlassSelectizeInput. This avoids the expensive
    # DOM destruction/recreation that renderUI causes.

    shiny::observe({
      available_choices <- analyte_choices()
      column_name <- tryCatch(
        col_selections$analyte_col(),
        error = function(e) NULL
      )
      shiny::req(column_name, column_name != "")
      updateGlassSelectizeInput(
        session,
        "filter_analyte",
        choices = available_choices
      )
    })

    shiny::observe({
      available_choices <- material_choices()
      column_name <- tryCatch(
        col_selections$material_col(),
        error = function(e) NULL
      )
      shiny::req(column_name, column_name != "")
      updateGlassSelectizeInput(
        session,
        "filter_material",
        choices = available_choices
      )
    })

    shiny::observe({
      available_choices <- sex_choices()
      column_name <- tryCatch(
        col_selections$sex_col(),
        error = function(e) NULL
      )
      shiny::req(column_name, column_name != "")
      updateGlassSelectizeInput(
        session,
        "filter_sex",
        choices = available_choices
      )
    })

    shiny::observe({
      available_choices <- group_1_choices()
      column_name <- tryCatch(
        col_selections$group_1_col(),
        error = function(e) NULL
      )
      shiny::req(column_name, column_name != "")
      updateGlassSelectizeInput(
        session,
        "filter_group_1",
        choices = available_choices
      )
    })

    shiny::observe({
      available_choices <- group_2_choices()
      column_name <- tryCatch(
        col_selections$group_2_col(),
        error = function(e) NULL
      )
      shiny::req(column_name, column_name != "")
      updateGlassSelectizeInput(
        session,
        "filter_group_2",
        choices = available_choices
      )
    })

    # Prevent outputs from being suspended when hidden in tabs
    # This ensures filters remain reactive even when tabs are switched
    output_names <- c(
      "analyte_filter_ui",
      "material_filter_ui",
      "sex_filter_ui",
      "group_1_filter_ui",
      "group_2_filter_ui"
    )

    lapply(
      X = output_names,
      FUN = function(name) {
        shiny::outputOptions(output, name, suspendWhenHidden = FALSE)
      }
    )

    # Update input colors based on column mappings
    # When a column is mapped, change the corresponding text input to purple
    shiny::observe({
      analyte_col <- col_selections$analyte_col()
      if (!is.null(analyte_col) && analyte_col != "") {
        updateGlassTextInput(
          session,
          inputId = "analyte_name",
          color = "purple"
        )
      } else {
        updateGlassTextInput(
          session,
          inputId = "analyte_name",
          color = "green"
        )
      }
    })

    shiny::observe({
      material_col <- col_selections$material_col()
      if (!is.null(material_col) && material_col != "") {
        updateGlassTextInput(
          session,
          inputId = "analyte_material",
          color = "purple"
        )
      } else {
        updateGlassTextInput(
          session,
          inputId = "analyte_material",
          color = "green"
        )
      }
    })

    shiny::observe({
      sex_col <- col_selections$sex_col()
      if (!is.null(sex_col) && sex_col != "") {
        updateGlassTextInput(
          session,
          inputId = "sex_name",
          color = "purple"
        )
      } else {
        updateGlassTextInput(
          session,
          inputId = "sex_name",
          color = "green"
        )
      }
    })

    shiny::observe({
      group_1_col <- col_selections$group_1_col()
      if (!is.null(group_1_col) && group_1_col != "") {
        updateGlassTextInput(
          session,
          inputId = "group_name",
          color = "purple"
        )
      } else {
        updateGlassTextInput(
          session,
          inputId = "group_name",
          color = "green"
        )
      }
    })

    # Return reactive values for filters and names
    return(
      list(
        # Filter reactive values
        filter_analyte = shiny::reactive(input$filter_analyte),
        filter_material = shiny::reactive(input$filter_material),
        filter_sex = shiny::reactive(input$filter_sex),
        filter_group_1 = shiny::reactive(input$filter_group_1),
        filter_group_2 = shiny::reactive(input$filter_group_2),

        # Name reactive values
        analyte_name = shiny::reactive(input$analyte_name),
        analyte_material = shiny::reactive(input$analyte_material),
        sex_name = shiny::reactive(input$sex_name),
        group_name = shiny::reactive(input$group_name)
      )
    )
  })
}
