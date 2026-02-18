#' Render a Custom Glass Table (Table-based Layout)
#'
#' Renders a styled data table with optional sidebar, sorting,
#' pagination, and integrated download buttons for CSV and Excel
#' export. Exported data is stripped of any HTML markup.
#'
#' @param data A data.frame or data.table.
#' @param col_names Optional vector of column names.
#' @param caption Optional HTML caption.
#' @param sidebar_html Optional raw HTML for the sidebar.
#' @param sidebar_title Title for the sidebar column.
#' @param highlight_rows Vector of row indices to highlight.
#' @param excluded_rows Vector of row indices to mark
#'  as excluded (red styling).
#' @param highlight_cells A list where names are specific
#'  cell values (strings) that may may appear, and the list
#'  list values are the HTML replacement.
#' @param sortable Logical.
#' @param rounded Character. Where to round corners:
#'   \code{"all"} (default) or \code{"bottom"}
#'   (flat top, rounded bottom).
#' @param downloadable Logical. If \code{TRUE}, render CSV and
#'  Excel download buttons in a toolbar above the table.
#' @param download_filename Character. Base filename (without
#'  extension) used when downloading. Defaults to
#'  \code{"table_export"}.
#'
#' @importFrom htmltools tagList tags htmlDependency HTML
#' @importFrom data.table is.data.table as.data.table
#' @importFrom shiny withMathJax
#' @importFrom jsonlite toJSON
#' @export
renderGlassTable <- function(data, # nolint
                             col_names = NULL,
                             caption = NULL,
                             sidebar_html = NULL,
                             sidebar_title = NULL,
                             highlight_rows = NULL,
                             excluded_rows = NULL,
                             highlight_cells = NULL,
                             sortable = TRUE,
                             rounded = "all",
                             downloadable = FALSE,
                             download_filename = "table_export") {
  # data must be a data.table for efficient processing.
  if (!data.table::is.data.table(data)) {
    data <- tryCatch(
      data.table::as.data.table(data),
      error = function(e) {
        stop(
          "Failed to convert data to data.table: ",
          e$message
        )
      }
    )
  }

  if (is.null(col_names)) {
    col_names <- names(data)
  }

  # --- Build Sidebar (Left) ---------------------------------------------------
  sidebar_col <- NULL
  if (!is.null(sidebar_html)) {
    header_class <- "glass-sidebar-header"
    header_content <- sidebar_title
    if (is.null(sidebar_title) || sidebar_title == "") {
      header_class <- paste(header_class, "empty")
      header_content <- htmltools::HTML("&nbsp;")
    }
    sidebar_col <- htmltools::tags$div(
      class = "glass-table-sidebar-col",
      htmltools::tags$div(
        class = header_class,
        header_content
      ),
      htmltools::tags$div(
        class = "glass-sidebar-body",
        htmltools::HTML(sidebar_html)
      )
    )
  }

  # --- Build Data Table (Right) -----------------------------------------------

  # Table Header (<thead>)
  header_cells <- lapply(seq_along(col_names), function(i) {
    col_name <- col_names[i]
    first_val <- data[[i]][1]

    # Auto-Align: Center numbers, Left strings
    # numeric_ish means it's either numeric or a character that looks like a number
    # (e.g., "12.5", "50%", "1,000")

    # MIGHT NEED FIX: This logic is not perfect. It may misclassify some columns.
    # What is the first value is missing or is not interpretable as numeric?
    # We might want to consider looking at more values to be sure
    # (e.g., check the first 5 non-NA values).
    is_numeric_ish <- is.numeric(first_val) ||
      (is.character(first_val) && grepl("^[<> 0-9.%,]+$", first_val))
    header_align <- if (is_numeric_ish) {
      "center"
    } else {
      "left"
    }

    classes <- paste0("glass-th align-", header_align)
    icon <- ""

    if (sortable) {
      classes <- paste(classes, "sortable")
      icon <- '<i class="fas fa-sort glass-sort-icon"></i>'
    }

    # data-col-index helps JS identify the column easily
    htmltools::tags$th(
      class = classes,
      `data-col-index` = i - 1,
      htmltools::HTML(paste0(col_name, " ", icon))
    )
  })

  thead <- htmltools::tags$thead(
    htmltools::tags$tr(header_cells)
  )

  # Table Body (<tbody>)
  # We construct rows. Using simpler loop for clarity.
  rows_html <- lapply(1:nrow(data), function(i) {
    row_class <- "glass-tr"
    if (!is.null(highlight_rows) && i %in% highlight_rows) {
      row_class <- paste(row_class, "highlight")
    }
    if (!is.null(excluded_rows) && i %in% excluded_rows) {
      row_class <- paste(row_class, "explore-row-excluded")
    }

    cells <- lapply(data, function(col_data) {
      val <- col_data[i]

      # Handle NA values (Return empty cell)
      if (is.na(val)) {
        return(htmltools::tags$td(class = "glass-td", ""))
      }

      # Check for Cell Highlighting (Exact Match)
      val_str <- as.character(val)
      content <- val_str

      if (!is.null(highlight_cells) && val_str %in% names(highlight_cells)) {
        # Use the custom HTML provided in the list
        content <- highlight_cells[[val_str]]
      }

      # Auto-Align Body Cells
      is_numeric_ish <- is.numeric(val) ||
        (is.character(val) && grepl("^[<> 0-9.%,]+$", val))
      body_align <- if (is_numeric_ish) {
        "center"
      } else {
        "left"
      }

      htmltools::tags$td(
        class = paste0("glass-td align-", body_align),
        content
      )
    })

    htmltools::tags$tr(class = row_class, `data-row-index` = i, cells)
  })

  tbody <- htmltools::tags$tbody(rows_html)

  # The Table Element
  table_html <- htmltools::tags$table(
    class = "glass-table",
    thead,
    tbody
  )

  # Scrollable Container for Data
  data_col <- htmltools::tags$div(
    class = "glass-table-data-col",
    table_html
  )

  pagination_footer <- htmltools::tags$div(class = "glass-pagination-container")

  # --- Caption ----------------------------------------------------------------
  caption_el <- if (!is.null(caption)) {
    htmltools::tags$div(
      class = "glass-table-caption",
      htmltools::HTML(caption)
    )
  } else {
    NULL
  }

  # --- Download Toolbar -------------------------------------------------------
  download_toolbar <- NULL
  export_script <- NULL

  if (downloadable) {
    # Strip HTML tags from strings for clean export
    strip_html <- function(x) gsub("<[^>]+>", "", as.character(x))

    export_columns <- vapply(
      col_names, strip_html, character(1),
      USE.NAMES = FALSE
    )
    export_types <- vapply(data, function(col) {
      if (is.numeric(col)) "number" else "string"
    }, character(1), USE.NAMES = FALSE)

    export_rows <- lapply(seq_len(nrow(data)), function(i) {
      lapply(seq_along(data), function(j) {
        val <- data[[j]][i]
        if (is.na(val)) {
          return(NULL)
        }
        if (is.numeric(val)) {
          return(val)
        }
        strip_html(as.character(val))
      })
    })

    export_data <- list(
      columns = export_columns,
      types = export_types,
      rows = export_rows,
      filename = download_filename
    )
    export_json <- jsonlite::toJSON(
      export_data,
      auto_unbox = TRUE, null = "null"
    )

    export_script <- htmltools::tags$script(
      type = "application/json",
      class = "glass-table-export-data",
      htmltools::HTML(as.character(export_json))
    )

    download_toolbar <- htmltools::tags$div(
      class = "glass-table-download-actions",
      htmltools::tags$button(
        class = "glass-table-dl-btn",
        `data-format` = "csv",
        title = "Download as CSV",
        htmltools::tags$i(class = "fas fa-file-csv"),
        " CSV"
      ),
      htmltools::tags$button(
        class = "glass-table-dl-btn",
        `data-format` = "xlsx",
        title = "Download as Excel",
        htmltools::tags$i(class = "fas fa-file-excel"),
        " Excel"
      )
    )
  }

  # Combine caption and download into a toolbar when downloadable
  if (downloadable) {
    toolbar_cls <- "glass-table-toolbar"
    if (is.null(caption_el)) {
      toolbar_cls <- paste(toolbar_cls, "download-only")
    }
    top_section <- htmltools::tags$div(
      class = toolbar_cls,
      caption_el,
      download_toolbar
    )
  } else {
    top_section <- caption_el
  }

  # --- Final Assembly ---------------------------------------------------------
  container_cls <- "glass-table-container"
  if (rounded == "bottom") container_cls <- paste(container_cls, "glass-table-round-bottom")

  ui <- htmltools::tags$div(
    class = container_cls,
    `data-page-size` = "25",
    export_script,
    top_section,
    htmltools::tags$div(
      class = "glass-table-main",
      sidebar_col,
      data_col
    ),
    pagination_footer
  )

  htmltools::tagList(
    shiny::withMathJax(), # Ensure MathJax is available for rendering math in tables
    ui,
    htmltools::htmlDependency(
      name = "glass-table",
      version = "1.0.0",
      src = c(
        file = system.file("assets", package = "EstimatingBiologicalVariation")
      ),
      script = "glass_table.js",
      stylesheet = "glass_table.css"
    )
  )
}
