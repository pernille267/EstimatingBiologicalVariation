# ==============================================================================
# Stan Model Compilation Utilities
# ==============================================================================

#' Ensure all Stan models are compiled
#'
#' Scans the package's \code{inst/stan} directory for \code{.stan} files with
#' the naming convention \code{stan_model_<NAME>.stan}. For each, it checks
#' whether a corresponding compiled \code{stan_model_<NAME>_compiled.rds} file
#' exists. Any missing compiled models are compiled via
#' \code{rstan::stan_model()} and saved as \code{.rds} files.
#'
#' @param stan_dir Character path to the directory containing \code{.stan}
#'   files. Defaults to the installed package's \code{stan/} directory.
#'   When developing locally (i.e. the package is not installed), you can
#'   pass \code{"inst/stan"} explicitly.
#' @param force Logical. If \code{TRUE}, recompile all models even if compiled
#'   versions already exist. Defaults to \code{FALSE}.
#' @param verbose Logical. If \code{TRUE}, print status messages to the
#'   console. Defaults to \code{TRUE}.
#'
#' @return A named logical vector indicating compilation status for each model.
#'   Names are model identifiers (e.g. \code{"NTT"}, \code{"NNN"}).
#'   Values are \code{TRUE} if the compiled \code{.rds} is now available
#'   (either pre-existing or freshly compiled), \code{FALSE} if compilation
#'   failed.
#'
#' @details
#' The function is designed to be called in two contexts:
#' \enumerate{
#'   \item \strong{At app startup} (inside \code{app_core.R}): automatically
#'     compiles any missing models so the user never hits a "missing model"
#'     error.
#'   \item \strong{Interactively by a developer}: e.g.
#'     \code{ensure_stan_models_compiled("inst/stan", force = TRUE)} to
#'     rebuild all compiled models after editing \code{.stan} source files.
#' }
#'
#' Models are detected by the glob pattern \code{stan_model_*.stan}, excluding
#' any file whose name already contains \code{_compiled}. The model name is
#' extracted from the filename
#' (e.g. \code{stan_model_NTT.stan} -> \code{"NTT"}).
#'
#' @examples
#' \dontrun{
#' # At startup (uses installed package path):
#' ensure_stan_models_compiled()
#'
#' # During development (force recompile):
#' ensure_stan_models_compiled("inst/stan", force = TRUE)
#' }
#'
#' @export
ensure_stan_models_compiled <- function(stan_dir = NULL,
                                        force = FALSE,
                                        verbose = TRUE) {
  # Resolve stan directory

  if (is.null(stan_dir)) {
    stan_dir <- system.file("stan", package = "EstimatingBiologicalVariation")
    if (stan_dir == "") {
      stop(
        "Could not find the 'stan/' directory in the installed package. ",
        "Pass the path explicitly, e.g. ",
        "ensure_stan_models_compiled('inst/stan')."
      )
    }
  }

  if (!dir.exists(stan_dir)) {
    stop("Stan directory does not exist: ", stan_dir)
  }

  # Discover .stan source files (exclude any with "_compiled" in the name)
  stan_files <- list.files(
    path = stan_dir,
    pattern = "^stan_model_.*\\.stan$",
    full.names = TRUE
  )
  stan_files <- stan_files[!grepl("_compiled", basename(stan_files))]

  if (length(stan_files) == 0) {
    if (verbose) message("No .stan model files found in: ", stan_dir)
    return(invisible(logical(0)))
  }

  # Extract model names: stan_model_NTT.stan -> "NTT"
  model_names <- sub(
    pattern = "^stan_model_(.*)\\.stan$",
    replacement = "\\1",
    x = basename(stan_files)
  )
  names(stan_files) <- model_names

  # Build expected compiled file paths
  compiled_paths <- file.path(
    stan_dir,
    paste0("stan_model_", model_names, "_compiled.rds")
  )
  names(compiled_paths) <- model_names

  # Determine which need compilation
  already_compiled <- file.exists(compiled_paths)
  needs_compile <- if (force) {
    rep(TRUE, length(model_names))
  } else {
    !already_compiled
  }
  names(needs_compile) <- model_names

  if (verbose) {
    n_total <- length(model_names)
    n_existing <- sum(already_compiled)
    n_to_compile <- sum(needs_compile)
    message(sprintf(
      "Stan models: %d found, %d already compiled, %d to compile%s.",
      n_total, n_existing, n_to_compile,
      if (force && n_existing > 0) " (force=TRUE)" else ""
    ))
  }

  # Result tracker
  result <- stats::setNames(already_compiled & !force, model_names)

  # Compile missing models
  models_to_compile <- model_names[needs_compile]
  for (i in seq_along(models_to_compile)) {
    m <- models_to_compile[i]
    stan_file <- stan_files[m]
    rds_file <- compiled_paths[m]

    if (verbose) {
      message(sprintf(
        "  [%d/%d] Compiling %s ...",
        i, length(models_to_compile), basename(stan_file)
      ))
    }

    compiled_model <- tryCatch(
      {
        rstan::stan_model(file = stan_file, auto_write = FALSE)
      },
      error = function(e) {
        warning(
          "Failed to compile ", basename(stan_file), ": ", e$message,
          call. = FALSE
        )
        return(NULL)
      }
    )

    if (!is.null(compiled_model)) {
      saveRDS(compiled_model, file = rds_file)
      result[m] <- TRUE
      if (verbose) message("    -> Saved: ", basename(rds_file))
    } else {
      result[m] <- FALSE
      if (verbose) message("    -> FAILED")
    }
  }

  if (verbose) {
    n_ok <- sum(result)
    message(sprintf(
      "Done. %d/%d models ready.", n_ok, length(result)
    ))
  }

  invisible(result)
}


#' Load a compiled Stan model, compiling if necessary
#'
#' Convenience wrapper that loads a single compiled model by name. If the
#' compiled \code{.rds} file doesn't exist, the \code{.stan} source is compiled
#' first.
#'
#' @param model_name Character. The model identifier, e.g. \code{"NTT"}.
#' @param stan_dir Character path to the stan directory.
#'
#' @return The compiled \code{stanmodel} object.
#' @keywords internal
load_or_compile_stan_model <- function(model_name,
                                       stan_dir = NULL) {
  if (is.null(stan_dir)) {
    stan_dir <- system.file("stan", package = "EstimatingBiologicalVariation")
  }

  rds_file <- file.path(stan_dir, paste0("stan_model_", model_name, "_compiled.rds"))
  stan_file <- file.path(stan_dir, paste0("stan_model_", model_name, ".stan"))

  if (file.exists(rds_file)) {
    return(readRDS(rds_file))
  }

  if (!file.exists(stan_file)) {
    stop(
      "Neither compiled model (", basename(rds_file),
      ") nor source (", basename(stan_file),
      ") found in: ", stan_dir
    )
  }

  message("Compiling ", basename(stan_file), " (this may take a minute)...")
  compiled <- rstan::stan_model(file = stan_file, auto_write = FALSE)
  saveRDS(compiled, file = rds_file)
  message("Saved compiled model: ", basename(rds_file))

  compiled
}
