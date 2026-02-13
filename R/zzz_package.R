#' @keywords internal
"_PACKAGE"

#' EstimatingBiologicalVariation: Bayesian Biological Variation Analysis
#'
#' A Shiny application for estimating biological variation components
#' (analytical, within-individual, and between-individual) using Bayesian
#' hierarchical models fitted via Stan. Includes Rcpp and RcppArmadillo
#' routines for efficient variance-component calculations.
#'
#' @name EstimatingBiologicalVariation-package
#' @aliases EstimatingBiologicalVariation
#'
#' @import shiny
#' @import data.table
#' @import ggplot2
#' @importFrom rstan sampling extract
#' @importFrom readxl read_excel excel_sheets
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @importFrom writexl write_xlsx
#' @importFrom truncnorm rtruncnorm dtruncnorm
#' @importFrom purrr map map_chr map_lgl map_dbl
#' @importFrom htmltools tags tagList div span h3 h4 h5 HTML
#' @importFrom stats qnorm qchisq rnorm dnorm sd var median quantile
#' @importFrom utils head tail
#' @importFrom Rcpp sourceCpp
#' @useDynLib EstimatingBiologicalVariation, .registration = TRUE
NULL
