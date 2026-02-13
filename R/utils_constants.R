# Constants and Configuration
# ------------------------------------------------------------------------------

#' Default names for expected columns to exist in uploaded data
COLUMN_SELECTORS <- c(
    "measurement_col", "subject_id_col", "sample_id_col",
    "replicate_id_col", "analyte_col", "material_col",
    "sex_col", "deseases_col", "group_1_col", "group_2_col"
)

#' Predefined hyperparameter priors for specific analytes
#' @export
ANALYTE_PRIORS <- list(
    "HbA1c" = list(
        beta = 5, cvi = 1.2, cva = 2.5, cvg = 5.4,
        beta_weakness = 0.1, cvi_weakness = 2, cva_weakness = 0.5, cvg_weakness = 0.15
    ),
    "Glucose" = list(
        beta = 5, cvi = 4.7, cva = 2.5, cvg = 8.0,
        beta_weakness = 0.1, cvi_weakness = 0.2, cva_weakness = 0.5, cvg_weakness = 0.33
    ),
    "ALA" = list(
        beta = 2, cvi = 16.0, cva = 5.0, cvg = 27.0,
        beta_weakness = 0.25, cvi_weakness = 2, cva_weakness = 2, cvg_weakness = 2
    ),
    "PBG" = list(
        beta = 1.8, cvi = 20.0, cva = 8.0, cvg = 30.0,
        beta_weakness = 0.25, cvi_weakness = 2, cva_weakness = 2, cvg_weakness = 2
    ),
    "CRP" = list(
        beta = 1, cvi = 34.7, cva = 5.0, cvg = 87.1,
        beta_weakness = 2, cvi_weakness = 0.2, cva_weakness = 1, cvg_weakness = 0.1
    )
)

#' Analyte name synonym mapping
#' @export
ANALYTE_SYNONYMS <- list(
    "HbA1c" = c(
        "hba1c", "a1c", "hemoglobin a1c", "glycated hemoglobin",
        "glycosylated hemoglobin", "glycohemoglobin", "langtidsblodsukker"
    ),
    "Glucose" = c(
        "glucose", "glukose", "glu", "blood sugar",
        "blodsukker", "p-glucose", "s-glucose", "b-glucose",
        "p-glukose", "s-glukose", "b-glukose", "fbs", "fbg"
    ),
    "ALA" = c(
        "ala", "aminolevulinic acid", "delta-aminolevulinic acid",
        "d-ala", "dala", "5-aminolevulinic acid", "aminolevulinsyre",
        "delta-aminolevulinsyre", "u-ala", "p-ala"
    ),
    "PBG" = c(
        "pbg", "porphobilinogen", "porfobilinogen", "u-pbg"
    )
)
