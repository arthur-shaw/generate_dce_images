#' Transform a filled template into YAML-ready nested list
#'
#' @description
#' Reads a completed translation template Excel file, validates it, and exports
#' the translations as a nested R list structure ready for YAML serialization.
#' The structure mirrors the original labels.yaml format.
#'
#' @param path Character. Path to the filled Excel template file.
#' @param country Character. ISO 3166-1 alpha-2 country code (must match template).
#' @param languages Character vector. ISO 639-1 language codes (must match template).
#' @param schema_path Character. Path to the schema.yaml file
#'   (default: "inst/schema.yaml").
#' @param output_path Character. If provided, writes YAML output to this file.
#'   If NULL, output is only printed to console.
#'
#' @return Invisibly, the nested list structure (also printed to console and/or file).
#'
#' @details
#' **Process:**
#' 1. Validates the template using `validate_excel_template()`.
#'    Aborts with `cli_abort()` if validation fails.
#' 2. Reads each non-Instructions sheet and extracts translations.
#' 3. Builds a nested list with structure:
#'    - For tbl_labels: `list(cost = list(en = ..., fr = ..., ...))`, etc.
#'    - For cost: `list(tbl_content = list(cost = list(ma = list(fr = ..., ar = ..., ...))))`
#'    - For hours/location/quality: `list(tbl_content = list(hours = list(fr = ..., ...)))`
#' 4. If "en" ∉ languages, the "en" column is NOT included in output.
#' 5. Serialises to YAML with `yaml::as.yaml()` and prints to console.
#' 6. If `output_path` is provided, also writes to file.
#'
#' @importFrom readxl read_xlsx excel_sheets
#' @importFrom yaml as.yaml
#' @importFrom cli cli_inform cli_abort
#' @importFrom glue glue
#' @importFrom dplyr tibble
#'
#' @keywords internal
template_to_yaml <- function(
  path,
  country,
  languages,
  schema_path = "inst/schema.yaml",
  output_path = NULL
) {

  # Load schema
  schema <- yaml::read_yaml(file = schema_path)

  # STEP 1: Validate the template
  validation_result <- validate_excel_template(
    path = path,
    country = country,
    languages = languages,
    schema_path = schema_path
  )

  if (!validation_result$valid) {
    cli::cli_abort(
      c(
        "✗" = "Template validation failed: {path}",
        "i" = "Correct the issues listed above and try again."
      )
    )
  }

  # STEP 2: Build the nested output structure
  output_list <- list()

  # Get sheet names
  sheet_names <- readxl::excel_sheets(path)

  # Process tbl_labels
  if ("tbl_labels" %in% sheet_names) {

    tbl_labels_df <- readxl::read_xlsx(path, sheet = "tbl_labels")

    # Build tbl_labels structure: list(cost = list(en = ..., fr = ..., ...))
    tbl_labels_list <- .build_tbl_labels(
      df = tbl_labels_df,
      languages = languages,
      schema = schema
    )

    output_list[["tbl_labels"]] <- tbl_labels_list

  }

  # Process tbl_content sheets
  tbl_content_list <- list()

  for (attr in c("cost", "hours", "location", "quality")) {

    if (!(attr %in% sheet_names)) next

    df <- readxl::read_xlsx(path, sheet = attr)

    if (attr == "cost") {

      # Cost is country- and language-specific
      cost_list <- .build_cost_section(
        df = df,
        country = country,
        languages = languages,
        schema = schema
      )

      tbl_content_list[[attr]] <- cost_list

    } else {

      # Hours, location, quality are language-specific only
      attr_list <- .build_lang_section(
        df = df,
        languages = languages,
        schema = schema
      )

      tbl_content_list[[attr]] <- attr_list

    }

  }

  if (length(tbl_content_list) > 0) {
    output_list[["tbl_content"]] <- tbl_content_list
  }

  # STEP 3: Serialize to YAML
  yaml_string <- yaml::as.yaml(output_list)

  # Print to console
  cli::cli_inform("\n")
  cli::cli_inform("✓ Template export successful: {path}")
  cli::cli_inform("\nYAML output (ready for copy-paste into labels.yaml):\n")
  cli::cli_inform("{yaml_string}")

  # Write to file if specified
  if (!is.null(output_path)) {
    writeLines(yaml_string, con = output_path)
    cli::cli_inform("Saved to: {output_path}")
  }

  invisible(output_list)

}

# Helper: Build tbl_labels structure
.build_tbl_labels <- function(df, languages, schema) {

  # Determine translation columns (all langs if "en" is a deployment lang, else all except "en")
  trans_cols <- if ("en" %in% languages) {
    setdiff(names(df), "key")
  } else {
    setdiff(names(df), c("key", "en"))
  }

  # For each key in the schema, build the language-keyed structure
  keys <- schema$tbl_labels$keys
  result <- list()

  for (key in keys) {
    key_dict <- list()

    for (col in trans_cols) {
      # Find the value for this key and column
      value <- df[df$key == key, col]
      if (length(value) == 1 && !is.na(value)) {
        key_dict[[col]] <- as.character(value)
      } else {
        key_dict[[col]] <- ""
      }
    }

    result[[key]] <- key_dict
  }

  result

}

# Helper: Build cost section (country- and language-specific)
.build_cost_section <- function(df, country, languages, schema) {

  # Determine translation columns
  trans_cols <- if ("en" %in% languages) {
    setdiff(names(df), "key")
  } else {
    setdiff(names(df), c("key", "en"))
  }

  keys <- schema$tbl_content$cost$keys

  # cost → list(country → list(lang → list(key → value)))
  country_dict <- list()

  for (lang in trans_cols) {
    lang_dict <- list()

    for (key in keys) {
      value <- df[df$key == key, lang]
      if (length(value) == 1 && !is.na(value)) {
        lang_dict[[key]] <- as.character(value)
      } else {
        lang_dict[[key]] <- ""
      }
    }

    country_dict[[lang]] <- lang_dict
  }

  # Return nested structure
  list(list(country_dict))[[1]]
  # Actually, we need to wrap this in the country structure
  # Return as named list: list(ma = list(fr = ..., ar = ...))
  named_list <- list()
  named_list[[country]] <- country_dict
  named_list

}

# Helper: Build hours/location/quality section (language-specific only)
.build_lang_section <- function(df, languages, schema) {

  # Determine translation columns
  trans_cols <- if ("en" %in% languages) {
    setdiff(names(df), "key")
  } else {
    setdiff(names(df), c("key", "en"))
  }

  keys <- schema$tbl_content[[df$key[1]]]$keys
  # Actually, we need to get the keys from the df or schema
  # Let me check what attribute this is by looking at what's available
  # Since we don't pass the attribute name, we need to figure it out
  # Let's use the provided df to get all keys
  keys <- df$key

  result <- list()

  for (lang in trans_cols) {
    lang_dict <- list()

    for (key in keys) {
      value <- df[df$key == key, lang]
      if (length(value) == 1 && !is.na(value)) {
        lang_dict[[key]] <- as.character(value)
      } else {
        lang_dict[[key]] <- ""
      }
    }

    result[[lang]] <- lang_dict
  }

  result

}
