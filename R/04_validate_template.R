#' Validate a filled translation template Excel file
#'
#' @description
#' Validates that a completed translation template meets all requirements before
#' export to YAML. Returns a list with validation results and detailed issues.
#'
#' @param path Character. Path to the filled Excel template file.
#' @param country Character. ISO 3166-1 alpha-2 country code (must match template).
#' @param languages Character vector. ISO 639-1 language codes (must match template).
#' @param schema_path Character. Path to the schema.yaml file
#'   (default: "inst/schema.yaml").
#'
#' @return List with two elements:
#'   - `valid` (logical): TRUE if all validations pass, FALSE otherwise.
#'   - `issues` (data.frame): Empty if valid; otherwise contains columns:
#'       - sheet: Sheet name where issue occurred
#'       - key: The row key affected (or "header" for column header issues)
#'       - language: Language column affected (or NA for row-level issues)
#'       - rule: Name of the validation rule that failed
#'       - message: Human-readable description of the issue
#'
#' @details
#' **Validation rules** (in order):
#'
#' 1. **No duplicate column headers**: Each sheet must have unique column names.
#'    Catches accidental duplicates (e.g., two "en" columns).
#'
#' 2. **No empty cells in translation columns**: All cells in language columns
#'    (all languages except "en" if "en" ∉ languages) must be non-empty and non-NA.
#'
#' 3. **Bold markers present**: Every cell in a translation column must contain
#'    at least one pair of double asterisks (\*\*...\*\*).
#'
#' 4. **All expected keys present**: Each sheet's "key" column must contain
#'    exactly the keys defined in schema.yaml; no missing rows.
#'
#' 5. **No extra keys**: The "key" column must not contain any rows absent
#'    from schema.yaml.
#'
#' @importFrom readxl read_xlsx excel_sheets
#' @importFrom yaml read_yaml
#' @importFrom dplyr tibble
#' @importFrom cli cli_inform cli_warn cli_abort
#' @importFrom glue glue
#'
#' @keywords internal
validate_excel_template <- function(
  path,
  country,
  languages,
  schema_path = "inst/schema.yaml"
) {

  # Load schema
  schema <- yaml::read_yaml(file = schema_path)

  # Initialize issues list
  issues_list <- list()

  # Get sheet names from file
  sheet_names <- readxl::excel_sheets(path)

  # Expected sheets
  expected_sheets <- c("Instructions", "tbl_labels", "cost", "hours", "location", "quality")

  if (!identical(sort(sheet_names), sort(expected_sheets))) {
    cli::cli_warn(
      "Sheet names do not match expected: {paste(sheet_names, collapse=', ')}"
    )
  }

  # Validate each content sheet (skip Instructions)
  for (sheet_name in expected_sheets[-1]) {

    if (!(sheet_name %in% sheet_names)) {
      cli::cli_warn(glue::glue("Sheet '{sheet_name}' not found in template"))
      next
    }

    # Determine section and get schema info
    if (sheet_name == "tbl_labels") {
      section <- "tbl_labels"
      indexed_by <- schema$tbl_labels$indexed_by
      keys <- schema$tbl_labels$keys
      attribute <- NULL
    } else {
      section <- "tbl_content"
      attribute <- sheet_name
      indexed_by <- schema$tbl_content[[attribute]]$indexed_by
      keys <- schema$tbl_content[[attribute]]$keys
    }

    # Read sheet
    df <- readxl::read_xlsx(path, sheet = sheet_name)

    # RULE 1: Check for duplicate column headers
    col_duplicates <- which(duplicated(names(df)))
    if (length(col_duplicates) > 0) {
      dup_cols <- names(df)[col_duplicates]
      issues_list[[glue::glue("{sheet_name}_dup_cols")]] <- dplyr::tibble(
        sheet = sheet_name,
        key = "header",
        language = dup_cols,
        rule = "no_duplicate_columns",
        message = glue::glue("Duplicate column header: {paste(dup_cols, collapse=', ')}")
      )
    }

    # Extract language columns (all columns except 'key')
    lang_cols <- setdiff(names(df), "key")

    # Determine which columns are actual translations (not just reference)
    # If "en" is NOT in the languages param, then "en" is reference-only
    # If "en" IS in the languages param, then "en" is a translation column
    translation_cols <- if ("en" %in% languages) {
      lang_cols
    } else {
      setdiff(lang_cols, "en")
    }

    # RULE 2 & 3: Check for empty cells and bold markers in translation columns
    # Note: bold markers only required for tbl_content, not tbl_labels (headers)
    for (col in translation_cols) {

      cells <- df[[col]]

      # RULE 2: No empty cells
      empty_rows <- which(is.na(cells) | cells == "")

      if (length(empty_rows) > 0) {
        row_keys <- df$key[empty_rows]
        issues_list[[glue::glue("{sheet_name}_{col}_empty")]] <- dplyr::tibble(
          sheet = sheet_name,
          key = row_keys,
          language = col,
          rule = "no_empty_cells",
          message = glue::glue("Empty cell in {col} for key: {paste(row_keys, collapse=', ')}")
        )
      }

      # RULE 3: Bold markers present (only for tbl_content, not tbl_labels)
      if (sheet_name != "tbl_labels") {

        no_bold_rows <- which(!grepl(x = cells, pattern = "\\*\\*.*\\*\\*"))

        if (length(no_bold_rows) > 0) {
          row_keys <- df$key[no_bold_rows]
          issues_list[[glue::glue("{sheet_name}_{col}_no_bold")]] <- dplyr::tibble(
            sheet = sheet_name,
            key = row_keys,
            language = col,
            rule = "bold_markers_present",
            message = glue::glue(
              "Missing bold markers (**...**) in {col} for key: {paste(row_keys, collapse=', ')}"
            )
          )
        }

      }

    }

    # RULE 4: All expected keys present
    sheet_keys <- df$key
    missing_keys <- setdiff(keys, sheet_keys)

    if (length(missing_keys) > 0) {
      issues_list[[glue::glue("{sheet_name}_missing_keys")]] <- dplyr::tibble(
        sheet = sheet_name,
        key = missing_keys,
        language = NA_character_,
        rule = "all_expected_keys_present",
        message = glue::glue("Missing key(s): {paste(missing_keys, collapse=', ')}")
      )
    }

    # RULE 5: No extra keys
    extra_keys <- setdiff(sheet_keys, keys)

    if (length(extra_keys) > 0) {
      issues_list[[glue::glue("{sheet_name}_extra_keys")]] <- dplyr::tibble(
        sheet = sheet_name,
        key = extra_keys,
        language = NA_character_,
        rule = "no_extra_keys",
        message = glue::glue("Unexpected key(s): {paste(extra_keys, collapse=', ')}")
      )
    }

  }

  # Combine all issues into a single data frame
  if (length(issues_list) > 0) {
    all_issues <- do.call(rbind, issues_list)
    rownames(all_issues) <- NULL
  } else {
    all_issues <- dplyr::tibble(
      sheet = character(),
      key = character(),
      language = character(),
      rule = character(),
      message = character()
    )
  }

  # Print summary
  if (nrow(all_issues) > 0) {
    cli::cli_inform("\n")
    cli::cli_warn("Validation failed with {nrow(all_issues)} issue(s):")
    cli::cli_inform("{.file {path}}")
    cli::cli_inform("\nIssues:")
    print(all_issues)
  } else {
    cli::cli_inform("✓ Validation passed: {.file {path}}")
  }

  # Return result
  list(
    valid = nrow(all_issues) == 0,
    issues = all_issues
  )

}
