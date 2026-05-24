#' Create a translation template Excel workbook
#'
#' @description
#' Generates a country- and language-specific Excel template for translating
#' DCE labels. The template includes pre-filled values from the existing YAML
#' (if they exist) and blank cells where new translations are needed.
#'
#' @param country Character. ISO 3166-1 alpha-2 country code (e.g., "ma", "gn").
#' @param languages Character vector. ISO 639-1 language codes
#'   (e.g., c("fr", "ar"), c("en", "tw")).
#' @param labels_path Character. Path to the existing labels.yaml file
#'   (default: "inst/labels.yaml").
#' @param schema_path Character. Path to the schema.yaml file
#'   (default: "inst/schema.yaml").
#' @param output_path Character. Path where the Excel file will be saved.
#'   If NULL, will be constructed as "template_{country}_{paste(languages, collapse='_')}.xlsx"
#'   in the current working directory.
#'
#' @return NULL (invisibly). Writes an Excel file to disk.
#'
#' @details
#' **Template structure:**
#' - Sheet 1: Instructions — explains the format and bold-marker convention.
#' - Sheets 2+: tbl_labels, cost, hours, location, quality.
#'   Each sheet has columns: key (locked), en (reference/editable), then one column per language.
#'
#' **Pre-filling logic:**
#' - For `tbl_labels`: looks up `labels$tbl_labels[[key]][[lang]]`.
#' - For content attributes (cost, hours, location, quality):
#'   - If cost: looks up `labels$tbl_content$cost[[country]][[lang]][[key]]`.
#'   - Otherwise: looks up `labels$tbl_content[[attr]][[lang]][[key]]`.
#' - If a value exists in the YAML, it is pre-filled in the template; otherwise the cell is blank.
#'
#' **English column:**
#' - The `en` column is always the first language column, always editable.
#' - If `"en"` ∈ `languages`, the `en` column IS the deployment translation for English.
#' - If `"en"` ∉ `languages`, the `en` column is reference-only.
#'
#' @importFrom yaml read_yaml
#' @importFrom openxlsx2 wb_workbook wb_add_worksheet wb_add_data
#'   wb_set_col_widths wb_freeze_panes wb_add_style
#' @importFrom glue glue
#' @importFrom cli cli_inform
#' @importFrom purrr map_df
#' @importFrom dplyr tibble
#'
#' @keywords internal
create_excel_template <- function(
  country,
  languages,
  labels_path = "inst/labels.yaml",
  schema_path = "inst/schema.yaml",
  output_path = NULL
) {

  # Load YAML files
  labels <- yaml::read_yaml(file = labels_path)
  schema <- yaml::read_yaml(file = schema_path)

  # Construct output path if not provided
  if (is.null(output_path)) {
    lang_str <- paste(languages, collapse = "_")
    output_path <- glue::glue("template_{country}_{lang_str}.xlsx")
  }

  # Create workbook
  wb <- openxlsx2::wb_workbook()

  # Add Instructions sheet
  wb <- .add_instructions_sheet(wb)

  # Add tbl_labels sheet
  wb <- .add_content_sheet(
    wb = wb,
    sheet_name = "tbl_labels",
    schema = schema,
    labels = labels,
    section = "tbl_labels",
    country = NULL,
    languages = languages,
    indexed_by = schema$tbl_labels$indexed_by,
    keys = schema$tbl_labels$keys
  )

  # Add tbl_content sheets (cost, hours, location, quality)
  for (attr in c("cost", "hours", "location", "quality")) {

    wb <- .add_content_sheet(
      wb = wb,
      sheet_name = attr,
      schema = schema,
      labels = labels,
      section = "tbl_content",
      attribute = attr,
      country = if (attr == "cost") country else NULL,
      languages = languages,
      indexed_by = schema$tbl_content[[attr]]$indexed_by,
      keys = schema$tbl_content[[attr]]$keys
    )

  }

  # Write to file
  openxlsx2::wb_save(wb, file = output_path, overwrite = TRUE)

  cli::cli_inform(
    c(
      "✓" = "Template created: {output_path}",
      "*" = "Sheets: Instructions, tbl_labels, cost, hours, location, quality",
      "*" = "Share with translators for {length(languages)} language(s): {paste(languages, collapse = ', ')}"
    )
  )

  invisible(output_path)

}

# Helper: Add instructions sheet
.add_instructions_sheet <- function(wb) {

  instructions_text <- c(
    "DCE LABELS TRANSLATION TEMPLATE",
    "",
    "Instructions:",
    "1. The 'key' column (first column in each sheet) is locked and should not be edited.",
    "2. The 'en' column provides a reference translation. You may edit it if providing",
    "   English translations for your country.",
    "3. Fill in the remaining language columns with translations for each key.",
    "4. IMPORTANT: Every translation must include at least one pair of double asterisks",
    "   (**word**) to mark emphasis. For example:",
    "     **Loin** de chez moi (à plus de 15 minutes)",
    "     فقط **في الصباح**",
    "5. Do not leave cells empty in the language columns you are translating.",
    "6. Save the file and return it for validation.",
    "",
    "Sheets in this template:",
    "- tbl_labels: Table column headers (cost, hours, location, quality, attribute, option_a, option_b)",
    "- cost: Cost levels (lvl_0 through lvl_5) — country- and language-specific",
    "- hours: Service hours options (only_morning, only_afternoon, all_day)",
    "- location: Distance options (far, close)",
    "- quality: Quality levels (not_trained, trained)"
  )

  wb <- openxlsx2::wb_add_worksheet(
    wb = wb,
    sheet = "Instructions",
    grid_lines = FALSE
  )

  # Write instructions as a single data frame column
  instructions_df <- data.frame(
    text = instructions_text,
    stringsAsFactors = FALSE
  )

  wb <- openxlsx2::wb_add_data(
    wb = wb,
    sheet = "Instructions",
    x = instructions_df,
    col_names = FALSE,
    row_names = FALSE
  )

  # Set column width
  wb <- openxlsx2::wb_set_col_widths(
    wb = wb,
    sheet = "Instructions",
    cols = 1,
    widths = 80
  )

  wb

}

# Helper: Add a content sheet (tbl_labels or tbl_content sections)
.add_content_sheet <- function(
  wb,
  sheet_name,
  schema,
  labels,
  section,
  attribute = NULL,
  country = NULL,
  languages,
  indexed_by,
  keys
) {

  # Build data frame: key | en | lang1 | lang2 | ...
  df <- dplyr::tibble(
    key = keys
  )

  # Add 'en' column (always present)
  en_values <- purrr::map_chr(
    keys,
    .f = \(k) .get_label(
      labels = labels,
      section = section,
      attribute = attribute,
      country = country,
      lang = "en",
      key = k
    )
  )
  df$en <- en_values

  # Add a column for each language (in the requested order)
  for (lang in languages) {

    col_name <- lang

    lang_values <- purrr::map_chr(
      keys,
      .f = \(k) .get_label(
        labels = labels,
        section = section,
        attribute = attribute,
        country = country,
        lang = lang,
        key = k
      )
    )

    df[[col_name]] <- lang_values

  }

  # Add worksheet
  wb <- openxlsx2::wb_add_worksheet(wb = wb, sheet = sheet_name)

  # Add data (with column headers)
  wb <- openxlsx2::wb_add_data(
    wb = wb,
    sheet = sheet_name,
    x = df,
    col_names = TRUE,
    row_names = FALSE
  )

  # Note: freezing panes not currently supported in openxlsx2 calls
  # Users can manually freeze in Excel if needed

  # Set column widths dynamically based on actual columns
  # Columns: key, en, lang1, lang2, ...
  col_count <- ncol(df)
  col_widths <- c(20, 25, rep(25, col_count - 2))
  
  wb <- openxlsx2::wb_set_col_widths(
    wb = wb,
    sheet = sheet_name,
    cols = 1:col_count,
    widths = col_widths
  )

  wb

}

# Helper: Retrieve a label from the labels structure
# Returns empty string if not found
.get_label <- function(
  labels,
  section,
  attribute = NULL,
  country = NULL,
  lang,
  key
) {

  value <- tryCatch(
    expr = {

      if (section == "tbl_labels") {

        labels$tbl_labels[[key]][[lang]]

      } else if (section == "tbl_content") {

        if (attribute == "cost") {

          # retrieve country-language-level values if they exist
          lbls <- labels$tbl_content$cost[[country]][[lang]][[key]]

          # if no values found for English
          # use template values
          if (lang == "en" & is.null(lbls)) {
            lbls <- labels$tbl_content$cost[["_template"]][[lang]][[key]]
          }
          lbls

        } else {

          labels$tbl_content[[attribute]][[lang]][[key]]

        }

      } else {

        NULL

      }

    },
    error = function(e) NULL
  )

  # Convert NULL or NA to empty string
  if (is.null(value) || is.na(value)) {
    return("")
  }

  as.character(value)

}
