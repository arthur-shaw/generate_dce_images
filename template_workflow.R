# ==============================================================================
# Excel Translation Template Workflow
# ==============================================================================
# This script orchestrates the process of creating translation templates,
# validating filled templates, and exporting translations to YAML format.

# ==============================================================================
# Step 1: Setup and load functions
# ==============================================================================

# Restore the R environment
renv::restore(prompt = FALSE)

# Load helper functions
fs::dir_ls(path = "R", regexp = "0[345]_.*\\.R$") |>
  purrr::walk(.f = ~ source(.x))

# ==============================================================================
# Step 2: Configure template parameters
# ==============================================================================
# Modify these variables to generate/validate/export for a different country

# Country: ISO 3166-1 alpha-2 code
template_country <- "ma"

# Languages: ISO 639-1 codes
# Example: French and Arabic in Morocco
# Example: English and Twi in Ghana
template_languages <- c("fr", "ar")

# ==============================================================================
# Step 3: Generate template
# ==============================================================================
# Run this to create a fresh template for translators

cli::cli_h1("Step 1: Generate Translation Template")

template_output_file <- create_excel_template(
  country = template_country,
  languages = template_languages,
  labels_path = "inst/labels.yaml",
  schema_path = "inst/schema.yaml",
  output_path = NULL  # Use default naming
)

cli::cli_inform(
  c(
    "",
    "Next steps:",
    "1. Open the generated Excel file",
    "2. Review and complete the translations",
    "3. Return to this script and modify 'template_input_file' (below)",
    "4. Run Step 4 to validate and export"
  )
)

# ==============================================================================
# Step 4: Validate filled template
# ==============================================================================
# Run this after translators have completed the template

cli::cli_h1("Step 2: Validate Filled Template")

# Set the path to the filled template file
template_input_file <- "template_ma_fr_ar.xlsx"

# For testing with an unfilled template, we'll comment this out
# Otherwise, run the validation:
# 
# validation_result <- validate_excel_template(
#   path = template_input_file,
#   country = template_country,
#   languages = template_languages,
#   schema_path = "inst/schema.yaml"
# )
#
# if (!validation_result$valid) {
#   cli::cli_abort("Template validation failed. Fix the issues and try again.")
# }

cli::cli_inform(
  c(
    "i" = "Uncomment the validation code above and set template_input_file",
    "i" = "to the path of your filled template to run validation."
  )
)

# ==============================================================================
# Step 5: Export to YAML
# ==============================================================================
# Run this to transform the validated template into YAML format

# template_to_yaml(
#   path = template_input_file,
#   country = template_country,
#   languages = template_languages,
#   schema_path = "inst/schema.yaml",
#   output_path = glue::glue("inst/labels_new_{template_country}_{paste(template_languages, collapse='_')}.yaml")
# )

cli::cli_inform(
  c(
    "i" = "Uncomment the template_to_yaml() call above after successful validation",
    "i" = "This will export the translations as YAML ready for copy-paste into labels.yaml"
  )
)

# ==============================================================================
# Done
# ==============================================================================

cli::cli_h1("Translation Workflow Complete")

cli::cli_inform(
  c(
    "The Excel template has been created.",
    "See the file for instructions on completing translations.",
    "",
    "Once complete, uncomment the validation and export steps above."
  )
)
