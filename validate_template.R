# ==============================================================================
# load functions
# ==============================================================================

progs <- c(
  "04_validate_template.R",
  "utils.R"
)

purrr::walk(
  .x = progs,
  .f = ~ source(fs::path(here::here(), "R", .x))
)

# ==============================================================================
# select template
# ==============================================================================

cli::cli_h1("Step 1: select a file")

input_file <- select_file(
  folder = here::here(),
  pattern = "^template_[a-z_]+\\.xlsx$"
)

# ==============================================================================
# extract country and language code(s) from template name
# ==============================================================================

cli::cli_h1("Step 2: parsing the file name")

cli::cli_h2("Country code")

template_country <- get_country_code(input_file)

cli::cli_h2("Language code(s)")

template_languages <- get_language_codes(input_file)

# ==============================================================================
# validate template
# ==============================================================================

cli::cli_h1("Step 3: validating the file")

template_input_file <- fs::path(
  here::here(),
  input_file
)

validation_result <- validate_excel_template(
  path = template_input_file,
  country = template_country,
  languages = template_languages,
  schema_path = "inst/schema.yaml"
)

if (!validation_result$valid) {
  cli::cli_abort("Template validation failed. Fix the issues and try again.")
}
