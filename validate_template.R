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

template_country <- input_file |>
	stringr::str_extract(pattern = "(?<=template_)([a-z]{2})(?=_)")

if (is.na(template_country)) {
  cli::cli_abort(
    message = c(
      "x" = "No country code found in the file name.",
      "i" = "Template names should be {.code template_<cc>_<lc1>_<lc2>.xlsx}, where",
      "*" = "{.code <cc>} is the country code, a lowercase, two-letter code",
      "*" = "{.code <lc>} is the language code(s), a lowercase, two-letter code and multiple codes are separated by {.code _}"
    )
  )
} else {
  cli::cli_inform(
    message = c(
      "Country code found: {.code {template_country}}"
    )
  )
}

cli::cli_h2("Language code(s)")

template_languages <- input_file |>
	stringr::str_extract(pattern = "(?<=template_[a-z]{2}_)([a-z_]+)(?=.xlsx)") |>
	stringr::str_split_1(pattern = "_")

if (any(is.na(template_languages))) {
  cli::cli_abort(
    message = c(
      "x" = "No language code found in the file name.",
      "i" = "Template names should be {.code template_<cc>_<lc1>_<lc2>.xlsx}, where",
      "*" = "{.code <cc>} is the country code, a lowercase, two-letter code",
      "*" = "{.code <lc>} is the language code(s), a lowercase, two-letter code and multiple codes are separated by {.code _}"
    )
  )
} else {
  n_codes <- length(template_languages)
  cli::cli_inform(
    message = c(
      "Found {n_codes} language code{?s}: {glue::glue_collapse(template_languages, sep = ', ')}"
    )
  )
}

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
