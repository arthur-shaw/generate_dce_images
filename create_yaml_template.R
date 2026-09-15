# ==============================================================================
# load functions
# ==============================================================================

progs <- c(
  # for `select_file()`
  "utils.R",
  # for validating Excel and creating YAML templates
  "04_validate_template.R",
  "05_template_to_yaml.R"
)

purrr::walk(
  .x = progs,
  .f = ~ source(fs::path(here::here(), "R", .x))
)

# ==============================================================================
# select Excel template
# ==============================================================================

cli::cli_h1("Step 1: select an Excel file")

file_name_template <- "^template_[a-z_]+\\.xlsx$"

input_file <- select_file(
  folder = here::here(),
  pattern = file_name_template
)

# ==============================================================================
# create template
# ==============================================================================

cli::cli_h1("Step 2: create a YAML template from Excel")

country <- get_country_code(input_file)
languages <- get_language_codes(input_file)

template_to_yaml(
  path = input_file,
  country = country,
  languages = languages,
  schema_path = here::here("inst", "schema.yaml"),
  output_path = here::here(
    "inst",
    glue::glue("labels_new_{country}_{paste(languages, collapse='_')}.yaml")
  )
)
