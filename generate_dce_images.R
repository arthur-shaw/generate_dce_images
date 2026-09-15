# ==============================================================================
# bootstrap environment
# ==============================================================================

progs <- c(
  # for: `select_file()`, `get_country_code()`, `get_language_codes()`
  "utils.R",
  # for `generate_images()` and supporting functions
  "06_generate_images.R",
  "01_prepare_data.R",
  "02_create_image.R",
  "labels.R"
)

purrr::walk(
  .x = progs,
  .f = ~ source(fs::path(here::here(), "R", .x))
)

# ==============================================================================
# select template
# ==============================================================================

cli::cli_h1("Step 1: select a file")

file_name_template <- "^labels_new_[a-z_]+\\.yaml$"

input_file <- select_file(
  folder = here::here("inst"),
  pattern = file_name_template
)

# ==============================================================================
# select language
# ==============================================================================

cli::cli_h1("Step 2: select a language")

# ------------------------------------------------------------------------------
# extract
# ------------------------------------------------------------------------------

country <- get_country_code(input_file)
languages <- get_language_codes(input_file)

# ------------------------------------------------------------------------------
# select
# ------------------------------------------------------------------------------

fzf_args <- c(
  "--header='Use UP/DOWN arrows to select, then press ENTER to confirm.'",
  "--header-first",           # Puts the instructions at the very top
  "--layout=reverse",         # Puts the prompt at the top (more natural for users)
  "--border=rounded",         # Adds a clean visual box around the interface
  "--info=inline"            # Cleans up the file counter layout
)

language <- system2(
  command = "fzf",
  args = shQuote(fzf_args),
  input = languages,
  stdout = TRUE,
  stderr = TRUE
)

# ==============================================================================
# select template
# ==============================================================================

cli::cli_h1("Step 3: generate images ")

cli::cli_inform(
  message = c(
    "i" = "Images being generated for:",
    "*" = "Country: {country}",
    "*" = "Language: {language}"
  )
)

generate_images(
  labels_path = here::here("inst", input_file),
  country = country,
  language = language
)
