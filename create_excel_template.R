# ==============================================================================
# load functions
# ==============================================================================

progs <- c(
  "03_create_template.R",
  "utils_country_lang_codes.R"
)

purrr::walk(
  .x = progs,
  .f = ~ source(fs::path(here::here(), "R", .x))
)

# ==============================================================================
# country code
# ==============================================================================

# ------------------------------------------------------------------------------
# ask
# ------------------------------------------------------------------------------

cli::cli_h1("Generate a template")

cli::cli_h2("Step 1: Country")

cli::cli_par()
cli::cli_text(
  cli::col_blue(
    "Provide a lowercase, two-letter country code"
  )
)
cli::cli_end()
cli::cli_par()
cli::cli_text(
  "Use ISO 3166-1 alpha-2 codes as here: ",
  "{.href https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}"
)
cli::cli_text("For example:")
cli::cli_li("{.strong ma} for Morocco ")
cli::cli_li("{.strong gh} for Ghana ")
cli::cli_end()

cli::cli_text("")
cli::cli_text("Type your answer below.")
cli::cli_text("Then, press {.kbd Enter}")

# ------------------------------------------------------------------------------
# prompt
# ------------------------------------------------------------------------------

# stop to ask user for an  answer
# capture a 1-line answer from stdin
template_country <- readLines("stdin", n = 1)

# ------------------------------------------------------------------------------
# check
# ------------------------------------------------------------------------------

check_two_char_code(template_country)

# ==============================================================================
# language code(s)
# ==============================================================================

# ------------------------------------------------------------------------------
# ask
# ------------------------------------------------------------------------------

cli::cli_h2("Step 2: Language(s)")

cli::cli_par()
cli::cli_text(
  cli::col_blue(
    "Provide a lowercase, two-letter language code. ",
    "If more than one language, provide a comma-separted list."
  )
)
cli::cli_end()
cli::cli_par()
cli::cli_text(
  "{.strong If possible}, use ISO 639-1 language codes as as in the Set 1 column here: ",
  "{.href https://en.wikipedia.org/wiki/List_of_ISO_639_language_codes}"
)
cli::cli_li("If only English: {.strong en}")
cli::cli_li("If both Arabic and French: {.strong ar, fr}")
cli::cli_end()
cli::cli_text("")
cli::cli_par()
cli::cli_text(
  "{.strong If no code exists}, try to create one by shortening a three-character code",
  "from the Set 3 column."
)
cli::cli_end()

cli::cli_text("")
cli::cli_text("Type your answer below.")
cli::cli_text("Then, press {.kbd Enter}")

# ------------------------------------------------------------------------------
# prompt
# ------------------------------------------------------------------------------

template_languages <- readLines("stdin", n = 1)

# ------------------------------------------------------------------------------
# process/check
# ------------------------------------------------------------------------------

# convert response into a character vector
# splitting response by `,`
template_languages <- langs_to_chr_vec(template_languages)

# check that each language is of valid form
check_langs(template_languages)

# ==============================================================================
# create Excel template
# ==============================================================================

cli::cli_h1("Translations template creation")

template_output_file <- create_excel_template(
  country = template_country,
  languages = template_languages,
  labels_path = fs::path(here::here(), "inst", "labels.yaml"),
  schema_path = fs::path(here::here(), "inst", "schema.yaml"),
  output_path = NULL  # Use default naming
)

cli::cli_inform(
  c(
    "",
    "Next steps:",
    "1. Open the generated Excel file",
    "2. Review and complete the translations"
  )
)
