#' Select a matching file in the target directory
#'
#' @param path Atomic character vector. File path of the target directory.
#' @param pattern Atomic character vector. Pattern of the file(s) to match.
#'
#' @return Character. File name of the selected, matching file.
#'
#' @importFrom cli cli_abort cli_inform
select_file <- function(
  folder,
  pattern
) {
  # 1. Find candidate files
  files <- list.files(
    path = folder,
    pattern = pattern
  )

  # 2. Fail before invoking fzf if there are none
  if (length(files) == 0) {
    cli::cli_abort(
      message = c(
        "!" = "No template file was found in the project directory.",
        "Project directory: {.file {folder}}",
        "i" = "Expected a filename matching {.code {pattern}}."
      )
    )
  }

  cli::cli_inform(
    message = c(
      "i" = "Please select the file on the next screen",
      "To do so:",
      "*" = "First, use the UP/DOWN arrows to select the file.",
      "*" = "Then, press ENTER to confirm the seleciton.",
      "If there is no file, press ESC to exit.",
      "Then, put a copy of your desired template in the project directory.",
      "The file name must match the following pattern: {.code {pattern}}",
      "Its directory must be: {.file {folder}}"
    )
  )
  Sys.sleep(3)

  fzf_args <- c(
    "--header='Use UP/DOWN arrows to select, then press ENTER to confirm.'",
    "--header-first",           # Puts the instructions at the very top
    "--layout=reverse",         # Puts the prompt at the top (more natural for users)
    "--border=rounded",         # Adds a clean visual box around the interface
    "--info=inline"            # Cleans up the file counter layout
  )

  # 3. Give the candidate paths to fzf
  result <- system2(
    command = "fzf",
    args = shQuote(fzf_args),
    input = files,
    stdout = TRUE,
    stderr = TRUE
  )

  # 4. Deal with selection/cancellation
  if (length(result) == 0 || !nzchar(result)) {
    cli::cli_abort(
      message = "No file selected"
    )
  }

  result

}

#' Get country code from the file name
#'
#' @description
#' Both the Excel and YAML stores of image labels encode
#' the country name in the file name.
#'
#' @param file_name Character. File name--importantly, with extension.
#'
#' @return Atomic character vector.
#'
#' @importFrom fs path_ext
#' @importFrom dplyr case_when
#' @importFrom stringr str_extract
#' @importFrom cli cli_abort cli_inform
get_country_code <- function(
  file_name
) {

  file_extension <- fs::path_ext(file_name)

  if (file_extension == "") {

    cli::cli_abort(
      message = c(
        "x" = "No file extension found in {.arg file_name}",
        "The program needs the file name with file extension to determine how to find the country code in it"
      )
    )

  }

  file_name_template <- dplyr::case_when(
    file_extension == "xlsx" ~  "(?<=template_)([a-z]{2})(?=_)",
    file_extension == "yaml" ~  "(?<=labels_new_)([a-z]{2})(?=_)",
    .default = NA_character_
  )

  if (is.na(file_name_template)) {

    cli::cli_abort(
      message = c(
        "x" = "Unexpected file extension found: {.value {file_extension}}",
        "This program only supports {.value xlsx} and {.value yaml}  extensions."
      )
    )

  }

  country_code <- file_name |>
    stringr::str_extract(pattern = file_name_template)

  if (is.na(country_code)) {
    cli::cli_abort(
      message = c(
        "x" = "No country code found in the file name.",
        "i" = "Template names should be {.code template_<cc>_<lc1>_<lc2>.{file_extension}}, where",
        "*" = "{.code <cc>} is the country code, a lowercase, two-letter code",
        "*" = "{.code <lc>} is the language code(s), a lowercase, two-letter code and multiple codes are separated by {.code _}"
      )
    )
  } else {
    cli::cli_inform(
      message = c(
        "Country code found: {.code {country_code}}"
      )
    )
  }

  return(country_code)

}

#' Get language code(s) from the file name
#'
#' @description
#' Both the Excel and YAML stores of image labels encode
#' the language codes in the file name.
#'
#' @param file_name Character. File name--importantly, with extension.
#'
#' @return Character vector.
#'
#' @importFrom fs path_ext
#' @importFrom dplyr case_when
#' @importFrom stringr str_extract str_split_1
#' @importFrom cli cli_abort cli_inform
get_language_codes <- function(
  file_name
) {

  file_extension <- fs::path_ext(file_name)

  if (file_extension == "") {

    cli::cli_abort(
      message = c(
        "x" = "No file extension found in {.arg file_name}",
        "The program needs the file name with file extension to determine how to find the country code in it"
      )
    )

  }

  file_name_template <- dplyr::case_when(
    file_extension == "xlsx" ~ "(?<=template_[a-z]{2}_)([a-z_]+)(?=.xlsx)",
    file_extension == "yaml" ~  "(?<=labels_new_[a-z]{2}_)([a-z_]+)(?=.yaml)",
    .default = NA_character_
  )

  if (is.na(file_name_template)) {

    cli::cli_abort(
      message = c(
        "x" = "Unexpected file extension found: {.value {file_extension}}",
        "This program only supports {.value xlsx} and {.value yaml}  extensions."
      )
    )

  }

  language_part <- stringr::str_extract(
    string = file_name,
    pattern = file_name_template
  )

  if (is.na(language_part)) {

    cli::cli_abort(
      message = c(
        "x" = "No language component found in the file name."
      )
    )

  }

  language_codes <- stringr::str_split_1(
    string = language_part,
    pattern = "_"
  )

  if (any(is.na(language_codes))) {
    cli::cli_abort(
      message = c(
        "x" = "No language code found in the file name.",
        "i" = "Template names should be {.code template_<cc>_<lc1>_<lc2>.{file_extension}}, where",
        "*" = "{.code <cc>} is the country code, a lowercase, two-letter code",
        "*" = "{.code <lc>} is the language code(s), a lowercase, two-letter code and multiple codes are separated by {.code _}"
      )
    )
  } else {
    n_codes <- length(language_codes)
    cli::cli_inform(
      message = c(
        "Found {n_codes} language code{?s}: {glue::glue_collapse(language_codes, sep = ', ')}"
      )
    )
  }

  return(language_codes)

}

#' Get resolved layout parameters for a country/language
#'
#' Starts from defaults, then overlays any country + language overrides.
#'
#' @param layout List. Parsed layout YAML.
#' @param country Character. ISO 3166-1 alpha-2 country code.
#' @param lang Character. ISO 639 language code.
#' @return Named list of layout parameters.
get_layout_params <- function(layout, country, lang) {
  params <- layout$defaults
  overrides <- layout$overrides[[country]][[lang]]
  if (!is.null(overrides)) {
    params <- utils::modifyList(params, overrides)
  }
  params
}
