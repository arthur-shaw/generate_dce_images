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
