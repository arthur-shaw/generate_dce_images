#' Check two-character codes
#'
#' @param x Character vector.
#'
#' @return None. Side-effect or aborting if error(s) found.
#'
#' @importFrom cli cli_abort
check_two_char_code <- function(x) {

  if (nchar(x) != 2) {
    cli::cli_abort(
      message = c(
        "x" = "Code is not two-characters long.",
        "i" = "You entered: {x}"
      )
    )
  }

  if (!grepl(x = x, pattern = "[a-z]{2}")) {
    cli::cli_abort(
      message = c(
        "x" = "Code does not consist of only lower-case letters.",
        "i" = "You entered {x}"
      )
    )
  }

}

#' Convert language string to a character vector
#'
#' @description
#' Splits the input vector by `,` and removes any outside whitespace.
#'
#' @return Character vector.
#'
#' @importFrom stringr str_split_1 str_trim
langs_to_chr_vec <- function(x) {

  if (grepl(x = x, pattern = ",")) {

    lang_vec <- x |>
      stringr::str_split_1(pattern = ",") |>
      stringr::str_trim(side = "both")

  } else {

    lang_vec <- x

  }

  return(lang_vec)

}

#' Check language input
#'
#' @description
#' Applies `check_two_char_code()` to each element of the language character
#' vector.
#'
#' @importFrom purrr walk
check_langs <- function(x) {

  purrr::walk(
    .x = x,
    .f = ~ check_two_char_code(.x)
  )

}
