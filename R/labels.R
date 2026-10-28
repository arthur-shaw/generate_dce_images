#' Get value label for value
#'
#' @param labels Named list. 
#' @param attribute Character. Name of the attribute.
#' @param lang Character. Name of the language
#' @param value Numeric. Value of the level
#'
#' @return Atomic character vector.
#'
#' @importFrom cli cli_abort
get_lbl <- function(
  labels,
  attribute,
  lang,
  value
) {

  tbl_content_node <- labels[["tbl_content"]]

  if (!attribute %in% names(tbl_content_node)) {

    cli::cli_abort(
      message = c(
        "x" = "Invalid attribute: {attribute}"
      )
    )

  }

  attribute_node <- tbl_content_node[[attribute]]

  if (!lang %in% names(attribute_node)) {

    cli::cli_abort(
      message = c(
        "x" = "Invalid language for {attribute}: {lang}"
      )
    )

  }

  attribute_node[[lang]][[value+1]]

}

#' Get label for table stubs and columns
#'
#' @inheritParams get_lbl
#'
#' @return Atomic character vector.
#'
#' @importFrom cli cli_abort
get_tbl_lbl <- function(
  labels,
  attribute,
  lang
) {

  tbl_labels_node <- labels[["tbl_labels"]]

  if (!attribute %in% names(tbl_labels_node)) {

    cli::cli_abort(
      message = c(
        "x" = "Invalid attribute for table labels: {attribute}"
      )
    )

  }

  lang_node <- tbl_labels_node[[attribute]]

  if (!lang %in% names(lang_node)) {

    cli::cli_abort(
      message = c(
        "x" = "Invalid language for table labels: {lang}"
      )
    )

  }

  lang_node[[lang]]

}

#' Create string labels that are the contents of the table
#'
#' @param df Data frame of numeric attribute level selections.
#' @param lbls Named list. Contents of `labels.yaml`.
#' @param lang Character. ISO 639-1 language code.
#'
#' @return Data frame containing string columns:
#' - salary_str
#' - hours_str
#' - location_str
#' - benefits_str
#'
#' @importFrom dplyr rowwise mutate across cur_column ungroup
create_str_lbls <- function(
  df,
  lbls,
  lang
) {

  df_w_strs <- df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dplyr::across(
        .cols = c(hours, location, benefits),
        .fns = ~ get_lbl(
          labels = lbls,
          attribute = dplyr::cur_column(),
          lang = lang,
          value = .x
        ),
        .names = "{.col}_str"
      )
    ) |>
    dplyr::ungroup()

  return(df_w_strs)

}
