#' Get value label for value
#'
#' @param labels Named list. 
#' @param attribute Character. Name of the attribute.
#' @param country Character. Name of the country.
#' @param lang Character. Name of the language
#' @param value Numeric. Value of the level
#'
#' @return Atomic character vector.
#'
#' @importFrom cli cli_abort
get_lbl <- function(
  labels,
  attribute,
  country,
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

  if (attribute == "cost") {

    if (!country %in% names(attribute_node)) {

      cli::cli_abort(
        message = c(
          "x" = "Invalid country for {attribute}: {country}"
        )
      )

    }

    country_node <- attribute_node[[country]]

    if (!lang %in% names(country_node)) {

      cli::cli_abort(
        message = c(
          "x" = "Invalid language for {attribute} > {country}: {lang}"
        )
      )

    }

    country_node[[lang]][[value+1]]

  } else {

    if (!lang %in% names(attribute_node)) {

      cli::cli_abort(
        message = c(
          "x" = "Invalid language for {attribute}: {lang}"
        )
      )

    }

    attribute_node[[lang]][[value+1]]

  }

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
#' @param country Character. ISO 3166-1 alpha-2 country codes.
#' @param lang Character. ISO 639-1 language code.
#'
#' @return Data frame containing string columns:
#' - cost_str
#' - hours_str
#' - location_str
#' - quality_str
#'
#' @importFrom dplyr rowwise mutate across cur_column ungroup
create_str_lbls <- function(
  df,
  lbls,
  country,
  lang
) {

  df_w_strs <- df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dplyr::across(
        .cols = c(cost, hours, location, quality),
        .fns = ~ get_lbl(
          labels = lbls,
          attribute = dplyr::cur_column(),
          country = country,
          lang = lang,
          value = .x
        ),
        .names = "{.col}_str"
      )
    ) |>
    dplyr::ungroup()

  return(df_w_strs)

}
