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
