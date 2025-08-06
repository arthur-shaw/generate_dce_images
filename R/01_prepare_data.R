#' Transform choice data into table format
#'
#' @param df Data frame.
#' @param choice_num Numberic. Number of the choice whose data to process.
#'
#' @return Data frame in table format.
#'
#' @importFrom dplyr filter arrange select rename_with mutate case_when across
#' if_else
#' @importFrom tidyr pivot_longer pivot_wider
prepare_data <- function(
  df,
  choice_num,
  cost_text,
  hours_text,
  location_text,
  quality_text,
  perceptions_text,
  food_text,
  cost_icon = "money-bill-1-wave",
  hours_icon = "clock",
  location_icon = "location-dot",
  quality_icon = "clipboard-check",
  perceptions_icon = "scale-unbalanced",
  food_icon = "utensils"
) {

  choice_table_df <- df |>
    # subset to the choice
    dplyr::filter(image == choice_num) |>
    # order by order variable
    dplyr::arrange(alt) |>
    # restructure data to wide format
    # | attribute | choice A | choice B |
    # | --------- | -------- | -------- |
    # |           |          |          |
    dplyr::select(
        alt,
        childcare, hours, location, quality, socialnorms, food
    ) |>
    tidyr::pivot_longer(
      cols = c(
        childcare, hours, location, quality, socialnorms, food
      ),
      names_to = "attribute",
      values_to = "value"
    ) |>
    tidyr::pivot_wider(
      id_cols = c(attribute),
      names_from = alt,
      values_from = value
    ) |>
    dplyr::rename_with(
      .cols = c(`1`, `2`),
      .fn = ~ dplyr::case_when(
        .x == "1" ~ "choice_A",
        .x == "2" ~ "choice_B"
      )
    ) |>
    # adjust the text of the attributes column
    dplyr::mutate(
      attribute = dplyr::case_when(
        attribute == "childcare" ~ cost_text,
        attribute == "hours" ~ hours_text,
        attribute == "location" ~ location_text,
        attribute == "quality" ~ quality_text,
        attribute == "socialnorms" ~ perceptions_text,
        attribute == "food" ~ food_text,
        .default = ""
      )
    ) |>
    # manage highlighting
    # remove if text in choice columns are the same
    # keep if the text is different
    dplyr::mutate(
      # create `_prime` variables to permit reference to original text
      choice_A_prime = choice_A,
      choice_B_prime = choice_B,
      # remove highlighting markers if text is the same across choices
      dplyr::across(
        .cols = c(choice_A, choice_B),
        .fns = ~ dplyr::if_else(
          condition = choice_A_prime == choice_B_prime,
          true = gsub(
            x = .x,
            pattern = "**",
            replacement = "",
            fixed = TRUE
          ),
          false = .x
        )
      )
    ) |>
    # remove `_prime` variables
    dplyr::select(-dplyr::ends_with("_prime")) |>
    # add an icons column
    dplyr::mutate(
      icon = dplyr::case_when(
        attribute == cost_text ~ cost_icon,
        attribute == hours_text ~ hours_icon,
        attribute == location_text ~ location_icon,
        attribute == quality_text ~ quality_icon,
        attribute == perceptions_text ~ perceptions_icon,
        attribute == food_text ~ food_icon,
        .default = ""
      ),
      .after = attribute
    )

  return(choice_table_df)

}

# b <- prepare_data(
#   df = choices_df,
#   choice_num = 1,
#   cost_text = "Childcare services cost",
#   hours_text = "Number of available services hours",
#   location_text = "Location",
#   quality_text = "Quality",
#   perceptions_text = "Perceptions",
#   food_text = "Food"
# )