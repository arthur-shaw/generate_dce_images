#' Generate discrete choice images for one country-language
#'
#' @param labels_path Atomic character vector.
#' Path to country-specific labels YAML file.
#' @param layout_path Atomic character vector.
#' Path to country-specific layout YAML file.
#' @param data_path Atomic character vector.
#' Path to the Stata file of discrete choices.
#' @param country Atomic character vector.
#' ISO 3166-1 alpha-2 country code (e.g., "ma", "gn").
#' @param language Atomic character vector.
#' ISO 639-1 language codes (e.g., "fr", "ar", "en", "tw")).
#' @param image_dir Atomic character vector.
#' Path to the top-level directory where country-language images
#' are stored.
#' 
#' @importFrom yaml read_yaml
#' @importFrom haven read_dta
#' @importFrom dplyr rename select
#' @importFrom assertr assert
generate_images <- function(
  labels_path,
  layout_path = here::here("inst", "layout.yaml"),
  data_path = here::here("data", "DCE_Childcare_GS.dta"),
  asset_path = here::here("assets"),
  country,
  language,
  image_dir = here::here("images")
) {

# ==============================================================================
# ingest labels
# ==============================================================================

lbls <- yaml::read_yaml(file = labels_path)

# ==============================================================================
# ingest country-language table/image layout parameters
# ==============================================================================

layout_params <- get_layout_params(
  layout = yaml::read_yaml(layout_path),
  country = country,
  lang = language
)

# ==============================================================================
# ingest choice data
# ==============================================================================

choices_df <- data_path |>
	haven::read_dta() |>
  # rename to match name of key in labels
  dplyr::rename(cost = childcare) |>
	# remove string values
  # these will be replaced with language specific versions
  dplyr::select(-dplyr::ends_with("_str")) |>
  # create new string variables
  create_str_lbls(
    lbls = lbls,
    country = country,
    lang = language
  ) |>
  # keep only those data needed
  # among other reasons to avoid name collision with desired variables
  dplyr::select(
    # image number
    image,
    # choice A (1) or B (2)
    alt,
    # string attribute levels
    cost = cost_str,
    hours = hours_str,
    location =location_str,
    quality = quality_str
  ) |>
  # check that all labels contain `*`
  assertr::assert(
    predicate = \(x) grepl(x = x, pattern = "*"),
    cost, hours, location, quality
  )

# ==============================================================================
# create output directory, if it doesn't already exist
# ==============================================================================

fs::dir_create(
  fs::path(image_dir, country, language)
)

# ==============================================================================
# execute image production pipeline iteratively for each choice
# ==============================================================================

# get a vector of all the choice IDs over which to iterate
choices <- choices_df |>
  dplyr::pull(image) |>
  unique()

purrr::walk(
  .x = choices,
  .f = ~ choices_df |>
    # first, extract data for a choice and reshape to the table format
    prepare_data(
      choice_num = .x,
      cost_text = get_tbl_lbl(
        labels = lbls,
        attribute = "cost",
        lang = language
      ),
      hours_text = get_tbl_lbl(
        labels = lbls,
        attribute = "hours",
        lang = language
      ),
      location_text = get_tbl_lbl(
        labels = lbls,
        attribute = "location",
        lang = language
      ),
      quality_text = get_tbl_lbl(
        labels = lbls,
        attribute = "quality",
        lang = language
      )
    ) |>
    # then, compose a {gt} table and save an image of it
    create_image(
      choice_num = .x,
      asset_path = asset_path,
      option_A_text = get_tbl_lbl(
        labels = lbls,
        attribute = "option_a",
        lang = language
      ),
      option_B_text = get_tbl_lbl(
        labels = lbls,
        attribute = "option_b",
        lang = language
      ),
      attribute_text = get_tbl_lbl(
        labels = lbls,
        attribute = "attribute",
        lang = language
      ),
      country = country,
      lang = language,
      output_dir = image_dir,
      # layout overrides
      col_width_icon    = layout_params$col_width_icon,
      col_width_attribute = layout_params$col_width_attribute,
      col_width_choice  = layout_params$col_width_choice,
      cell_padding      = layout_params$cell_padding,
      image_height      = layout_params$image_height,
      font_size         = layout_params$font_size,
      viewport_width    = layout_params$viewport_width
    ),
  .progress = TRUE
)

}
