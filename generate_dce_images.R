# ==============================================================================
# setup
# ==============================================================================

# ------------------------------------------------------------------------------
# load computational environment
# ------------------------------------------------------------------------------

renv::restore(prompt = FALSE)

# ------------------------------------------------------------------------------
# set project parameters
# ------------------------------------------------------------------------------


# note: to set country-language-specific layout paramters, add to layout.yaml
# this file contains
# - default values
# - optional country-language overrides

# ------------------------------------------------------------------------------
# set paths
# ------------------------------------------------------------------------------

proj_dir <- here::here()
script_dir <- fs::path(proj_dir, "R")
data_dir <- fs::path(proj_dir, "data")
image_dir <- fs::path(proj_dir, "images")

# ------------------------------------------------------------------------------
# load programs
# ------------------------------------------------------------------------------

script_dir |>
	fs::dir_ls(regexp = "\\.R") |>
	purrr::walk(.f = ~ source(.x))

# ==============================================================================
# ingest labels
# ==============================================================================

lbls <- yaml::read_yaml(file = fs::path("inst", "labels.yaml"))

# ==============================================================================
# ingest country-language table/image layout parameters
# ==============================================================================

layout_params <- get_layout_params(
  layout = yaml::read_yaml(fs::path("inst", "layout.yaml")),
  country = proj_country,
  lang = proj_lang
)

# ==============================================================================
# ingest choice data
# ==============================================================================

choices_df <- fs::path(data_dir, "DCE_Childcare_GS.dta") |>
	haven::read_dta() |>
  # rename to match name of key in labels
  dplyr::rename(cost = childcare) |>
	# remove string values
  # these will be replaced with language specific versions
  dplyr::select(-dplyr::ends_with("_str")) |>
  # create new string variables
  create_str_lbls(
    lbls = lbls,
    country = proj_country,
    lang = proj_lang
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
  fs::path(image_dir, proj_country, proj_lang)
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
        lang = proj_lang
      ),
      hours_text = get_tbl_lbl(
        labels = lbls,
        attribute = "hours",
        lang = proj_lang
      ),
      location_text = get_tbl_lbl(
        labels = lbls,
        attribute = "location",
        lang = proj_lang
      ),
      quality_text = get_tbl_lbl(
        labels = lbls,
        attribute = "quality",
        lang = proj_lang
      )
    ) |>
    # then, compose a {gt} table and save an image of it
    create_image(
      choice_num = .x,
      option_A_text = get_tbl_lbl(
        labels = lbls,
        attribute = "option_a",
        lang = proj_lang
      ),
      option_B_text = get_tbl_lbl(
        labels = lbls,
        attribute = "option_b",
        lang = proj_lang
      ),
      attribute_text = get_tbl_lbl(
        labels = lbls,
        attribute = "attribute",
        lang = proj_lang
      ),
      country = proj_country,
      lang = proj_lang,
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
