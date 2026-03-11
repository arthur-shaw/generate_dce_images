# ==============================================================================
# setup
# ==============================================================================

# ------------------------------------------------------------------------------
# load computational environment
# ------------------------------------------------------------------------------

renv::restore(prompt = FALSE)

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
      cost_text = "تكلفة خدمات الحدائق",
      hours_text = "عدد ساعات الخدمة المتاحة",
      location_text = "الموقع",
      quality_text = "الجودة"
    ) |>
    # then, compose a {gt} table and save an image of it
    create_image(
      choice_num = .x,
      option_A_text = "الخيار أ",
      option_B_text = "الخيار ب",
      attribute_text = "يصف",
      lang = "ar",
      output_dir = image_dir
    ),
  .progress = TRUE
)
