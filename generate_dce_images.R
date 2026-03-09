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

choices_df <- fs::path(data_dir, "DCE_Childcare_MA_AR.dta") |>
	haven::read_dta() |>
  # keep only those data needed
  # among other reasons to avoid name collision with desired variables
  dplyr::select(
    # image number
    image,
    # choice A (1) or B (2)
    alt,
    # string attribute levels
    childcare_str, hours_str, location_str, quality_str
  ) |>
  # if `**` is missing, provide it
  # otherwise, keep text the same
	# dplyr::mutate(
  #   cost = dplyr::case_when(
  #     childcare_str == "0 GNF/mois" ~ "**0** GNF/mois",
  #     childcare_str == "75,000 GNF/mois" ~ "**75 000** GNF/mois",
  #     childcare_str == "150,000 GNF/mois" ~ "**150 000** GNF/mois",
  #     childcare_str == "225,000 GNF/mois" ~ "**225 000** GNF/mois",
  #     childcare_str == "300,000 GNF/mois" ~ "**300 000** GNF/mois",
  #     childcare_str == "375,000 GNF/mois" ~ "**375 000** GNF/mois",
  #     .default = childcare_str
  #   ),
  #   # replace any remaining commas with a space as a French thousands separator
  #   # in case the rules above failed to address any separators
  #   cost = dplyr::if_else(
  #     condition = grepl(x = cost, pattern = ",", fixed = TRUE),
  #     true = sub(x = cost, pattern = ",", replacement = " ", fixed = TRUE),
  #     false = cost
  #   ),
  #   hours = dplyr::case_when(
  #     hours_str == "Uniquement le matin" ~ "Uniquement **le matin**",
  #     hours_str == "Uniquement l'après-midi" ~ "Uniquement **l'après-midi**",
  #     hours_str == "Matin et après-midi" ~ "**Matin et après-midi**",
  #     .default = hours_str
  #   ),
  #   location = dplyr::case_when(
  #     # shorten and add markup
  #     location_str == "La garderie est loin de chez moi (à plus de 15 minutes)" ~
  #       "**Loin** de chez moi (à plus de 15 minutes)",
  #     location_str == "La garderie est proche de chez moi (à moins de 15 minutes" ~
  #       "**Proche** de chez moi (à moins de 15 minutes)",
  #     location_str == "La garderie est proche de chez moi (à moins de 15 minutes)" ~
  #       "**Proche** de chez moi (à moins de 15 minutes)",
  #     # shorten only
  #     location_str == "La garderie **est loin** de chez moi (à plus de 15 minutes)" ~
  #       "**Loin** de chez moi (à plus de 15 minutes)",
  #     location_str == "La garderie **est loin** de chez moi (à plus de 15 minutes" ~
  #       "**Loin** de chez moi (à plus de 15 minutes)",
  #     location_str == "La garderie **est proche** de chez moi (à moins de 15 minutes)" ~
  #       "**Proche** de chez moi (à moins de 15 minutes)",
  #     location_str == "La garderie **est proche** de chez moi (à moins de 15 minutes" ~
  #       "**Proche** de chez moi (à moins de 15 minutes)",
  #     .default = location_str
  #   ),
  #   quality = dplyr::case_when(
  #     # shorten
  #     quality_str == "La garderie a un personnel **formé** qui proposent des activités stimulantes" ~
  #       "Avec un personnel **formé** qui proposent des activités stimulantes",
  #     quality_str == "La garderie a un personnel **non-formé**" ~
  #       "Avec un personnel **non-formé**",
  #     # shorten and add markup
  #     quality_str == "La garderie a un personnel formé qui proposent des activités stimulantes" ~
  #       "Avec un personnel **formé** qui **proposent des activités stimulantes**",
  #     quality_str == "La garderie a un personnel non-formé" ~
  #       "Avec un personnel **non-formé**",
  #     .default = quality_str
  #   )
  # ) |>
	# keep only the desired columns
  dplyr::select(
    # image number
    image,
    # choice A (1) or B (2)
    alt,
    # string attribute levels
    cost = childcare_str, hours = hours_str, location = location_str, quality = quality_str
  ) |>
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
