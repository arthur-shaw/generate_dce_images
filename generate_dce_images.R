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

choices_df <- fs::path(data_dir, "DCE_Childcare_FR.dta") |>
	haven::read_dta() |>
  # if `**` is missing, provide it
  # otherwise, keep text the same
	dplyr::mutate(
    childcare = dplyr::case_when(
      childcare_str == "0 GNF/ mois" ~ "**0** GNF/mois",
      childcare_str == "75,000 GNF/mois" ~ "**75 000** GNF/mois",
      childcare_str == "150,000 GNF/mois" ~ "**150 000** GNF/mois",
      childcare_str == "225,000 GNF/mois" ~ "**225 000** GNF/mois",
      childcare_str == "300,000 GNF/mois" ~ "**300 000** GNF/mois",
      childcare_str == "375,000 GNF/mois" ~ "**375 000** GNF/mois",
      .default = childcare_str
    ),
    hours = dplyr::case_when(
      hours_str == "Uniquement le matin" ~ "Uniquement **le matin**",
      hours_str == "Uniquement l'après-midi" ~ "Uniquement **l'après-midi**",
      hours_str == "Journée complète" ~ "**Journée complète**",
      .default = hours_str
    ),
    location = dplyr::case_when(
      location_str == "La garderie est loin de chez moi (à plus de 15 minutes)" ~
        "La garderie **est loin** de chez moi (à plus de 15 minutes)",
      location_str == "La garderie est proche de chez moi (à moins de 15 minutes)" ~
        "La garderie **est proche** de chez moi (à moins de 15 minutes)",
      .default = location_str
    ),
    quality = dplyr::case_when(
      quality_str == "L'enfant est avec une personne qui s'occupe de lui dans un environnement sûr" ~
        "L'enfant est avec une personne qui s'occupe de lui dans un environnement **sûr**",
      quality_str == "L'enfant est avec une personne qui s'occupe de lui dans un environnement sûr ET stimulant" ~
        "L'enfant est avec une personne qui s'occupe de lui dans un environnement **sûr ET stimulant**",
      quality_str == "L'enfant est avec une personne qui s'occupe de lui dans un environnement sûr **ET stimulant**" ~
        "L'enfant est avec une personne qui s'occupe de lui dans un environnement **sûr ET stimulant**",
      .default = quality_str
    ),
    socialnorms = dplyr::case_when(
      socialnorms_str == "Peu de gens que je connais penseraient que je suis un mauvais parent parce que j'envoie mon enfant à la garderie" ~
        "**Peu de gens** que je connais penseraient que je suis un mauvais parent parce que j'envoie mon enfant à la garderie",
      socialnorms_str == "La plupart des gens que je connais penseraient que je suis un mauvais parent si j'envoie mon enfant à la garderie" ~
        "**La plupart des gens** que je connais penseraient que je suis un mauvais parent si j'envoie mon enfant à la garderie",
      .default = socialnorms_str
    ),
    food = dplyr::case_when(
      food_str == "La garderie n'offre pas de nourriture à l'enfant" ~
        "La garderie **n'offre pas** de nourriture à l'enfant",
      food_str == "La garderie offre de la nourriture à l'enfant" ~
        "La garderie **offre** de la nourriture à l'enfant",
      .default = food_str
    )
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
      cost_text = "Coûts des services de garderie",
      hours_text = "Nombre d'heures de service disponibles",
      location_text = "Emplacement",
      quality_text = "Qualité",
      perceptions_text = "Perceptions",
      food_text = "Nourriture"
    ) |>
    # then, compose a {gt} table and save an image of it
    create_image(
      choice_num = .x,
      option_A_text = "Option A",
      option_B_text = "Option B",
      output_dir = image_dir
    ),
  .progress = TRUE
)
