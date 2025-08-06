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
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::everything(),
      .fns = ~ labelled::to_character(
        x = .x,
        levels = "labels"
      )
    )
  ) |>
	dplyr::mutate(
    childcare = dplyr::case_when(
      childcare == "0GNF/ monthly" ~ "**0** GNF/monthly",
      childcare == "75,000 GNF/monthly" ~ "**75,000** GNF/monthly",
      childcare == "150,000 GNF/monthly" ~ "**150,000** GNF/monthly",
      childcare == "225,000 GNF/monthly" ~ "**225,000** GNF/monthly",
      childcare == "300,000 GNF/monthly" ~ "**300,000** GNF/monthly",
      childcare == "375,000 GNF/monthly" ~ "**375,000** GNF/monthly"
    ),
    hours = dplyr::case_when(
      hours == "Only during the morning" ~ "Only during the **morning**",
      hours == "Only during the afternoon" ~ "Only during the **afternoon**",
      hours == "Full day" ~ "**Full day**"
    ),
    location = dplyr::case_when(
      location == "The daycare is far from home (more than 15 minutes away)" ~
        "The daycare is **far** from home (more than 15 minutes away)",
      location == "The daycare is close to home (less than 15 minutes away)" ~
        "The daycare is **close** to home (less than 15 minutes away)"
    ),
    quality = dplyr::case_when(
      quality == "Child is with a caregiver in a safe environment" ~
        "Child is with a caregiver in a **safe** environment",
      quality == "Child is with a caregiver in a safe AND stimulating environment" ~
        "Child is with a caregiver in a **safe AND stimulating** environment"
    ),
    socialnorms = dplyr::case_when(
      socialnorms == "Few people I know would think I am a bad parent for sending child to daycare" ~
        "**Few people** I know would think I am a bad parent for sending child to daycare",
      socialnorms == "Most people I know would think I am a bad parent for sending child to daycare" ~
        "**Most people** I know would think I am a bad parent for sending child to daycare"
    ),
    food = dplyr::case_when(
      food == "The daycare does not offer food for the child" ~
        "The daycare **does not offer** food for the child",
      food == "The daycare offers food for the child" ~
        "The daycare **offers** food for the child",
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
      cost_text = "Childcare services cost",
      hours_text = "Number of available services hours",
      location_text = "Location",
      quality_text = "Quality",
      perceptions_text = "Perceptions",
      food_text = "Food"
    ) |>
    # then, compose a {gt} table and save an image of it
    create_image(
      choice_num = .x,
      option_A_text = "Option A",
      option_B_text = "Option B",
      output_dir = image_dir
    )
)
