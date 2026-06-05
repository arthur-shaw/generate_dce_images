#' Create choice comparison images from data
#'
#' @param df Data frame produced by `prepare_data()`.
#' @param choice_num Numeric. Number to include in image name.
#' @param option_A_text Character. Text to show in choice A column label.
#' @param option_B_text Character. Text to show in choice B column label.
#' @param attribute_text Character. Text to show as attribute column label.
#' @param country Character. ISO 3166-1 alpha-2 country codes.
#' @param lang Character. Two-character ISO 639 language code.
#' Find values here:
#' https://en.wikipedia.org/wiki/List_of_ISO_639_language_codes.
#' @param output_dir Character. Path to directory where images should be saved.
#' @param col_width_icon Numeric. Width of the column for the icon,
#' before it is merged with the attribute column.
#' @param col_width_attribute Numeric. Width of the attribute label column.
#' @param col_width_choice Numeric. Width of the choice columns.
#' Same value is applied to each.
#' @param cell_padding Numeric. Padding in pixels to add along the horizontal
#' axis to all cells in the table.
#' @param image_height Numeric. Height of the image.
#' This indirectly affects the width / speace within the cell.
#' @param font_size Numeric.
#' @param viewport_width Numeric.Viewport width used for capturing
#' the image of the HTML table in ta headless browser session.
#'
#' @importFrom gt text_transform cells_body cols_label tab_style cell_fill
#' cells_column_labels cell_borders px cols_width cell_text tab_options
#' choices_table
#' @importFrom purrr map_chr
#' @importFrom glue glue
#' @importFrom fontawesome fa
#' @importFrom dplyr everything
#' @importFrom fs path
#' @importFrom stringr str_pad
create_image <- function(
  df,
  choice_num,
  option_A_text = "Option A",
  option_B_text = "Option B",
  attribute_text = "Attribute",
  country,
  lang,
  output_dir,
  # layout overrides
  # for the table
  col_width_icon = 170,
  col_width_attribute = 180,
  col_width_choice = 172,
  cell_padding = 10,
  image_height = 150,
  font_size = 25,
  viewport_width = 720
) {

  # define the set of right-to-left (RTL) languages
  rtl_langs <- c(
    "ar", # Arabic
    "he", # Hebrew
    "fa", # Farsi/Persian
    "ur", # Urdu
    "ku", # Kurdish
    # Aramaic
    "az", # Azeri - but has LTR writing system too
    "dv", # Dhivehi
    "ff", # Fula
    "zg" # Tamazight / Amazigh (written with Arabic script)
    # Rohingya
    # Syriac
    # N'Ko
  )

  # for RTL languages
  # invert the column order
  # so that the table's contents are in RTL reading order
  if (lang %in% rtl_langs) {

    df <- df |>
      dplyr::select(
        choice_B, choice_A, attribute, icon
      )

  }

  # compose a table
  choices_table <- df |>
    gt::gt() |>
    # transform icon name into a Font Awesome icon
    # importantly, iterating over each element of column
    # so as to return a scalar for that row
    gt::text_transform(
      locations = gt::cells_body(columns = icon),
      fn = function(x) {
        gt::local_image(
          filename = fs::path(proj_dir, "assets", x),
          height = image_height
        )
      }
    ) |>
    # replace remaining `**` with an HTML span tag
    # whose style visually highlights the differing elements
    gt::text_transform(
      locations = gt::cells_body(columns = c(choice_A, choice_B)),
      fn = function(x) {
        purrr::map_chr(
          .x = x,
          .f = ~ .x  |>
            # make first `**` into opening part of the tag
            # with style definition
            sub(
              pattern = "**",
              replacement = "<span style='color:red; font-weight: bold'>",
              fixed = TRUE
            ) |>
            # make the second `**` into the closing part of the tag
            sub(pattern = "**", replacement = "</span>", fixed = TRUE)
        )
      }
    ) |>
    # "merge" the icon and attribute column labels
    gt::cols_label(
      attribute = gt::html(
        glue::glue(
          "<div style='text-align:center;' colspan='2'>{attribute_text}</div>"
        )
      ),
      icon = ""
    ) |>
    # make the text in the attribute column bold
    gt::tab_style(
      locations = gt::cells_body(columns = attribute),
      style = gt::cell_text(weight = "bolder")
    ) |>
    # set the background color cells in the table
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#156082"),
        gt::cell_text(color = "white")
      ),
      locations = gt::cells_column_labels()
    ) |>
    # row stripes
    gt::opt_row_striping(row_striping = TRUE) |>
    gt::tab_options(
      table.background.color = "#e7eaed",
      row.striping.background_color = "#ccd2d8"
    ) |>
    # adjust the cell borders, color and size
    gt::tab_style(
      style = gt::cell_borders(
        sides = "all",
        color = "white",
        weight = gt::px(6)
      ),
      locations = gt::cells_column_labels(columns = -c(attribute, icon))
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "all",
        color = "white",
        weight = gt::px(6)
      ),
      locations = gt::cells_body(
        rows = dplyr::everything(),
        columns = -c(attribute, icon)
      )
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("top", "bottom"),
        color = "white",
        weight = gt::px(6)
      ),
      locations = gt::cells_body(
        rows = dplyr::everything(),
        columns = c(attribute, icon)
      )
    ) |>
    # set the column widths for adequate space and consistent look
    gt::cols_width(
      icon ~ gt::px(col_width_icon),
      attribute ~ gt::px(col_width_attribute),
      choice_A ~ gt::px(col_width_choice),
      choice_B ~ gt::px(col_width_choice)
    ) |>
    # adjust horizontal cell padding
    gt::tab_options(
      data_row.padding = gt::px(cell_padding),
      column_labels.padding = gt::px(cell_padding)
    ) |>
    # make choice columns center-aligned
    gt::cols_align(
      align = "center",
      columns = c(choice_A, choice_B)
    ) |>
    # specify the column labels for the choices
    gt::cols_label(
      choice_A = option_A_text,
      choice_B = option_B_text
    ) |>
    # style the column labels
    # for the choice columns
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_column_labels(columns = c(choice_A, choice_B))
    ) |>
    # for all columns, formally,
    # but choice columns practically (since all other column labels are empty)
    gt::tab_options(
      column_labels.border.top.color = "white",
      column_labels.font.weight = "bold",
      table.font.size = font_size 
    )

  # name the file as `image_` plus a 2-digit left-hand zero-padded number
  # plus a two-character language suffix `_{lang}`
  # directory is the 2-character country code
  image_file_path <- fs::path(
    output_dir,
    country,
    lang,
    paste0(
      "image_",
      stringr::str_pad(
        string = choice_num,
        width = 2,
        side = "left",
        pad = "0"
      ),
      "_", lang,
      ".png"
    )
  )

  # save table to an image
  gt::gtsave(
    data = choices_table,
    filename = image_file_path,
    vwidth = viewport_width,
  )

  # construct the file path from the perspective of WSL
  wsl_path <- paste0(
    # pre-pend to make it a mounted volume
    "/mnt/",
    # convert Windows drive letter to lower case
    tolower(substr(image_file_path, 1, 1)),
    # get the rest of the original path, from the 3rd to last path character
    substr(image_file_path, 3, nchar(image_file_path))
  )

  # run tool for lossy compressiom of PNG files
  base::system2(
    # call executable in WSL
    "wsl",
    # compose call
    args = c(
      "pngquant", "--quality=50-70", "--ext", ".png", "--force",
      shQuote(wsl_path)
    )
  )

}
