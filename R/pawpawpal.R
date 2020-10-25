# Pawpawpal
#
#
# This is a package called pawpawpal that is used to provide the color palettes for Pawpaw Analytics
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# --------------------- Palettes -----

pawcolors <- c("light blue" = "#32B0CD",
               "dark blue" = "#3262CD",
               "darkest blue" = "#132B61",
               "mint" = "#32CD9D",
               "eggplant" = "#CD32B0",
               "green" = "#B0CD32",
               "dark purple" = "#9D32CD",
               "red" = "#CD4F32",
               "light green" = "#62CD32",
               "null blue" = "#85c8cd")

# Function to return hex codes from named colors:
paw_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (pawcolors)

  pawcolors[cols]
}

paw_cols()

pawpaw_palettes = list(analogous = paw_cols("null blue", "light blue", "dark blue"),
                       scale = paw_cols("null blue", "darkest blue"),
                       dichotomous = paw_cols("light blue", "red"),
                       triadic = paw_cols("light blue", "eggplant", "green"),
                       mixed = paw_cols("light blue", "dark purple", "red", "light green"))



#' Return function to interpolate a drsimonj color palette
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
pawpaw_pal <- function(palette = "analogous", reverse = FALSE, ...) {
  pal <- pawpaw_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}


# Now we can pull the hex codes from a palette at will.

pawpaw_pal("analogous")

# For colors
#' Title
#'
#' @param palette Choose the palette for use. analogous, triadic1, mixed
#' @param discrete Is the scale discrete? default = TRUE
#' @param reverse Is the scale reversede? default = FALSE
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
scale_color_pawpaw <- function(palette = "analogous", discrete = TRUE, reverse = FALSE, ...) {
  pal <- pawpaw_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("pawpaw_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# For Fill
scale_fill_pawpaw <- function(palette = "analogous", discrete = TRUE, reverse = FALSE, ...) {
  pal <- pawpaw_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("pawpaw_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# --------------------- Creating a PPA theme  ----------------------------------------------------------




library(ggplot2)

siteTheme <- function() {
  theme(panel.background = element_rect(fill = "#3bb0cd", colour = NA),
        plot.background = element_rect(fill = "#3bb0cd",colour = NA),
        legend.background = element_rect(fill = "#3bb0cd", colour = NA),
        legend.box.background = element_rect(fill = "#3bb0cd",colour = NA),
        axis.line = element_blank())
}

# --------------------- Set Font and Text Size -----
theme_paw <-
  function() {theme_minimal(base_size = 14,
                            base_family = "Roboto Mono") +
      theme(panel.grid.minor = element_blank(),
            rect = element_rect(fill = "transparent"))
    }

# Remove Legend
theme_paw_nl <-
  function() {theme_paw() +
      theme(legend.position = "none",
            panel.grid.minor = element_blank())
  }

# Remove Legend and x axis labels
theme_paw_nlx <-
  function() {theme_paw() +
      theme(legend.position = "none",
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank())
  }

# Remove Legend and y axis labels
theme_paw_nly <-
  function() {theme_paw() +
      theme(legend.position = "none",
            panel.grid.minor = element_blank(),
            axis.title.y = element_blank())
  }

# Remove Legend and all axis labels
theme_paw_nlxy <-
  function() {theme_paw() +
      theme(legend.position = "none",
            panel.grid.minor = element_blank(),
            axis.title = element_blank())
  }

# Remove x axis labels
theme_paw_nx <-
  function() {theme_paw() +
      theme(axis.title.x = element_blank(),
            panel.grid.minor = element_blank(),)
  }

# Remove y axis labels
theme_paw_ny <-
  function() {theme_paw() +
      theme(axis.title.y = element_blank(),
            panel.grid.minor = element_blank(),)
  }

# Remove all axis labels
theme_paw_nxy <-
  function() {theme_paw() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),)
  }

# Theme for usmap
theme_paw_usmap <-
  function() {theme_paw() +
      theme(legend.position = "right",
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank())
  }


# --------------------- Common Scales ----------------------------------------------

# --------------------- Percents

# y scale percent
scale_y_perc =
  scale_y_continuous(labels = scales::label_percent(accuracy = 1))

# x scale percent
scale_x_perc =
  scale_x_continuous(labels = scales::label_percent(accuracy = 1))

# ---------------------- Dollar

# y scale dollar
scale_y_dollar =
  scale_y_continuous(labels = scales::label_dollar(accuracy = 1))

# x scale dollar
scale_x_dollar =
  scale_x_continuous(labels = scales::label_dollar(accuracy = 1))

# ---------------------- Comma

scale_y_comma =
  scale_y_continuous(labels = scales::label_comma(accuracy = 1))

scale_x_comma =
  scale_x_continuous(labels = scales::label_comma(accuracy = 1))

# ----------------------- lseq

lseq <- function(from=1, to=100000, length.out=6) {
  # logarithmic spaced sequence
  # blatantly stolen from library("emdbook"), because need only this
  exp(seq(log(from), log(to), length.out = length.out))
}

# --------------------- us_codes df ----------------------------

us_fips <-
  dplyr::rename(
    merge(
      janitor::clean_names(read.csv("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv", colClasses = "character"))
    ,
      dplyr::mutate_all(dplyr::rename(janitor::clean_names(read.csv("https://query.data.world/s/bvoza42bvwbkyjtdhzm545qov5rric")), state_code = state), ~as.character(.))
    )
  , fips = stcountyfp)

# --------------------- read.paste()

read.paste <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}
