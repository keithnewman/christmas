#' Snowman card
#'
#' Create a snowman Christmas card
#'
#' @param filepath File path to save image to
#' @param message Individual message
#' @param greeting Greeting message. Default `"Merry Christmas"`
#' @param n_snowflakes Number of snowflakes. Default 100.
#' @param seed Random seed. Defaul 2512.
#' @return PNG file
#' @export

snowman_card <- function(filepath,
                         message,
                         greeting = "Merry Christmas",
                         n_snowflakes = 100,
                         seed = 2512) {
  # generate data
  set.seed(seed)
  snowflakes <- data.frame(
    x = stats::runif(n_snowflakes),
    y = stats::runif(n_snowflakes)
  )
  nose_pts <- matrix(
    c(
      0.6, 0.5,
      0.65, 0.48,
      0.6, 0.46,
      0.6, 0.5
    ),
    ncol = 2,
    byrow = TRUE
  )
  nose <- sf::st_polygon(list(nose_pts))

  g <- ggplot2::ggplot() +
    # snow on ground
    ggplot2::annotate(
      geom = "rect",
      xmin = 0, xmax = 1,
      ymin = 0, ymax = 0.2,
      fill = "gray98", colour = "gray98"
    ) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    # add snowflakes
    ggplot2::geom_point(
      data = snowflakes,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y
      ),
      colour = "white",
      pch = 8
    ) +
    # add snowman
    ggforce::geom_circle(
      data = data.frame(
        x0 = c(0.6, 0.6),
        y0 = c(0.3, 0.5),
        r = c(0.15, 0.1)
      ),
      mapping = ggplot2::aes(x0 = .data$x0, y0 = .data$y0, r = .data$r),
      fill = "white",
      colour = "white"
    ) +
    # add eyes and buttons
    ggplot2::geom_point(
      data = data.frame(
        x = c(0.6, 0.6, 0.6, 0.57, 0.62),
        y = c(0.25, 0.3, 0.35, 0.52, 0.52),
        size = stats::runif(5, 2, 4.5)
      ),
      mapping = ggplot2::aes(x = .data$x, y = .data$y, size = .data$size)
    ) +
    ggplot2::scale_size_identity() +
    # add sticks for arms
    ggplot2::annotate(
      geom = "segment",
      x = 0.7, xend = 0.85, y = 0.3, yend = 0.4,
      colour = "chocolate4",
      linewidth = 2
    ) +
    ggplot2::annotate(
      geom = "segment",
      x = 0.46, xend = 0.33, y = 0.3, yend = 0.4,
      colour = "chocolate4",
      linewidth = 2
    ) +
    # add hat
    ggplot2::annotate(
      geom = "rect",
      xmin = 0.46,
      xmax = 0.74,
      ymin = 0.56,
      ymax = 0.6,
      fill = "brown"
    ) +
    ggplot2::annotate(
      geom = "rect",
      xmin = 0.5,
      xmax = 0.7,
      ymin = 0.56,
      ymax = 0.73,
      fill = "brown"
    ) +
    # add nose
    ggplot2::geom_sf(
      data = nose,
      fill = "orange",
      colour = "orange"
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::annotate(
      geom = "text",
      x = 0.5,
      y = 0.95,
      label = greeting,
      colour = "red3",
      fontface = "bold",
      size = 9
    ) +
    ggplot2::annotate(
      geom = "text",
      x = 0.5,
      y = 0.07,
      label = message,
      colour = "red3",
      fontface = "bold",
      size = 5
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#0e1c2e")
    )
  ggplot2::ggsave(filename = filepath, plot = g, height = 4, width = 4)
}
