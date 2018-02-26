#' Place labels
#'
#' A place label is a simple data structure that exists to plot
#' spatial data as labels.
#'
#' @param x a spatial object, sf or anything convertible with `sf::st_as_sf` supported
#' @param ... arguments passed to `graphics::plot`,
#'
#' @return
#' @export
#'
#' @examples
#' library(sf)
#' north_carolina <- read_sf(system.file("shape/nc.shp", package="sf"))
#' nc_labs <- placelabel(north_carolina)
#' plot(st_geometry(north_carolina))
#' plot(nc_labs, add = TRUE, cex = 0.8)
placelabel <- function(x, ...) {
  UseMethod("placelabel")
}
#' @name placelabel
#' @export
placelabel.default <- function(x, ...) {
  placelabel(sf::st_as_sf(x))
}
#' @name placelabel
#' @export
placelabel.sf <- function(x, ...) {
  idx <- setdiff(seq_len(dim(x)[2]), which(names(x) == attr(x, "sf_column")))
  labname <- names(x)[idx[1L]]
  ## we have to only have one column of labels, otherwise we can't use subsetting without
  ## all manner of methods to make work
  if (length(idx) > 1L) message(sprintf("using column '%s' for labels", labname))
  x <- x[c(labname, attr(x, "sf_column"))]
  class(x) <- c("placelabel", class(x))
  x
}


#' Plot place label
#'
#' Plot a `placelabel` either by creating a blank plot and plotting only labels (the default),
#' or add to an existing plot with `add = TRUE`.
#'
#' This plot method is a thin wrapper around `graphics::text` and includes all of those arguments.
#' The `...` argument is passed to `graphics::plot` to control the set up of the plot
#' when `add = FALSE`.
#'
#' @param x placelabel
#' @inheritParams graphics::text
#' @param ... arguments passed to `graphics::plot` (ignored if `add = TRUE`)
#' @param add create a new plot if `FALSE` (the default), or add to an existing plot
#'
#' @return nothing
#' @export
#'
#' @examples
#' library(sf)
#' north_carolina <- read_sf(system.file("shape/nc.shp", package="sf"))
#' nc_labs <- placelabel(north_carolina["NAME"])
#' plot(nc_labs)
plot.placelabel <- function(x,
                            adj = NULL, pos = NULL, offset = 0.5,
                            vfont = NULL, cex = 1, col = NULL, font = NULL,
                            ...,
                            add = FALSE) {
  op <- options(warn = -1)
  on.exit(options(op))
  xy <- sf::st_coordinates(sf::st_centroid(x))
  labname <- setdiff(names(x), attr(x, "sf_column"))[1L]
  if (!add)  plot(xy, type = "n", ...)
  ex <- par("usr")
  rangex <- range(xy[ , 1L], na.rm = TRUE)
  rangey <- range(xy[ , 2L], na.rm = TRUE)
  if (rangex[1L] > ex[2L] || rangex[2L] < ex[1L]) warning("all x locations outside range of current plot")
  if (rangey[1L] > ex[4L] || rangey[2L] < ex[3L]) warning("all x locations outside range of current plot")
  text(xy, labels = x[[labname]],
       adj = adj, pos = pos, offset = offset,
       vfont = vfont, cex = cex, col = col, font = font)
}
