#' Place labels
#'
#' A place label is a simple data structure that exists to plot
#' spatial data as labels.
#'
#' @seealso plot.placelabel
#' @param x a spatial object, sf or anything convertible with `sf::st_as_sf` supported
#' @param text the name of the column to be used as a label
#' @param ... arguments passed to `graphics::plot`,
#'
#' @return a data frame 'placelabel'
#' @export
#'
#' @examples
#' library(sf)
#' north_carolina <- read_sf(system.file("shape/nc.shp", package="sf"))
#' nc_labs <- placelabel(north_carolina)
#' plot(st_geometry(north_carolina))
#' plot(nc_labs, add = TRUE, cex = 0.8)
placelabel <- function(x, text = NULL, ...) {
  UseMethod("placelabel")
}
#' @name placelabel
#' @export
placelabel.default <- function(x, text = NULL, ...) {
  placelabel(sf::st_as_sf(x))
}
#' @name placelabel
#' @export
placelabel.sf <- function(x, text = NULL, ...) {
  if (is.null(text)) {
    idx <- setdiff(seq_len(dim(x)[2]), which(names(x) == attr(x, "sf_column")))
    labname <- names(x)[idx[1L]]
  } else {
    labname <- text
  }
  ## new approach
  op <- options(warn = -1)
  on.exit(options(op))
  xy <- sf::st_coordinates(sf::st_centroid(x))
  out <-  sf::st_set_geometry(x, NULL)
  out[c("X_", "Y_")] <- tibble::as_tibble(xy)
  ## put the default label upfront
  if (length(names(out)) > 3L) message(sprintf("using column '%s' for labels", labname))
  out <- out[c(labname, setdiff(names(out), labname))]
  class(out) <- c("placelabel", class(out))
  out
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
#' @importFrom graphics plot par text
#' @importFrom sf st_centroid st_coordinates st_set_geometry
#' @examples
#' library(sf)
#' north_carolina <- read_sf(system.file("shape/nc.shp", package="sf"))
#' nc_labs <- placelabel(north_carolina)
#' plot(nc_labs)
#' nc_labs <- placelabel(north_carolina, text = "NAME")
#' plot(nc_labs, cex = 0.5)
#' ## leverage sf aesthetics without destroying plot layout
#' plot(north_carolina[1], add = TRUE)
#' plot(nc_labs, cex = 0.5, add = TRUE)
#' plot(st_geometry(north_carolina))
#' plot(nc_labs, "BIR74", add = TRUE, cex = 0.5)
plot.placelabel <- function(x, labels = NULL,
                            adj = NULL, pos = NULL, offset = 0.5,
                            vfont = NULL, cex = 1, col = NULL, font = NULL,
                            ...,
                            add = FALSE) {
  if (is.null(labels)) labname <- names(x)[1L] else {
    if (!is.character(labels)) warning("expecting a character value for 'labels' (column name in x)")
    labname <- labels[1L]
  }
  xy <- as.matrix(x[c("X_", "Y_")])
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
