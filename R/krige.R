#' Heatmap of kriging predictions
#'
#' TODO
#'
#' @section Aesthetics: geom_krige understands the following aesthetics (required
#'   aesthetics are in bold):
#'
#'   - **x**
#'   - **y**
#'   - **z**
#'   - alpha
#'   - color
#'   - fill
#'   - group
#'   - linetype
#'   - size
#'   - subgroup
#'
#' @section Computed variables:
#'
#'   \describe{
#'     \item{pred}{predicted value}
#'     \item{var}{prediction variance}
#'    }
#'
#' @inheritParams ggplot2::geom_point
#' @param nx,ny Resolution of grid used for predictions
#' @param xlim,ylim Range to compute and draw predictions. If `NULL`, defaults to
#'   range of data.
#' @param formula TODO
#' @param model TODO
#' @param inits TODO
#' @param geom,stat Use to override the default connection between `geom_krige()` and `stat_krige()`
#' @name geom_krige
#' @rdname geom_krige
#'
#' @import ggplot2
NULL

#' @rdname geom_krige
#' @export
stat_krige <- function(mapping = NULL, data = NULL, geom = "krige",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...,
                       nx = 100,
                       ny = 100,
                       xlim = NULL,
                       ylim = NULL,
                       formula = z ~ 1,
                       model = "Exp",
                       inits = c(NA, NA, NA)) {
  layer(
    stat = StatKrige, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      nx = nx,
      ny = ny,
      xlim = xlim,
      ylim = ylim,
      formula = formula,
      model = model,
      inits = inits,
      ...)
  )
}

#' @rdname geom_krige
#' @export
StatKrige <- ggproto("StatKrige", Stat,
                     compute_group = function(data, scales,
                                              nx = 100, ny = 100, xlim = NULL, ylim = NULL,
                                              formula = z ~ 1, model = "Exp", inits = c(NA, NA, NA)) {

  # Creating grid for kriging
  rangex <- xlim %||% scales$x$dimension()
  rangey <- ylim %||% scales$y$dimension()

  grid <- create_grid(rangex, rangey, nx, ny)


  # Factoring of the prediction mimicks that of stat_smooth

  # model <- do.call(method, c(base.args, method.args))
  # prediction <- predictdf(model, xseq, se, level)
  # predictdf <- function(model, xseq, se, level) UseMethod("predictdf")

  # Found issue on github where ggplot maintainers discuss:
  # https://github.com/tidyverse/ggplot2/issues/3132
  # Recommend tidy prediction tools?


  krigedf(data, formula, model, inits, grid)
  },

  required_aes = c("x", "y", "z"),
  default_aes = aes(color = NA, fill = after_stat(pred))
)

#' @rdname geom_krige
#' @export
geom_krige <- function(mapping = NULL, data = NULL,
                       stat = "krige", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomKrige,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_krige
#' @export
GeomKrige <- ggproto("GeomKrige", GeomTile)
