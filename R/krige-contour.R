#' Contours of kriging predictions
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
#'   geom_krige_lines understands the following aesthetics (required
#'   aesthetics are in bold):
#'
#'   - **x**
#'   - **y**
#'   - **z**
#'   - alpha
#'   - color
#'   - group
#'   - linetype
#'   - size
#'   - subgroup
#'
#' @section Computed variables:
#'
#'   \describe{
#'     \item{pred}{predicted value}
#'    }
#'
#' @inheritParams geom_krige
#' @param bins TODO
#' @param binwidth TODO
#' @param breaks TODO
#' @param var TODO
#' @name geom_krige_contour
#' @rdname geom_krige_contour
#'
#' @import ggplot2
NULL

#' @rdname geom_krige_contour
#' @export
stat_krige_contour <- function(mapping = NULL, data = NULL,
                                      geom = "krige_contour", position = "identity",
                                      ...,
                                      bins = NULL,
                                      binwidth = NULL,
                                      breaks = NULL,
                                      nx = 100,
                                      ny = 100,
                                      xlim = NULL,
                                      ylim = NULL,
                                      formula = z ~ 1,
                                      model = "Exp",
                                      inits = c(NA, NA, NA),
                                      var = FALSE,
                                      na.rm = FALSE,
                                      show.legend = NA,
                                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatKrigeContour,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      breaks = breaks,
      nx = nx,
      ny = ny,
      xlim = xlim,
      ylim = ylim,
      inits = inits,
      formula = formula,
      model = model,
      var = var,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_krige_contour
#' @export
StatKrigeContour <- ggproto("StatKrigeContour", Stat,

  required_aes = c("x", "y", "z"),
  default_aes = aes(order = after_stat(pred), fill = after_stat(pred)),

  compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                           breaks = NULL, na.rm = FALSE,
                           nx = 100, ny = 100, xlim = NULL, ylim = NULL,
                           formula = z ~ 1, inits = c(NA, NA, NA), model = "Exp", var = FALSE) {

  # Creating grid for kriging
  rangex <- xlim %||% scales$x$dimension()
  rangey <- ylim %||% scales$y$dimension()

  grid <- create_grid(rangex, rangey, nx, ny)

  prediction <- krigedf(data, formula, model, inits, grid)

  if (var) {
    prediction <- prediction[,-3]
  } else {
    prediction <- prediction[,-4]
  }

  colnames(prediction) <- c("x", "y", "z") # Needs these names for xyz_to_isolines

  z.range <- range(prediction$z, na.rm = TRUE, finite = TRUE)

  breaks <- contour_breaks(z.range, bins, binwidth, breaks)

  isobands <- xyz_to_isobands(prediction, breaks)
  names(isobands) <- pretty_isoband_levels(names(isobands))
  path_df <- iso_to_polygon(isobands, data$group[1])

  # Weird fix, need unique names for isobands.
  # ggplot2 code never does anything like this -- it doesn't need to?
  isoband_names <- unique(names(isobands))

  path_df$pred <- ordered(path_df$level, levels = isoband_names)
  path_df$level <- NULL

  path_df
  }
)

#' @rdname geom_krige_contour
#' @export
geom_krige_contour <- function(mapping = NULL, data = NULL,
                       stat = "krige_contour", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomKrigeContour,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname geom_krige_contour
#' @export
GeomKrigeContour <- ggproto("GeomKrigeContour", GeomPolygon)
