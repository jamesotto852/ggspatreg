#' @rdname geom_krige_contour
#' @export
stat_krige_contour_lines <- function(mapping = NULL, data = NULL,
                         geom = "krige_contour_lines", position = "identity",
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
    stat = StatKrigeContourLines,
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
      formula = formula,
      inits = inits,
      model = model,
      var = var,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_krige_contour
#' @export
StatKrigeContourLines <- ggproto("StatKrigeContourLines", Stat,
  required_aes = c("x", "y", "z"),
  default_aes = aes(color = after_stat(pred), order = after_stat(pred)),

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

  isolines <- xyz_to_isolines(prediction, breaks)
  path_df <- iso_to_path(isolines, data$group[1])

  # Weird fix, need unique names for isobands.
  # ggplot2 code never does anything like this -- it doesn't need to?
  isoline_names <- unique(names(isolines))

  # Need to round to reasonable accuracy
  # TODO: scientific notation?
  isoline_names <- scales::number(as.numeric(isoline_names), big.mark="")

  # Should pred be conts or discrete?
  # Definitely don't want interval names from ggplot2:::pretty_isoband_levels()
  path_df$level <- scales::number(as.numeric(path_df$level), big.mark="")
  path_df$pred <- ordered(path_df$level, levels = isoline_names)
  path_df$level <- NULL

  path_df
  }
)

#' @rdname geom_krige_contour
#' @export
geom_krige_contour_lines <- function(mapping = NULL, data = NULL,
                       stat = "krige_contour_lines", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomKrigeContourLines,
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
GeomKrigeContourLines <- ggproto("GeomKrigeContourLines", GeomPath,
  default_aes = aes(
    size = 1,
    colour = "#000000",
    linetype = 1,
    alpha = NA
  )
)
