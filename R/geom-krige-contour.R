# Very simple, similar to how GeomDensity2dFilled inherits from GeomPolygon

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

geom_krige_contour_filled <- function(mapping = NULL, data = NULL,
                       stat = "krige_contour_filled", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomKrigeContourFilled,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}


GeomKrigeContour <- ggproto("GeomKrigeContour", GeomContour)

GeomKrigeContourFilled <- ggproto("GeomKrigeContourFilled", GeomContourFilled)