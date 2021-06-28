# Very simple, similar to how GeomDensity2dFilled inherits from GeomPolygon

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

GeomKrige <- ggproto("GeomKrige", GeomTile)