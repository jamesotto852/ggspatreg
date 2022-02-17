# this script contains several unexported helper functions

# unexported functions from ggplot2
xyz_to_isobands <- get("xyz_to_isobands", asNamespace("ggplot2"))
xyz_to_isolines <- get("xyz_to_isolines", asNamespace("ggplot2"))
iso_to_polygon <- get("iso_to_polygon", asNamespace("ggplot2"))
iso_to_path <- get("iso_to_path", asNamespace("ggplot2"))
contour_breaks <- get("contour_breaks", asNamespace("ggplot2"))
pretty_isoband_levels <- get("pretty_isoband_levels", asNamespace("ggplot2"))


create_grid <- function(rangex, rangey, nx, ny) {
  partition_x <- seq(rangex[1], rangex[2], length.out = nx)
  partition_y <- seq(rangey[1], rangey[2], length.out = ny)

  grid <- data.frame(expand.grid(partition_x, partition_y))
  names(grid) <- c("x", "y")

  grid
}

krigedf <- function(data, formula, model, inits, grid) {
  # Allow for no specification of cov.model? Choosing "best" as measured by some IC?
  #   cov.model <- model %||% best
  # Is there an easy way to generate reasonable initial values? MOM?

  vgm <- gstat::variogram(formula, locations = ~x+y, data = data)

  vgm_fit <- gstat::fit.variogram(vgm, model = gstat::vgm(model = model, psill = inits[1], range = inits[2], nugget = inits[3]))
  prediction <- gstat::krige(formula = formula, data = data, locations = ~x+y, newdata = grid, model = vgm_fit)

  colnames(prediction) <- c("x", "y", "pred", "var")
  prediction
}
