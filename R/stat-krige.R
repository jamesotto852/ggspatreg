# Pulling ideas from code for density2d and function stats/geoms

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
      var = var,
      ...)
  )
}


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
  # default_aes = {
  #   if (var) {
  #     aes(colour = NA, fill = after_stat(var))
  #   } else {
  #     aes(colour = NA, fill = after_stat(prediction))
  #   }
  # }
)

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
