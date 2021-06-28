# Pulling ideas from code for density2d and function stats/geoms

# Maybe good to allow for fill to be the SEs?
# Maybe allow for contours instead of raster?
#   Similar to geom_density2d and geom_density2d_filled
# Could allow for optional argument with geoR variogram class object


stat_krige <- function(mapping = NULL, data = NULL, geom = "krige",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...,
                       nx = 100,
                       ny = 100,
                       xlim = NULL, 
                       ylim = NULL,
                       method = "OK",
                       model = NULL,
                       inits = NULL) {
  layer(
    stat = StatKrige, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm, 
      nx = nx,
      ny = ny,
      xlim = xlim,
      ylim = ylim,
      method = method,
      model = model,
      inits = inits,
      var = var,
      ...)
  )
}


StatKrige <- ggproto("StatKrige", Stat,
                     compute_group = function(data, scales, 
                                              nx = 100, ny = 100, xlim = NULL, ylim = NULL, 
                                              method = "OK", model = NULL, inits = NULL) {
                       
  # Creating grid for kriging
  rangex <- xlim %||% scales$x$dimension()
  rangey <- ylim %||% scales$y$dimension()
  
  partition_x <- seq(rangex[1], rangex[2], length.out = nx)
  partition_y <- seq(rangey[1], rangey[2], length.out = ny)
  
  grid <- expand.grid(partition_x, partition_y) 
  
  
  # Factoring of the prediction mimicks that of stat_smooth
  
  # model <- do.call(method, c(base.args, method.args))
  # prediction <- predictdf(model, xseq, se, level)
  # predictdf <- function(model, xseq, se, level) UseMethod("predictdf")
  
  # Found issue on github where ggplot maintainers discuss:
  # https://github.com/tidyverse/ggplot2/issues/3132
  # Recommend tidy prediction tools?
  #   (Haven't found much for spatial prediction)
  
  
  krigedf(data, method, model, inits, grid)
  },
  
  required_aes = c("x", "y", "z"),
  default_aes = aes(color = NA, fill = after_stat(prediction))
  # default_aes = {
  #   if (var) {
  #     aes(colour = NA, fill = after_stat(var))
  #   } else {
  #     aes(colour = NA, fill = after_stat(prediction))
  #   }
  # }
)


krigedf <- function(data, method, model, inits, grid) {
 
  # Allow for no specification of cov.model? Choosing "best" as measured by some IC?
  #   cov.model <- model %||% best
  # Is there an easy way to generate reasonable initial values? MOM?
  
  # Only have been able to figure out how to krige via MLE with UK
  
  # Could allow for SK and KED
  
  
  if (method == "OK") {
    # Allow users to specify weights parameter
    #   cressie weights for robust estimation
    #   (npairs is the default)
    vgm <- geoR::variog(data = data$z, coords = cbind(data$x, data$y), weights = "npairs")
    
    vgm_fit <- geoR::variofit(vgm, ini.cov.pars = inits, cov.model = model)
    
    krige <- geoR::krige.conv(
     data = data$z, coords = cbind(data$x, data$y), locations = grid, 
     krige = geoR::krige.control(obj.model = vgm_fit)
    )
  }
  
  
  if (method == "UK") {
    vgm_fit <- geoR::likfit(data = data$z, coords = cbind(data$x, data$y), trend = "1st",
                        ini.cov.pars = inits, cov.model = model)

    krige <- geoR::krige.conv(
      data = data$z, coords = cbind(data$x, data$y), locations = grid, 
      krige = geoR::krige.control(
        obj.model = vgm_fit,
        # Could allow for specifying trend via formula (~) 
        type.krige = "ok", trend.d = "1st", trend.l = "1st"
      )
    )
  }
  
  
  
  prediction <- data.frame(grid[,1], grid[,2], krige$predict, krige$krige.var)
  colnames(prediction) <- c("x", "y", "prediction", "var") 
  
  prediction
}
