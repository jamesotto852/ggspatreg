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
                         inits = NULL,
                         method = "OK",
                         model = NULL,
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
      method = method,
      inits = inits,
      model = model,
      var = var,
      na.rm = na.rm, 
      ...
    )
  )
}

stat_krige_contour_filled <- function(mapping = NULL, data = NULL,
                                      geom = "krige_contour_filled", position = "identity",
                                      ...,
                                      bins = NULL,
                                      binwidth = NULL,
                                      breaks = NULL, 
                                      nx = 100,
                                      ny = 100,
                                      xlim = NULL,
                                      ylim = NULL,
                                      inits = NULL,
                                      method = "OK",
                                      model = NULL,
                                      var = FALSE,
                                      na.rm = FALSE,
                                      show.legend = NA,
                                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatKrigeContourFilled,
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
      method = method,
      model = model,
      var = var,
      na.rm = na.rm,
      ...
    )
  )
}


StatKrigeContour <- ggproto("StatKrigeContour", Stat,
  required_aes = c("x", "y", "z"),
  default_aes = aes(order = after_stat(level)),
  
  compute_group = function(data, scales, bins = NULL, binwidth = NULL, #z.range,
                          breaks = NULL, na.rm = FALSE,
                          nx = 100, ny = 100, xlim = NULL, ylim = NULL, # should these be here?
                          method = "OK", inits = NULL, model = NULL, var = FALSE) {
  # Creating grid for kriging
  rangex <- xlim %||% scales$x$dimension()
  rangey <- ylim %||% scales$y$dimension()
  
  partition_x <- seq(rangex[1], rangex[2], length.out = nx)
  partition_y <- seq(rangey[1], rangey[2], length.out = ny)
  
  grid <- expand.grid(partition_x, partition_y) 
  
  prediction <- krigedf(data, method, model, inits, grid)
  
  if (var) {
    prediction <- prediction[,-3]
  } else {
    prediction <- prediction[,-4]
  }

  colnames(prediction) <- c("x", "y", "z") # Needs these names for xyz_to_isolines
  
  
  z.range <- range(prediction$z, na.rm = TRUE, finite = TRUE)
  
  breaks <- ggplot2:::contour_breaks(z.range, bins, binwidth, breaks)
  
  isolines <- ggplot2:::xyz_to_isolines(prediction, breaks)
  path_df <- ggplot2:::iso_to_path(isolines, data$group[1])
   
  path_df$level <- as.numeric(path_df$level)
  path_df$nlevel <- scales::rescale_max(path_df$level)
   
  path_df
  }
)


StatKrigeContourFilled <- ggproto("StatKrigeContourFilled", Stat,
                             
  required_aes = c("x", "y", "z"),
  default_aes = aes(order = after_stat(level), fill = after_stat(level)),
  
  compute_group = function(data, scales, bins = NULL, binwidth = NULL, 
                           breaks = NULL, na.rm = FALSE,
                           nx = 100, ny = 100, xlim = NULL, ylim = NULL, # should these be here?
                           method = "OK", inits = NULL, model = NULL, var = FALSE) {
    
  # Creating grid for kriging
  rangex <- xlim %||% scales$x$dimension()
  rangey <- ylim %||% scales$y$dimension()
  
  partition_x <- seq(rangex[1], rangex[2], length.out = nx)
  partition_y <- seq(rangey[1], rangey[2], length.out = ny)
  
  grid <- expand.grid(partition_x, partition_y) 
  
  prediction <- krigedf(data, method, model, inits, grid)
  
  if (var) {
    prediction <- prediction[,-3]
  } else {
    prediction <- prediction[,-4]
  }

  colnames(prediction) <- c("x", "y", "z") # Needs these names for xyz_to_isolines
  
  z.range <- range(prediction$z, na.rm = TRUE, finite = TRUE)
    
  breaks <- ggplot2:::contour_breaks(z.range, bins, binwidth, breaks)
   
  isobands <- ggplot2:::xyz_to_isobands(prediction, breaks)
  names(isobands) <- ggplot2:::pretty_isoband_levels(names(isobands))
  path_df <- ggplot2:::iso_to_polygon(isobands, data$group[1])
  
  # Weird fix, need unique names for isobands.
  # ggplot2 code never does anything like this -- it doesn't need to?
  isoband_names <- unique(names(isobands))
  
  # path_df$level <- ordered(path_df$level, levels = names(isobands))
  path_df$level <- ordered(path_df$level, levels = isoband_names)
  path_df$level_low <- breaks[as.numeric(path_df$level)]
  path_df$level_high <- breaks[as.numeric(path_df$level) + 1]
  path_df$level_mid <- 0.5*(path_df$level_low + path_df$level_high)
  path_df$nlevel <- scales::rescale_max(path_df$level_high)
   
  path_df
  }
)

