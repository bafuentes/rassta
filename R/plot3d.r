#' @title
#' Interactive Maps of 3D surfaces
#'
#' @description
#' Interactive maps showing the 3-dimensional (\emph{XYZ}) variability in raster
#' layers representing continuous variables. The \emph{XYZ} reference positions
#' will be obtained from an elevation layer and the values of the continuous
#' variables will be used as a surface color gradient. For this function to
#' work, there must be a raster layer of elevation (e.g., digital terrain model)
#' and at least one continuous variable among the raster layers to map. The maps
#' produced are interactive, meaning that manual axis rotation and zoom are
#' possible. Special consideration must be taken with large raster layers (large
#' spatial coverage and/or high spatial resolution). This function can
#' aggregates the spatial resolution (i.e., cell size) in order to handle large
#' raster layers. This is achieved by internally calling
#' \code{\link[terra]{aggregate}}. An aggregation factor will determine the
#' final cell size, where \emph{final cell size = cell size*aggregation factor}.
#' In addition, a spatial extent can be provided to reduce the total mapping
#' area and thus, to further reduce processing time. This function uses the
#' \strong{plotly} library. See \strong{Details} for current limitations.
#'
#' @param var.rast SpatRaster, as in \code{\link[terra]{rast}}. Multi-layer
#'   SpatRaster of \emph{n} continuous variables and one layer representing the
#'   surface/terrain elevation.
#' @param z Integer. Position (index) of the raster layer of elevation in
#'   \emph{var.rast}.
#' @param ex Numeric. Value indicating the \emph{exaggeration} factor for the
#'   \emph{Z} axis. This can be useful to enhance the visualization of subtle
#'   topographic variability. Default: 0.1
#' @param agg Boolean. Should the spatial resolution be aggregated to reduce
#'   processing time? Default: FALSE
#' @param fact Numeric. If \emph{agg = TRUE}, value indicating the aggregation
#'   factor. Default: NULL
#' @param spext Numeric. List with the coordinates of the bounding box for
#'   spatial subset (xmin, xmax, ymin, ymax). SpatRaster or SpatVector from
#'   which a spatial extent can be calculated are also an acceptable input.
#'   Default: NULL
#' @param pals Character. List of strings with the names of the \emph{n} color
#'   ramps (one per continuous variable). See
#'   \code{\link[grDevices]{hcl.colors}}. Default: NA
#' @param rev Character. List of \emph{n} Booleans indicating whether or not to
#'   reverse the color ramp for each continuous variable. Default: NA
#'
#' @return
#' List with \strong{plotly-htmlwidget} objects. Each object calls the 3D map
#' for a continuous variable in \emph{var.rast}.
#'
#' @details
#' Currently, this function does not allow to adjust the labels for \emph{XY}
#' axes so that actual coordinates are shown. Instead, the relative position
#' values are shown on these axes.
#'
#' @examples
#' require(terra)
#' p <- system.file("exdat", package = "rassta")
#' # Multi-layer SpatRaster of topographic variables
#' ft <- list.files(path = p, pattern = "^height|^slope|^wetness",
#'                  full.names = TRUE
#'                 )
#' tvars <- terra::rast(ft)
#' # Single-layer SpatRaster of terrain elevation
#' fe <- list.files(path = p, pattern = "^elevation", full.names = TRUE)
#' e <- terra::rast(fe)
#' # Add elevation to the SpatRaster of topographic variables
#' etvars <- c(e, tvars)
#' # Interactive 3D maps
#' maps <- plot3D(var.rast = etvars, z = 1, ex = 0.2,
#'                pals = c("Zissou", "Plasma", "Spectral")
#'               )
#' if(interactive()){maps}
#'
#' @export
#' @family
#' Miscellaneous Functions
#' @rdname
#' plot3D
plot3D <- function(var.rast, z, ex = 0.1, agg = FALSE, fact = NULL,
                   spext = NULL, pals = NA, rev = NA)
{

  #-----Binding variables to prevent devtools::check() notes-----#
  i <- NULL
  #--------------------------------------------------------------#

  # Set palettes
  `%do%` <- foreach::`%do%`
  ppal <- foreach::foreach(i = 1:(terra::nlyr(var.rast)-1)) %do% {

    # Check if palette has been defined by user
   if(base::is.na(pals[i]) == TRUE) {

     ppal <- grDevices::hcl.colors(150, "spectral", rev = FALSE)

    } else {

      revpal <- if(base::is.na(rev[i]) == TRUE) { FALSE } else {rev[i]}
      ppal <- grDevices::hcl.colors(150, pals[i], rev = revpal)

    }

  }

  # Aggregate spatial resolution?
  aggvar <- if(agg == TRUE) {

    terra::aggregate(var.rast, fact)

  } else {

    var.rast

  }

  # Crop raster?
  aggvar <- if(base::is.null(spext) == FALSE) {

    terra::crop(aggvar, terra::ext(spext))

  } else {

    aggvar

  }

  # Surface matrix
  surf <- terra::as.matrix(aggvar[[z]], wide = TRUE)
  surf <- surf[, base::ncol(surf):1]
  gc()

  # Index of variables
  vars <- base::seq(1:terra::nlyr(aggvar))[-z]

  # Loop for each variable
  `%do%` <- foreach::`%do%`
  fig <- foreach::foreach(i = 1:base::length(vars)) %do% {

    # Define matrix of surface color based on variable values
    surfcol <- terra::as.matrix(aggvar[[(vars[i])]], wide = TRUE)
    surfcol <- surfcol[, base::ncol(surfcol):1]
    gc()

    # 3D surface map
    `%>%` <- dplyr::`%>%`
    fig <- plotly::plot_ly(z = surf,
                           colors = base::unlist(ppal[i]),
                           type = "surface",
                           colorbar = base::list(
                             title = base::list(
                               text = base::names(aggvar[[(vars[i])]]),
                               font = base::list(size = 20)
                              )
                            ),
                           surfacecolor = surfcol
                          ) %>%
      plotly::layout(title = base::names(aggvar[[(vars[i])]]),
                     scene = base::list(xaxis = base::list(title = "X"),
                                        yaxis = base::list(title = "Y"),
                                        zaxis = base::list(title = "Z"),
                                        aspectratio = base::list(
                                          x = 1, y = 1, z = ex
                                        )
                                      )
                    )
  }

  # Return maps
  base::names(fig) <- base::names(var.rast)[vars]
  base::return(fig)

}
