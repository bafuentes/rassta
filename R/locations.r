#' @title
#' Select Representative Sampling Locations for Stratification Units
#'
#' @description
#' Selection of the \emph{representative sampling locations} based on landscape
#' similarity values. For a give stratification unit, the representative
#' sampling location is the XY position where the highest landscape similarity
#' value occurs. This location is assumed to best reflect the influence that the
#' landscape configuration of a given stratification unit exerts on response
#' phenomena. Currently, two selection methods are supported: (1) \emph{maximum
#' similarity within buffer zones} ("buffer"), and (2) \emph{absolute maximum
#' similarity} ("absolute"). For the buffer method, the \emph{n} largest zones
#' enclosing landscape similarity values above a certain threshold are first
#' identified. Then, for each zone, one sample is placed at the \emph{XY}
#' location where the landscape similarity value is maximized. For the absolute
#' method, a sample is placed at the \emph{XY} locations with the \emph{n}
#' maximum landscape similarity values. In both methods, it is possible to
#' constrain the sampling process to the boundaries of the stratification unit.
#' Constraining the process ensures that the sampling locations determined for a
#' given unit are placed within the boundaries of that unit. See
#' \strong{Details} for some guidance in the use of this function for
#' classification units.
#'
#' @param ls.rast SpatRaster, as in \code{\link[terra]{rast}}. Multi-layer
#'   SpatRaster representing landscape similarities to stratification units.
#' @param su.rast SpatRaster. Single-layer SpatRaster representing the
#'   stratification units occurring across geographic space. Integer values are
#'   expected as cell values (i.e., numeric codes) of stratification units.
#' @param method Character. String denoting the sampling method. Current options
#'   are "buffer" for the maximum similarity within buffer zones method, and
#'   "absolute" for the absolute maximum similarity method. Default: "buffer"
#' @param constrained Boolean. Should the sampling process be constrained to the
#'   boundaries of each stratification unit? See \strong{Details}. Default: TRUE
#' @param buf.quant Numeric. Number expressed in quantile notation (0-1)
#'   indicating the similarity threshold for the creation of buffer zones. Only
#'   zones enclosing raster cells with \emph{landscape similarity >= buf.quant}
#'   will be created and thus, considered for sampling. See \strong{Details}.
#'   Default: 0.9
#' @param buf.n Integer. Positive integer indicating the \emph{n} largest buffer
#'   zones for which sampling locations will be selected (\emph{n} buffer zones
#'   per stratification unit, one sampling location per buffer zone). Default: 1
#' @param abs.n Integer. When \emph{method = "absolute"}, Positive integer
#'   indicating the number of sampling locations to select for each
#'   stratification unit. See \strong{Details}. Default: 1
#' @param tol Numeric. When \emph{method = "absolute"}, this number will be
#'   subtracted from the sampled maximum value of a landscape similarity layer
#'   to ensure that the requested number of sampling locations will be found
#'   (see \strong{Details}). The default assumes that landscape similarity
#'   values are on a scale of 1 to 100. If these values are on a different scale
#'   (e.g., decimal), then, \emph{tol} needs to be adjusted accordingly.
#'   Default: 1
#' @param parallel Boolean. Perform parallel processing? A parallel backend must
#'   be registered beforehand with \code{\link[doParallel]{registerDoParallel}}.
#'   Keep in mind that the amount of RAM to allocate when performing parallel
#'   processing can result prohibitive for large data sets. Default: FALSE
#' @param to.disk Boolean. Should output SpatVector(s) (as in
#'   \code{\link[terra]{vect}}) be written to disk? Default: FALSE
#' @param outdir Character. If \emph{to.disk = TRUE}, string specifying the path
#'   for the output SpatVector(s). Default: "."
#' @param verbose Boolean. Show warning messages in the console? Default: FALSE
#' @param ... Additional arguments, as for \code{\link[terra]{writeVector}}.
#'
#' @return
#' If \emph{method = "buffer"} and \emph{constrained = TRUE}, a list with the
#' following components:
#'
#' \strong{locations}: SpatVector of point geometry. Each point in this vector
#' represents the sampling location placed at the maximum landscape similarity
#' value within a stratification unit's buffer zone. Tabular attributes in this
#' SpatVector are (1) \emph{SU} = stratification unit's numeric code, (2)
#' \emph{land_sim} = landscape similarity value, (3) \emph{x} = X coordinate,
#' and (4) \emph{y} = Y coordinate.
#'
#' \strong{buffer}: SpatVector of polygon geometry. Each polygon in this vector
#' represents the buffer zone of an stratification unit.
#'
#' If \emph{method = "buffer"} and \emph{constrained = FALSE}:
#'
#' \strong{locations}: Same as \strong{locations} from \emph{method = "buffer"}
#' and \emph{constrained = TRUE}.
#'
#' If \emph{method = "absolute"}:
#'
#' \strong{locations}: SpatVector of point geometry. Each point in this vector
#' represents the sampling location placed at the maximum landscape similarity
#' value for an stratification unit. Tabular attributes in this SpatVector are
#' (1) \emph{SU} = stratification unit's numeric code, (2) \emph{land_sim} =
#' landscape similarity value, (3) \emph{x} = X coordinate, and (4) \emph{y} = Y
#' coordinate.
#'
#' @details
#' Except when \emph{buf.n = 1} or \emph{abs.n = 1}, the number of returned
#' sampling locations per stratification unit may be smaller than requested,
#' especially when \emph{constrained = TRUE}. For the constrained buffer method,
#' reducing the landscape similarity threshold value \emph{buf.quant} will not
#' always result in more buffer zones; i.e., more sampling locations. The reason
#' for this is that reducing the threshold value for the creation of buffer
#' zones may actually promote the spatial contiguity of zones. For instance, two
#' buffer zones created at \emph{buf.quant = 0.9}, may be merged into a single
#' buffer zone when \emph{buf.quant = 0.80}. This will occur if the raster cells
#' between the two buffer zones satisfy: \emph{landscape similarity >=
#' quantile(landscape similarity, 0.8)}. For the absolute method, increasing the
#' value of the \emph{tol} argument will ensure a safer search for \emph{n}
#' sampling locations and thus, greater chances of getting the total number of
#' requested sampling locations per stratification unit.
#'
#' Note that this sampling scheme can be applied for classification units. In
#' order to do this, one should replace the multi-layer SpatRaster of landscape
#' similarities with a multi-layer SpatRaster of spatial signatures. One should
#' also replace the raster layer of stratification units with that of
#' classification units.
#'
#' @examples
#' require(terra)
#' p <- system.file("exdat", package = "rassta")
#' # Multi-layer SpatRaster of landscape similarities
#' fls <- list.files(path = p, pattern = "su_", full.names = TRUE)
#' ls <- terra::rast(fls)
#' # Single-layer SpatRaster of stratification units
#' fsu <- list.files(path = p, pattern = "strata.tif", full.names = TRUE)
#' su <- terra::rast(fsu)
#' # Get 1 representative sampling location per stratification unit
#' rl <- locations(ls.rast = ls, su.rast = su)
#' # Plot representative locations (including buffer areas)
#' if(interactive()){
#'   plot(su, type = "classes", fun = function() c(points(rl$locations),
#'                                                 polys(rl$buffers))
#'       )}
#'
#' @export
#' @family
#' Functions for Stratified Sampling
#' @rdname
#' locations
#' @seealso
#' \code{\link{similarity}}, \code{\link{strata}}
#'
locations <- function(ls.rast, su.rast, method = "buffer", constrained = TRUE,
                      buf.quant = 0.9, buf.n = 1, abs.n = 1, tol = 1,
                      parallel = FALSE, to.disk = FALSE, outdir = ".",
                      verbose = FALSE, ...)
{

  #-----Binding variables to prevent devtools::check() notes-----#
  i <- j <- k <- NULL
  #--------------------------------------------------------------#

  # According to sampling method
  if(method == "buffer") {

    # Iteration over SU's landscape similarity layers in SpatRaster
    `%ps%` <- if(parallel == TRUE) {foreach::`%dopar%`} else {foreach::`%do%`}
    su_ls <- foreach::foreach(i = 1:(terra::nlyr(ls.rast))) %ps% {

      # Numeric code of SU from landscape similarity layer on current iteration
      sucode <- stringi::stri_replace_all_regex(
        base::names(ls.rast[[i]]), "[a-zA-punct]", ""
      )
      sucode <- base::as.numeric(sucode)

      # Create template data frame to store attributes of locations shapefile
      reploc <- base::data.frame(
        SU = base::character(),
        land_sim = base::numeric(),
        x = base::numeric(),
        y = base::numeric()
      )

      # Constrained buffer pixels to geographic space covered by SU?
      if(constrained == TRUE) {

        # Extract patches spatially enclosed by SU
        wsu <- terra::mask(
          ls.rast[[i]], su.rast, inverse = TRUE, maskvalues = sucode
        )

        # Extraction of buffer pixels with values above SU's landscape...
        # ...similarity threshold
        ## Define SU's landscape similarity threshold value
        qthresh <- base::as.numeric(
          terra::global(
            wsu, fun = stats::quantile, probs = buf.quant, na.rm = TRUE
          )
        )
        ## "Binarization" of SU's landscape similarity layer based on...
        ## ...buffer pixels' threshold value
        wsu <- terra::classify(
          wsu,
          base::matrix(
            c(qthresh, terra::minmax(wsu)[2], 1), ncol = 3, nrow = 1
          ),
          include.lowest = TRUE,
          others = NA
        )

        # Identify group(s) of connected buffer pixels (i.e., boundary...
        # ...detection [Queen's case])
        wsu <- terra::patches(wsu, directions = 8)

        # Convert patches to polygons and calculate area
        x <- terra::as.polygons(
          wsu, trunc = TRUE, dissolve = TRUE, values = TRUE, extent = FALSE
        )
        x$area <- terra::expanse(x)
        base::remove(wsu)
        gc()

      } else {

        # Extraction of buffer pixels with values above SU's landscape...
        # ...similarity threshold
        ## Define SU's landscape similarity threshold value
        qthresh <- base::as.numeric(
          terra::global(
            ls.rast[[i]],
            fun = stats::quantile,
            probs = buf.quant,
            na.rm = TRUE
          )
        )
        ## "Binarization" of SU's landscape similarity layer based on...
        ## ...buffer pixels' threshold value
        wt <- terra::classify(
          ls.rast[[i]],
          base::matrix(
            c(qthresh, terra::minmax(ls.rast[[i]])[2], 1),
            ncol = 3,
            nrow = 1
          ),
          include.lowest = TRUE,
          others = NA
        )

        # Identify group(s) of connected buffer pixels (i.e., boundary...
        # ...detection [Queen's case])
        wt <- terra::patches(wt, directions = 8)

        # Convert patches to polygons and calculate area
        x <- terra::as.polygons(
          wt, trunc = TRUE, dissolve = TRUE, values = TRUE, extent = FALSE
        )
        x$area <- terra::expanse(x)
        remove(wt)
        gc()

      }

      # Extract n largest polygon(s) via area sorting
      ## Make sure that requested n is not greater than number of polygons
      maxunits <- base::as.numeric(
        if(buf.n > base::length(x)) {maxunits <- base::length(x)} else {buf.n}
      )
      ## Extract n polygon(s) with largest area
      xd <- terra::as.data.frame(x)
      xd <- base::order(-xd[, 2])[1:maxunits]
      x <- x[xd, ]
      ## Add column with numeric code of SU's landscape similarity layer...
      ## ...on current iteration
      x[["SU"]] <- sucode

      # For each extracted polygon(s), get pixel(s) with maximum SU's...
      # ...landscape similarity value(s)
      `%do%` <- foreach::`%do%`
      foreach::foreach(j = 1:base::length(x)) %do% {

        # Mask and crop to extract SU's landscape similarity pixels within...
        # ...polygon
        z <- terra::mask(
          terra::crop(ls.rast[[i]], x[j]), x[j]
        )

        # Get maximum SU's landscape similarity value and its location
        ## Index of cell
        zmax <- terra::where.max(z)
        ## Position
        zxy <- terra::xyFromCell(z, zmax[1, 2])

        # Rename columns with SU and maximum SU's landscape similarity value
        base::colnames(zmax)[c(1, 3)] <- c("SU", "land_sim")

        # Add column with name of SU's landscape similarity layer on...
        # ...current iteration
        zmax[1, 1] <- sucode
        zmax <- base::as.data.frame(zmax)

        ## Append XY location to maximum SU's landscape similarity value
        zxy <- base::as.data.frame(zxy)
        z <- base::cbind(zmax[, c(1, 3)], zxy)

        # Send extracted location to originally empty data frame
        reploc <- base::rbind(reploc, z)

      }

      # Return objects
      x <- terra::as.data.frame(x, geom = "WKT")
      base::list(locations = reploc, buffers = x)

    }

    # Get list of data frames with sampling locations
    `%do%` <- foreach::`%do%`
    locations <- foreach::foreach(k = 1:base::length(su_ls)) %do% {

      su_ls[[k]]$locations

    }

    # Merge list of data frames with sampling locations
    locations <- base::Reduce(
      function(...) base::merge(..., all = TRUE), locations
    )
    ## Enforce sorting by SU
    locations <- locations[base::order(locations[,1]), ]

    # Create SpatVector with sampling locations
    locations <- terra::vect(
      locations, geom = c("x", "y"), crs = terra::crs(su.rast)
    )

    # Get list of data frames with sampling buffers
    `%do%` <- foreach::`%do%`
    buffers <- foreach::foreach(k = 1:base::length(su_ls)) %do% {

      su_ls[[k]]$buffers

    }

    # Merge list of data frames with sampling buffers
    buffers <- base::Reduce(
      function(...) base::merge(..., all = TRUE), buffers
    )
    ## Enforce sorting by SU
    buffers <- buffers[base::order(buffers[,3]), ]

    # Create SpatVector with sampling buffers
    buffers <- terra::vect(
      buffers, geom = "geometry", crs = terra::crs(su.rast)
    )
    ## Select only SU's numeric code and buffer area as attributes
    buffers <- buffers[, c(3:2)]

    # Return objects
    if(to.disk == TRUE) {

      if(constrained == TRUE) {

        terra::writeVector(
          locations, base::file.path(outdir, "reploc_buf_con.shp"), ...
        )
        terra::writeVector(
          buffers, base::file.path(outdir, "reppol_buf_con.shp"), ...
        )
        base::list(locations = locations, buffers = buffers)

      } else {

        terra::writeVector(
          locations, base::file.path(outdir, "reploc_buf_uncon.shp"), ...
        )
        locations

      }

    } else {

      if(constrained == TRUE) {

        base::list(locations = locations, buffers = buffers)

      } else {

        locations <- locations

      }

    }

  } else if (method == "absolute") {

    # Iteration over SU's landscape similarity layers in stack
    `%ps%` <- if(parallel == TRUE) { foreach::`%dopar%` } else {foreach::`%do%`}
    su_ls <- foreach::foreach(i = 1:(terra::nlyr(ls.rast))) %ps% {

      # Numeric code of SU from landscape similarity layer on current...
      # ...iteration
      sucode <- stringi::stri_replace_all_regex(
        base::names(ls.rast[[i]]), "[a-zA-punct]", ""
      )
      sucode <- base::as.numeric(sucode)

      # Create empty data frame to serve as basis for shapefile of locations
      reploc <- base::data.frame(
        SU = base::character(),
        land_sim = base::numeric(),
        x = base::numeric(),
        y = base::numeric()
      )

      # Constrained locations within space covered by SU?
      if(constrained == TRUE) {

        # Extract patches spatially enclosed by SU
        w <- terra::mask(
          ls.rast[[i]], su.rast, inverse = TRUE, maskvalues = sucode
        )

        # Get maximum SU's landscape similarity value (minus tolerance)
        val <- terra::minmax(w)[2]

        # Subtract tolerance value
        if((val - tol) < terra::minmax(w)[1]) {

          base::stop(
            paste(
              'Tolerance value is too high. Please use a lower value.',
              'Problem occurred for similarity layer',
              base::names(ls.rast[[i]])
            )
          )

        } else {

          val <- val - tol

        }

        # Eliminate pixel with SU's landscape similarity values lower...
        # ...than maximum (minus tolerance)
        w <- terra::classify(
          w,
          base::matrix(
            c(terra::minmax(w)[1], val, NA),
            nrow = 1,
            ncol = 3
          ),
          right = FALSE
        )

        # Convert raster pixels to point locations
        w <- terra::as.points(w)
        ## Get coordinates as attributes
        w[["x"]] <- terra::geom(w)[,c("x")]
        w[["y"]] <- terra::geom(w)[,c("y")]
        w <- base::as.data.frame(w[[1:3]])

      } else {

        # Get maximum SU's landscape similarity value (minus tolerance)
        val <- terra::minmax(ls.rast[[i]])[2]

        if((val - tol) < terra::minmax(w)[1]) {

          base::stop(
            paste(
              'Tolerance value is too high. Please use a lower value.',
              'Problem occurred for similarity layer',
              base::names(ls.rast[[i]])
            )
          )

        } else {

          val <- val - tol

        }

        # Eliminate pixel with SU's landscape similarity values lower than...
        # ...maximum (minus tolerance)
        w <- terra::classify(
          ls.rast[[i]],
          base::matrix(
            c(terra::minmax(ls.rast[[i]])[1], val, NA),
            nrow = 1,
            ncol = 3
          ),
          right = FALSE
        )

        # Convert raster pixels to point locations
        w <- terra::as.points(w)
        ## Get coordinates as attributes
        w[["x"]] <- terra::geom(w)[,c("x")]
        w[["y"]] <- terra::geom(w)[,c("y")]
        w <- base::as.data.frame(w[[1:3]])

      }

      # Extract n locations with maximum SU's landscape similarity values
      ## Make sure that requested n is not greater than number of locations
      maxvals <- base::as.numeric(
        if(abs.n > base::nrow(w)) { maxvals <- base::nrow(w) } else { abs.n }
      )
      ## Extract locations
      w <- w[base::order(w[,1], decreasing = TRUE)[1:maxvals], ]
      ## Rename column with SU's landscape similarity value
      base::colnames(w)[1] <- "land_sim"
      ## Add column with name of SU's landscape similarity layer on...
      ## ...current iteration
      w$SU <- sucode
      base::rownames(w) <- NULL
      w <- w[, c(4,1,2,3)]

      # Send extracted location to originally empty data frame
      reploc <- base::rbind(reploc, w)

      # Return objects
      base::remove(w)
      return(reploc)

    }

    # Merge list of data frames with sampling locations
    locations <- base::Reduce(
      function(...) base::merge(..., all = TRUE), su_ls
    )
    ## Enforce sorting by SU
    locations <- locations[base::order(locations[,1]), ]

    # Create SpatVector with sampling locations
    locations <- terra::vect(
      locations, geom = c("x", "y"), crs = terra::crs(su.rast)
    )

    # Return objects
    if(to.disk == TRUE) {

      if(constrained == TRUE) {

        terra::writeVector(
          locations, base::file.path(outdir, "reploc_abs_con.shp"), ...
        )
        locations

      } else {

        terra::writeVector(
          locations, base::file.path(outdir, "reploc_abs_uncon.shp"), ...
        )
        locations

      }

    } else {

      locations

    }

  } else {

    if(verbose == TRUE){
      base::warning("Nothing was done. Please select a valid sampling method")
    }

  }

}
