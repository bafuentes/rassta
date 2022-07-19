#' @title
#' Predict Distribution Functions Across Geographic Space
#'
#' @description
#' Predicts constrained, univariate distribution functions across the geographic
#' space supported by raster layers. For a given continuous variable used to
#' create a classification unit, this function first calculates a user-defined
#' distribution function for that variable using only observations selected from
#' within the classification unit. In this way, the distribution function is
#' univariate and constrained. Subsequently, a \emph{locally-estimated
#' scatterplot smoothing} (LOESS) or a \emph{generalized additive model} (GAM)
#' is fitted. This model is fitted using the variable’s observations as
#' explanatory values and the values from the distribution function as the
#' response values. Finally, the fitted model is predicted on the complete
#' geographic space supported by the raster layer of the given variable. This
#' process is iterated for all of the continuous variables and classification
#' units. Each resulting layer can be thought of as a landscape correspondence
#' measurement between an \emph{XY} location in geographic space and the
#' landscape configuration represented by a given classification unit in terms
#' of a specific variable. The following distribution functions are currently
#' supported: the probability density function (PDF), the empirical cumulative
#' density function (ECDF), and the inverse of the empirical cumulative density
#' function (iECDF). Please refer to \strong{Details} for more information about
#' how each distribution function is calculated. Also, see details on parallel
#' processing.
#'
#' @param cuvar.rast SpatRaster, as in \code{\link[terra]{rast}}. Multi-layer
#'   SpatRaster containing \emph{n} continuous raster layers (i.e., variables)
#'   and one raster layer of classification units with integer cell values
#'   (i.e., Numeric identifiers).
#' @param cu.ind Integer. Position (index) of the raster layer of classification
#'   units in \emph{cuvar.rast}.
#' @param cu Integer. Vector of integer values that correspond to the numeric
#'   identifiers of the units in the raster layer of classification units.
#' @param vars Character. Vector of strings containing the names of the \emph{n}
#'   continuous variables in \emph{cuvar.rast}. These names have to be
#'   sequentially repeated according to the number of classification units (See
#'   \strong{Examples}).
#' @param dif Character. Vector of strings containing the distribution function
#'   to calculate for each continuous variable within each classification unit.
#'   The function will match the position of the name of the distribution
#'   function with that of the name of the continuous variable in \emph{vars}.
#' @param hist.type Character. Type of histogram to calculate. Options are
#'   "regular", "irregular" (unequally-sized bins, very slow), and "combined"
#'   (the one with greater penalized likelihood is returned). See
#'   \code{\link[histogram]{histogram}}. Default: "regular"
#' @param hist.pen Character. Penalty to apply when calculating the histogram
#'   (see \code{\link[histogram]{histogram}}). Default: "default"
#' @param grid.mult Numeric. Multiplying factor to increase/decrease the size of
#'   the "optimal" grid size for the \emph{Kernel Density Estimate} (KDE).
#'   Default: 1
#' @param kern Character. Type of kernel to use for the KDE. Default: "normal"
#' @param quant.sep Numeric. Spacing between quantiles for the calculation of
#'   the ECDF and iECDF. Quantiles are in the range of 0-1 thus spacing must be
#'   a decimal. Default: 0.01
#' @param method Character. Model to fit. Current options are "loess" for
#'   locally-estimated scatterplot smoothing (see \code{\link[stats]{loess}}),
#'   and "gam" for generalized additive model with support for large datasets
#'   (see \code{\link[mgcv]{bam}}). Default: "loess"
#' @param span Numeric. If \emph{method = "loess"}, degree of smoothing for
#'   LOESS fitting. Default: 0.6
#' @param k Numeric. If \emph{method = "gam"}, Number of knots for the
#'  \emph{cubic regression splines}. Default: 20
#' @param discrete Boolean. If \emph{method = "gam"}, discretize variables for
#'   storage and efficiency reasons? Can reduce processing time significantly.
#'   Default: TRUE
#' @param to.disk Boolean. Write the output raster layers of predicted
#'   distribution function to disk? See details an example about parallel
#'   processing. Default: FALSE
#' @param outdir Character. If \emph{to.disk = TRUE}, string specifying the path
#'   for the output raster layers of predicted distribution function. Default:
#'   "."
#' @param prefix Character. If \emph{to.disk = TRUE}, string specifying a prefix
#'   for the file names of the output raster layers of predicted distribution
#'   function. Default: ""
#' @param extension Character. If \emph{to.disk = TRUE}, string specifying the
#'   extension for the output raster layers of predicted distribution function.
#'   Default: ".tif"
#' @param overwrite Boolean. If \emph{to.disk = TRUE}, should raster layers in
#'   disk and with same name as the output raster layer(s) of predicted
#'   distribution function be overwritten? Default: FALSE
#' @param verbose Boolean. Show warning messages in the console? Default: FALSE
#' @param ... If \emph{to.disk = TRUE}, additional arguments as for
#'   \code{\link[terra]{writeRaster}}.
#'
#' @return
#' Single-layer or multi-layer SpatRaster with the predicted distribution
#' function for each variable and for each classification unit.
#'
#' @details
#' To calculate the PDF, this function uses the binned KDE for observations
#' drawn from the breaks of a regular/irregular histogram. The "optimal" number
#' of bins for the histogram is defined by calling the function
#' \code{\link[histogram]{histogram}} (Mildenberger et al., 2019) with the
#' user-defined penalty \emph{hist.pen}. Subsequently, the optimal number of
#' bins is treated as equivalent to the "optimal" grid size for the binned KDE.
#' The grid size can be adjusted by specifying the multiplying factor
#' \emph{grid.mult}. Lastly, the "optimal" bandwidth for the binned KDE is
#' calculated by applying the \emph{direct plugin} method of Sheather and Jones
#' (1991). For the calculation of optimal bandwidth and for the binned KDE, the
#' package \strong{KernSmooth} is called. To calculate both the ECDF and the
#' iECDF, this function calls the \code{\link[stats]{ecdf}} function on
#' equally-spaced quantiles. The spacing between quantiles can be manually
#' adjusted via \emph{quant.sep}. In the case of iECDF, the ECDF is inverted by
#' applying the formula: \emph{iECDF = ((x - max(ECDF)) * -1) + min(ECDF)};
#' where \emph{x} corresponds to each value of the ECDF.
#'
#' The "cu", "vars", and "dif" parameters of this function are configured such
#' that the output table from \code{\link{select_functions}} can be used
#' directly as input. (see \strong{Examples}).
#'
#' When writing output raster layer to disk, multiple distribution functions can
#' be predicted in parallel if a parallel backend is registered beforehand with
#' \code{\link[doParallel]{registerDoParallel}}. Keep in mind that the function
#' may require a large amount of memory when using a parallel backend with large
#' raster layers (i.e., high resolution and/or large spatial coverage).
#'
#' Some issues have been reported when manually creating cluster objects using
#' the \pkg{parallel} package. To overcome this issue, a cluster object can be
#' registered directly through \code{\link[doParallel]{registerDoParallel}}
#' without passing it first through \code{\link[parallel]{makeCluster}}. See
#' examples.
#'
#' @examples
#' require(terra)
#' p <- system.file("exdat", package = "rassta")
#' # Multi-layer SpatRaster of topographic variables
#' ## 3 continuous variables
#' ftva <- list.files(path = p, pattern = "^height|^slope|^wetness",
#'                    full.names = TRUE
#'                   )
#' tva <- terra::rast(ftva)
#' # Single-layer SpatRaster of topographic classification units
#' ## Five classification units
#' ftcu <- list.files(path = p, pattern = "topography.tif", full.names = TRUE)
#' tcu <- terra::rast(ftcu)
#' # Add the classification units to the SpatRaster of topographic variables
#' tcuvars <- c(tcu, tva)
#' # Data frame with source for "cu", "vars", and "dif"
#' ftdif <- list.files(path = p, pattern = "topodif.csv", full.names = TRUE)
#' tdif <- read.csv(ftdif)
#' # Check structure of source data frame
#' head(tdif)
#' # Predict distribution functions
#' ## 1 distribution function per variable and classification unit = 1
#' tpdif <- predict_functions(cuvar.rast = tcuvars,
#'                            cu.ind = 1,
#'                            cu = tdif$Class.Unit[1:3],
#'                            vars = tdif$Variable[1:3],
#'                            dif = tdif$Dist.Func[1:3],
#'                            grid.mult = 3,
#'                            span = 0.9
#'                          )
#' # Plot predicted distribution functions
#' if(interactive()){plot(tpdif, col = hcl.colors(100, "Oslo", rev = TRUE))}
#'
#' #--------
#' # Writing results to disk and parallel processing
#'
#' if(interactive()){
#'   # Directory for temporary files
#'   o <- tempdir()
#'   # Register parallel backend
#'   require(doParallel)
#'   registerDoParallel(4)
#'   # Predict distribution functions
#'   tpdif <- predict_functions(cuvar.rast = tcuvars,
#'                              cu.ind = 1,
#'                              cu = tdif$Class.Unit[1:3],
#'                              vars = tdif$Variable[1:3],
#'                              dif = tdif$Dist.Func[1:3],
#'                              grid.mult = 3, span = 0.9,
#'                              to.disk = TRUE,
#'                              outdir = o
#'                          )
#'    # Stop cluster
#'    stopImplicitCluster()
#'    # Clean temporary files
#'    file.remove(sources(tpdif))
#'  }
#'
#' @export
#' @family
#' Landscape Correspondence Metrics
#' @rdname
#' predict_functions
#' @references
#' T. Mildenberger, Y. Rozenholc, and D. Zasada. histogram: Construction of
#' Regular and Irregular Histograms with Different Options for Automatic Choice
#' of Bins, 2019. \url{https://CRAN.R-project.org/package=histogram}
#'
#' S. Sheather and M. Jones. A reliable data-based bandwidth selection method
#' for kernel density estimation. Journal of the Royal Statistical Society.
#' Series B. Methodological, 53:683–690, 1991.
#'
predict_functions <- function(cuvar.rast, cu.ind, cu, vars, dif,
                              hist.type = "regular", hist.pen = "default",
                              grid.mult = 1, kern = "normal", quant.sep = 0.01,
                              method = "loess", span = 0.6, k = 20,
                              discrete = TRUE, to.disk = FALSE, outdir = ".",
                              prefix = "", extension = ".tif",
                              overwrite = FALSE, verbose = FALSE,
                              ...)
{
  #-----Binding variables to prevent devtools::check() notes-----#
  i <- NULL
  #--------------------------------------------------------------#
  # Matrix from SpatRaster
  rasmat <- stats::na.omit(terra::values(cuvar.rast))

  # Inputs as vectors
  cu <- as.vector(cu)
  vars <- as.vector(vars)
  dif <- as.vector(dif)

  # Define processing scheme (disk [parallel] VS memory [sequential])
  `%ps%` <- if(to.disk == TRUE) {foreach::`%dopar%`} else {foreach::`%do%`}

  # Loop function
  pdif <- foreach::foreach(i = 1:base::length(dif)) %ps% {

    if (dif[i] == 'PDF') {

      # Extract variable observations within specific CU
      rvar <- rasmat[base::which(rasmat[, cu.ind] == cu[i]), vars[i]]

      # Estimation of optimum number of regular bins for histogram
      var_hist <- histogram::histogram(
        rvar,
        type = hist.type,
        penalty = hist.pen,
        plot = FALSE,
        greedy = TRUE,
        verbose = FALSE
      )

      # Get number of bins as optimum grid size for KDE
      optgridsize <- base::length(var_hist[["breaks"]]) * grid.mult

      # Estimation of optimum bandwidth via the direct plugin method (DPI)
      optbw <- KernSmooth::dpik(
        rvar,
        scalest = "iqr",
        level = 2,
        kernel = kern,
        gridsize = optgridsize
      )

      # KDE (binned)
      var_kde <- KernSmooth::bkde(
        rvar,
        kernel = kern,
        bandwidth = base::as.numeric(optbw),
        gridsize = optgridsize,
        truncate = FALSE
      )

      # Create dataframe with variables for model fit
      fit_df <- base::as.data.frame(base::cbind(var_kde[[1]], var_kde[[2]]))
      base::colnames(fit_df) <- c("x", "y")

      # Normalize KDE (0-100)
      fit_df$y <- scales::rescale(
        fit_df$y,
        to = c(0, 100),
        from = base::range(fit_df$y, na.rm = TRUE, finite = TRUE)
      )

      # Clean memory
      rm(rvar, var_hist, optgridsize, optbw, var_kde)
      gc()

    } else if (dif[i] == 'ECDF') {

      # Extract variable observations within specific CU
      rvar <- rasmat[base::which(rasmat[, cu.ind] == cu[i]), vars[i]]

      # Construction of ECDF
      var_ecdf <- stats::ecdf(rvar)

      # Create dataframe with quantiles for model fit
      fit_df <- base::data.frame(
        "x" = (stats::quantile(rvar, probs = base::seq(0, 1, quant.sep))),
        row.names = NULL
      )

      # ECDF to extracted quantiles
      fit_df$y <- var_ecdf(fit_df$x)

      # Normalize ECDF (0-100)
      fit_df$y <- fit_df$y * 100

      # Clean memory
      rm(rvar, var_ecdf)
      gc()

    } else if (dif[i] == 'iECDF') {

      # Extract variable observations within specific CU
      rvar <- rasmat[base::which(rasmat[, cu.ind] == cu[i]), vars[i]]

      # Construction of ECDF
      var_ecdf <- stats::ecdf(rvar)

      # Create data frame with quantiles for model fit
      fit_df <- base::data.frame(
        "x" = (stats::quantile(rvar, probs = base::seq(0, 1, quant.sep))),
        row.names = NULL
      )

      # ECDF to extracted quantiles
      fit_df$y <- var_ecdf(fit_df$x)

      # Normalize ECDF (0-100)
      fit_df$y <- fit_df$y * 100

      # Invert ECDF
      fit_df$y <- ((fit_df$y - max(fit_df$y)) * -1) + min(fit_df$y)
      gc()

      # Clean memory
      rm(rvar, var_ecdf)
      gc()

    } else {

      if(verbose == TRUE){
        base::warning(
          "Nothing was done. No distribution function was recognized."
        )
      }

    }

    # Prepare data frame for model fit
    base::colnames(fit_df)[1] <- base::names(cuvar.rast[[vars[i]]])

    # Fit
    if(method == "loess") {

      # Prepare formula
      fm <- base::paste("y", "~", base::names(cuvar.rast[[vars[i]]]))

      # Fit
      fit <- stats::loess(
        formula = stats::formula(fm),
        data = fit_df,
        model = TRUE,
        normalize = FALSE,
        degree = 1,
        span = span,
        family = "gaussian",
        method = "loess",
        surface = "direct",
        statistics = "exact"
      )

    } else {

      if(rlang::is_installed("mgcv") == FALSE) {

        base::stop(
          'Package \"mgcv\" must be installed to use method = \"gam\"'
        )

      } else {

        # Prepare formula
        fm <- base::paste(
          "y", " ~ ",
          "s(",
          base::names(cuvar.rast[[vars[i]]]), ", bs = 'cr', k = ", k,
          ")",
          sep = ""
        )

        # Fit
        fit <- mgcv::bam(
          formula = stats::formula(fm),
          data = fit_df,
          family = stats::gaussian(link = "identity"),
          method = "fREML",
          select = FALSE,
          fit = TRUE,
          discrete = discrete,
          nthreads = 1,
          cluster = NULL
        )

      }

    }

    # Names for output raster layer of predicted distribution function
    cname <- base::paste(prefix, cu[i], sep = "")
    lname <- base::paste(cname, vars[i], sep = "_")      # Layer name
    rastname <- base::paste(lname, extension, sep = "")  # File name

    # > Disk-based writing < #
    if(to.disk == TRUE) {

      if(method == "loess") {

        # Predict LOESS on variable across entire geographic space
        pred <- terra::predict(
          object = cuvar.rast[[vars[i]]],
          model = fit,
          na.rm = TRUE,
          cores = 1,
          filename =  base::file.path(outdir, rastname),
          overwrite = overwrite,
          wopt = base::list(datatype = 'FLT4S', names = lname, ...)
        )

      } else {

        # Predict BAM on variable across entire geographic space
        pred <- terra::predict(
          object = cuvar.rast[[vars[i]]],
          model = fit,
          type = "response", newdata.guaranteed = TRUE,
          discrete = discrete, n.threads = 1, cluster = NULL,
          na.rm = TRUE,
          cores = 1,
          cpkgs = "mgcv",
          filename =  base::file.path(outdir, rastname),
          overwrite = overwrite,
          wopt = base::list(datatype = 'FLT4S', names = lname, ...)
        )

      }

      # Truncate range of predictions
      pred <- terra::ifel(
        pred > 100, 100.000,
        terra::ifel(pred < 0, 0, pred),
        filename =  base::file.path(outdir, rastname),
        overwrite = TRUE,
        ...
      )

      # Retrieve file name for raster of predicted distribution function
      rastname

    # > Memory-based writing < #
    } else {

      if(method == "loess") {

        # Predict LOESS on variable across entire geographic space
        pred <- terra::predict(
          object = cuvar.rast[[vars[i]]],
          model = fit,
          na.rm = TRUE,
          cores = 1
        )

      } else {

        # Predict BAM on variable across entire geographic space
        pred <- terra::predict(
          object = cuvar.rast[[vars[i]]],
          model = fit,
          type = "response", newdata.guaranteed = TRUE,
          discrete = discrete, n.threads = 1, cluster = NULL,
          na.rm = TRUE,
          cores = 1,
          cpkgs = "mgcv"
        )
      }

      # Set names
      base::names(pred) <- lname
      terra::varnames(pred) <- lname

      # Truncate range of predictions
      pred <- terra::ifel(
        pred > 100, 100.000,
        terra::ifel(pred < 0, 0, pred)
      )

    }

  }

  # Clean memory
  remove(rasmat)
  gc()

  # Retrieve predicted distribution functions
  if(to.disk == TRUE) {

    # Retrieve raster layers of predicted distribution function from disk
    pdif <- base::unlist(pdif)
    pdif <- base::paste("^", pdif, "$", sep = "")
    pdif <- base::lapply(pdif, list.files, path = outdir, full.names = TRUE)
    pdif <- terra::rast(base::unlist(pdif))

  } else {

    # Retrieve raster layers of predicted distribution function from memory
    pdif <- terra::rast(pdif)

  }

}
