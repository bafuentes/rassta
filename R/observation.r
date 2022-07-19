#' @title
#' Select the Representative Response Observation for Stratification Units
#'
#' @description
#' Selection of the \emph{representative response observation} for each
#' stratification unit occurring across geographic space. One observation from a
#' set of \emph{n} observations of a response variable sampled/measured within
#' the spatial boundaries of a given stratification unit is selected according
#' to the following criteria: (1) \emph{maximum landscape similarity}, (2)
#' \emph{median response value}, and (3) \emph{random selection}. The maximum
#' landscape similarity (mls) selects the observation at the \emph{XY} spatial
#' location where the landscape similarity value is maximized for a given
#' stratification unit. The median response value (mrv) selects the observation
#' whose response value is (closest to) the median of all response values, as
#' measured from the observations spatially enclosed by a given stratification
#' unit. The random selection, as implied by the name, randomly selects one
#' observation from the set of observations spatially enclosed by a given
#' stratification unit. See \strong{Details} for some guidance in the use of
#' this function for classification units.
#'
#' @param su.rast SpatRaster, as in \code{\link[terra]{rast}}. Single-layer
#'   SpatRaster representing the stratification units occurring across
#'   geographic space. Integer values are expected as cell values (i.e., numeric
#'   codes) of stratification units.
#' @param obs SpatVector, as in \code{\link[terra]{vect}}. Vector of point
#'   geometry whose tabular attributes should contain an ID column (1,2,...,n)
#'   and a column of the response' values.
#' @param col.id Integer. Index of the ID column in the tabular attributes of
#'   \emph{obs}.
#' @param col.resp Integer. Index of the response' values column in the tabular
#'   attributes of \emph{obs}.
#' @param method Character. String specifying the selection method for the
#'   response representative observation. Options are "mls" for the maximum
#'   landscape similarity value, "mrv" for the median response value, and
#'   "random" for random selection. Default: "mls"
#' @param ls.rast SpatRaster, as in \code{\link[terra]{rast}}. Multi-layer
#'   SpatRaster representing landscape similarities to stratification units.
#'   Only required if \emph{method = "mls"}.
#' @param verbose Boolean. Show warning messages in the console? Default: FALSE
#'
#' @return
#' A list with the following components:
#'
#' \strong{su.repobs}: Data table with the following attributes: (1) Original
#' IDs of the selected observation, (2) representative response value, (3)
#' stratification unit's numeric code, and (4) landscape similarity value at the
#' \emph{XY} location of the selected observation (only if \emph{method =
#' "mls"}).
#'
#' \strong{su.norepobs}: List of the numeric codes of stratification units
#' without observations.
#'
#' \strong{su.repobs.sp}: SpatVector of point geometry with the representative
#' response observation for each stratification unit represented in
#' \emph{su.rast}.
#'
#' @details
#' This selection scheme can be applied to classification units. For
#' classification units, one should replace the multi-layer SpatRaster of
#' landscape similarities with a multi-layer SpatRaster of spatial signatures.
#' One should also replace the raster layer of stratification units with that of
#' classification units.
#'
#' @examples
#' require(terra)
#' p <- system.file("exdat", package = "rassta")
#' # Single-layer SpatRaster of stratification units
#' fsu <- list.files(path = p, pattern = "strata.tif", full.names = TRUE)
#' su <- terra::rast(fsu)
#' # Observations with response values.
#' ## For this example, soil organic carbon (SOC) collected at 15 cm soil depth
#' fob <- list.files(path = p, pattern = "soc.shp", full.names = TRUE)
#' ob <- terra::vect(fob)
#' # Column indices for ID and measured response value
#' id <- 1
#' re <- 2
#' # Multi-layer SpatRaster of landscape similarities
#' fls <- list.files(path = p, pattern = "su_", full.names = TRUE)
#' ls <- terra::rast(fls)
#' # Selection of representative response observations for stratification units
#' ro <- observation(su.rast = su, obs = ob, col.id = id, col.resp = re,
#'                   ls.rast = ls
#'                  )
#' # Plot representative observations
#' if(interactive()){plot(su, type = "classes",
#'                        fun = function() points(ro$su_repobs.sp)
#'                       )}
#'
#' @export
#' @family
#' Functions for Stratified Sampling
#' @rdname
#' observation
#' @seealso
#' \code{\link{strata}}, \code{\link{similarity}}
#'
observation <- function(su.rast, obs, col.id, col.resp, method = "mls", ls.rast,
                        verbose = FALSE)
{

  #-----Binding variables to prevent devtools::check() notes-----#
  i <- SU <- NULL
  #--------------------------------------------------------------#

  # Set name of SpatRaster with SUs for consistency purposes
  base::names(su.rast) <- "SU"

  # Copy original observations and reduce attributes to just id and response
  obsSP <- obs[, c(col.id, col.resp)]

  # Extract SUs' numeric codes to each observation
  obsLS <- terra::extract(su.rast, obsSP, method = "simple", na.rm = TRUE)
  obsSP[["SU"]] <- obsLS[, 2]

  # Data frame from observations
  obsDT <- terra::as.data.frame(obsSP)

  # Design based on maximum landscape similarity to SU (mls)
  if(method == "mls" ) {

    # Extract SU's landscape similarity values to each observation
    obsLS <- terra::extract(ls.rast, obsSP, method = "simple", na.rm = TRUE)

    # Merge SU's landscape similarity values with DF of observations
    obsDAT <- base::merge.data.frame(
      obsDT, obsLS, by = base::colnames(obsDT)[1]
    )

    # Create empty data frame from observations id, response values, and SU
    repval <- obsDAT[0, 1:3]
    ## Add column for landscape similarity value
    repval$land_sim <- base::numeric()

    # For each SU, iterate through corresponding landscape similarity...
    # ...column and get representative observation
    `%do%` <- foreach::`%do%`
    foreach::foreach(i = 4:base::ncol(obsDAT)) %do% {

      # Data frame of observations with id, response value, SU, and SU's...
      # ...landscape similarity columns
      df <- obsDAT[, c(1:3, i)]

      # Just to make sure that column name (same as SU's numeric code)...
      # ...for landscape similarity column is numeric
      sucode <- stringi::stri_replace_all_regex(
        base::colnames(df)[4], "[a-zA-punct]", ""
      )
      sucode <- base::as.numeric(sucode)

      # Filter df rows that match the SU's numeric code -> Equivalent to...
      # ...extract by Location
      ## Concatenate grep metacharacters and SU's code structure to...
      ## ...prevent getting the rows of SUs with similar code structure
      gvar <- base::paste("^", sucode, "$", sep = "")
      ## Extract rows
      df <- df[c(base::grep(gvar, df$SU)), ]

      # Get observation with maximum landscape similarity value
      df <- df[base::order(-df[[4]]), ][1,]

      # Rename SU's landscape similarity column
      base::colnames(df)[4] <- "land_sim"

      # Append rows to data frame with representative observations
      repval <- base::rbind(repval, df)

    }

    ## Sort by ID
    repval <- repval[base::order(repval[[1]]), ]
    ## Reset row names
    base::rownames(repval) <- NULL

    # Get list of SU without representative observation
    ## Set of SU
    x <- terra::unique(su.rast)[, 1]
    ## Set of SU with representative observation
    y <- repval[["SU"]]
    ## Difference between sets
    norepval <- base::setdiff(x, y)

    # SpatVector of SUs' representative observations
    repval.sp <- terra::merge(
      obs, repval[, c(1, 3, 4)], by = base::colnames(repval)[1], all.x = FALSE
    )

    # Return objects
    base::list(
      su_repobs = repval,
      su_norepobs = norepval,
      su_repobs.sp = repval.sp
    )

  } else if(method == "mrv") {

    # Function to find observed value closest to the median of all...
    # ...observed values within SU
    closestval <- function(x) x[base::order(
      base::abs(x - stats::median(x))
    )[1]]

    # Column with observed response values
    resp <- base::colnames(obsDT)[2]

    # Su's observation with representative value
    `%>%` <- dplyr::`%>%`
    repval <- obsDT %>%
      dplyr::group_by(SU) %>%
      dplyr::mutate_at(
        dplyr::all_of(resp), base::list(response = closestval)
      )

    # Select only rows which corresponding observation has the...
    # ...representative value
    repval <- repval[base::which(repval[, 2] == repval[, 4]), ]
    ## Eliminate duplicates -> In cases where SU has two or more...
    ## ...observations with same rep. value
    repval <- repval[!(base::duplicated(repval[, 3])), ]
    ## Selecting final columns
    repval <- repval[, 1:3]

    # Get list of SU without representative observation
    ## Set of SU
    x <- terra::unique(su.rast)[, 1]
    ## Set of SU with representative observation
    y <- repval[["SU"]]
    ## Difference between sets
    norepval <- base::setdiff(x, y)

    # SpatVector of SUs' representative observations
    repval.sp <- terra::merge(
      obs, repval[, c(1,3)], by = base::colnames(repval)[1], all.x = FALSE
    )

    # Return objects
    base::list(
      su_repobs = repval,
      su_norepobs = norepval,
      su_repobs.sp = repval.sp
    )

  } else if(method == "random") {

    # Random selection of observation for each SU
    `%>%` <- dplyr::`%>%`
    repval <- obsDT %>% dplyr::group_by(SU) %>% dplyr::sample_n(1)

    # Get list of SU without representative observation
    ## Set of SU
    x <- terra::unique(su.rast)[, 1]
    ## Set of SU with representative observation
    y <- repval[["SU"]]
    ## Difference between sets
    norepval <- base::setdiff(x, y)

    # Spatial points data frame of SUs' representative observations
    repval.sp <- terra::merge(
      obs, repval[, c(1,3)], by = base::colnames(repval)[1], all.x = FALSE
    )

    # Return objects
    base::list(
      su_repobs = repval,
      su_norepobs = norepval,
      su_repobs.sp = repval.sp
    )

  } else {

    if(verbose == TRUE){

      base::warning("Nothing was done. Please select a valid selection method")

    }

  }

}
