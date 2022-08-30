#' @title
#' Reproduce Figures from Fuentes et al. (n.d.)
#'
#' @description
#' This function is intended to reproduce the figures presented in \emph{rassta:
#' Raster-based Spatial Stratification Algorithms} (Fuentes et al., 2021). Note
#' that this function assumes that all the necessary inputs for each figure are
#' loaded in the working environment. For the creation of each input, please
#' refer to the data and examples presented in the aforementioned work. Also,
#' please note that the use of this function is not intended for RStudio.
#'
#' @param x Integer. Number identifying the figure to reproduce.
#' @param d List. List with the data required for the figure to reproduce.
#' @param scaling Integer. This number scales (i.e., resizes) the R's plotting
#'   device, such that \emph{width = x/scaling & height = x/scaling},
#'   with \emph{x} = pixels. The default pixel size (not adjustable) and scaling
#'   value should work fine. Default = 100
#' @param to.disk Boolean. Save figure to disk? Default = FALSE
#' @param verbose Boolean. Show warning messages in the console? Default: FALSE
#'
#' @return
#' None
#'
#' @examples
#' if(interactive()){
#' require(terra)
#' p <- system.file("exdat", package = "rassta")
#' # Single-layer SpatRaster of geologic units
#' f <- list.files(path = p, pattern = "geology.tif", full.names = TRUE)
#' geol <- terra::rast(f)
#' # Dummy layers from geologic units
#' mat.sig <- dummies(ca.rast = geol, preval = 100, absval = 0)
#' figure(17, d = mat.sig)
#' }
#'
#' @export
#' @family
#' Miscellaneous Functions
#' @rdname
#' figure
#' @references
#' B.A. Fuentes, M.J. Dorantes, and J.R. Tipton. rassta: Raster-based Spatial
#' Stratification Algorithms. EarthArXiv, 2021.
#' \doi{https://doi.org/10.31223/X50S57}
figure <- function(x, d, scaling = 100, to.disk = FALSE, verbose = FALSE) {

  if(x == 4) {

    # Get user's graphical parameters
    userpar <- graphics::par(no.readonly = TRUE)
    # Set size of graphic device
    grDevices::dev.new(width = 1195/scaling, height = 431/scaling, unit = "px",
                       noRStudioGD = FALSE
                      )

    # Set graphics arrangement
    graphics::par(mfrow = c(2, 5))
    # Define color ramps for terrain variables
    nc <- c("Zissou", "Batlow", "Lajolla", "Spectral")
    # Plot all terrain variables
    for(i in 1:4){terra::plot(d[[1]][[i]], main = base::names(d[[1]])[i],
                                    mar = c(1.5, 1.5, 1.5, 3.5),
                                    col = grDevices::hcl.colors(100, nc[i])
                                  )
    }
    # Modify size of figures and text for next elements
    graphics::par(mex = 0.5)
    # Plot SOM codes
    terra::plot(d[[2]]$SOM, main = "SOM - Codes")
    # Plot SOM elements (variables)
    for(i in 1:4){terra::plot(d[[2]]$SOM, type = "property",
                              shape = "straight",
                              property = kohonen::getCodes(d[[2]]$SOM, 1)[,i],
                              main = base::paste("SOM -",
                          base::colnames(kohonen::getCodes(d[[2]]$SOM, 1))[i]
                        )
                      )
    }
    # Set graphics arrangement for next element
    graphics::par(mar = c(6,6,6,1))
    # Plot results from gap statistic for PAM
    terra::plot(d[[2]]$SOMgap, main = "Gap Statistic", mex = 9)
    # Mark optimum number of clusters
    graphics::abline(v = d[[2]]$Kopt)

    # Write to disk?
    if(to.disk == TRUE) { grDevices::savePlot("figure_4.png", type = "png") }

    # Restore user's graphical parameters
    base::on.exit(graphics::par(userpar))

  } else if(x == 5) {

    # Get user's graphical parameters
    userpar <- graphics::par(no.readonly = TRUE)
    # Set size of graphic device
    grDevices::dev.new(width = 1264/scaling, height = 590/scaling, unit = "px",
                       noRStudioGD = FALSE
                      )

    # Set graphics arrangement
    graphics::par(mfrow = c(1,2))
    # Extract rasterized SOM grid
    terr.rsom <- d[[1]]$sompam.rast[[1]]
    # Plot rasterized SOM grid
    terra::plot(terr.rsom, main = "Terrain Self-Organizing Map",
                mar = c(2, 2.5, 2, 3.5),
                col = grDevices::hcl.colors(100, "spectral", rev = TRUE)
              )
    # Extract rasterized PAM solution
    terr.cu <- d[[1]]$sompam.rast[[2]]
    # Reclassify PAM assignments based on terrain height (for better visualization)
    ## With terra::zonal(), unit-wise mean value of terrain height
    terr.stat <- terra::zonal(d[[2]]$height, terr.cu, fun = mean)
    ## Order PAM assignments based on terrain height (descending)
    terr.stat <- terr.stat[base::order(terr.stat$height, decreasing = TRUE), ]
    ## Column with original PAM assignments
    terr.stat$CU <- base::seq(1, 8)
    ## With terra::classify(), reclassify values of PAM assignments
    terr.cu <- terra::classify(terr.cu, terr.stat[, c(1,3)])
    terra::plot(terr.cu, type = "classes",
                col = grDevices::hcl.colors(8, "spectral"),
                main = "Terrain Classification Units (PAM clustering)",
                mar = c(2, 2.5, 2, 3.5)
              )

    # Write to disk?
    if(to.disk == TRUE) { grDevices::savePlot("figure_5.png", type = "png") }

    # Restore user's graphical parameters
    base::on.exit(graphics::par(userpar))

  } else if(x == 6){

    # Get user's graphical parameters
    userpar <- graphics::par(no.readonly = TRUE)
    # Set size of graphic device
    grDevices::dev.new(width = 990/scaling, height = 445/scaling, unit = "px",
                       noRStudioGD = FALSE
                      )

    # Set graphics arrangement
    graphics::par(mfrow = c(2,2))
    # Plot stratification units
    terra::plot(d[[1]]$su.rast, type = "classes",
                col = grDevices::hcl.colors(56, "spectral"),
                mar = c(1.5, 1.5, 1.5, 18), plg = list(ncol = 4),
                main = "Stratification Units"
              )
    # Plot climatic classification units
    terra::plot(d[[2]][[1]], type = "classes",
                col = grDevices::hcl.colors(4, "spectral"),
                mar = c(1.5, 1.5, 1.5, 18),
                main = "Climatic Classification Units",
                levels = c("1. Highest Rainfall and Lowest Temperature",
                           "2. High Rainfall and Low Temperature",
                           "3. Low Rainfall and High Temperature",
                           "4. Lowest Rainfall and Highest Temperature"
                          )
              )
    # Plot soil parent material units
    terra::plot(d[[2]][[2]], type = "classes",
                col = grDevices::hcl.colors(6, "spectral"),
                mar = c(1.5, 1.5, 1.5, 18),
                main = "Soil Parent Material Units",
                levels = c("1. Igneous",
                           "2. Sedimentary",
                           "3. Alluvium - Moderately weathered ",
                           "4. Alluvium - Somewhat weathered",
                           "5. Alluvium - Rich in organic matter",
                           "6. Alluvium - Rich in clay and organic matter"
                          )
              )
    # Plot terrain classification units
    terra::plot(d[[2]][[3]], type = "classes",
                col = grDevices::hcl.colors(8, "spectral"),
                mar = c(1.5, 1.5, 1.5, 18),
                main = "Terrain Classification Units",
                levels = c("1. Summit", "2. Shoulder",
                           "3. Backslope", "4. Backslope - Steep",
                           "5. Footslope", "6. Footslope - Steep",
                           "7. Toeslope", "8. Floodplain"
                          )
              )

    # Write to disk?
    if(to.disk == TRUE) { grDevices::savePlot("figure_6.png", type = "png") }

    # Restore user's graphical parameters
    base::on.exit(graphics::par(userpar))

  } else if(x == 8) {

    # Get user's graphical parameters
    userpar <- graphics::par(no.readonly = TRUE)
    # Set size of graphic device
    grDevices::dev.new(width = 1500/scaling, height = 495/scaling, unit = "px",
                       noRStudioGD = FALSE
                      )

    # Set graphics arrangement
    graphics::par(mfrow = c(1,4))
    # Plot climatic variables
    for(i in 1:2){terra::plot(d[[3]][[i]],
                                    main = c("Precipitation (Annual)",
                                             "Temperature (Mean Annual)"
                                            )[i],
                                    col = grDevices::hcl.colors(100,
                                                                "spectral",
                                                                rev = c(FALSE,
                                                                        TRUE
                                                                        )[i]
                                                              ),
                                    mar = c(14, 2, 2, 3.5)
                                  )
    }
    # Plot climatic classification units
    terra::plot(d[[2]],
                col = grDevices::hcl.colors(4, "spectral"),
                type = "classes",
                main = "Climatic Classification Units",
                mar = c(14, 2, 2, 3.5)
              )
    # Add table with selected distribution functions
    graphics::par(fig = c(0, 1, 0, 1), cex = 0.8, new = TRUE)
    graphics::legend(x = 'bottomright', inset = c(-0.08, 0), ncol = 3,
                     title = 'Selected Distribution Functions',
                     legend = c('Unit', as.vector(d[[1]]$distfun$Class.Unit),
                                'Variable', as.vector(d[[1]]$distfun$Variable),
                                'Function', d[[1]]$distfun$Dist.Func
                              )
                    )

    # Write to disk?
    if(to.disk == TRUE) { grDevices::savePlot("figure_8.png", type = "png") }

    # Restore user's graphical parameters
    base::on.exit(graphics::par(userpar))

  } else if(x == 9) {

    # Get user's graphical parameters
    userpar <- graphics::par(no.readonly = TRUE)
    # Set size of graphic device
    grDevices::dev.new(width = 710/scaling, height = 431/scaling, unit = "px",
                       noRStudioGD = FALSE
                      )

    # Set graphics arrangement
    graphics::par(mfrow = c(3,3))
    # Plot climatic classification units
    terra::plot(d[[2]],
                col = grDevices::hcl.colors(4, "Spectral"),
                mar = c(1.5, 5, 1.5, 5),
                main = "Climatic Classification Units"
              )
    # Plot distribution functions predicted across geographic space
    for(i in 1:8){terra::plot(d[[1]][[i]],
                                    col = grDevices::hcl.colors(300,
                                                                "Oslo",
                                                                rev = TRUE
                                                              ),
                                    mar = c(1.5, 5, 1.5, 5),
                                    main = base::names(d[[1]])[i]
                                  )
    }

    # Write to disk?
    if(to.disk == TRUE) { grDevices::savePlot("figure_9.png", type = "png") }

    # Restore user's graphical parameters
    base::on.exit(graphics::par(userpar))

  } else if(x == 10) {

    # Get user's graphical parameters
    userpar <- graphics::par(no.readonly = TRUE)
    # Set size of graphic device
    grDevices::dev.new(width = 1350/scaling, height = 475/scaling, unit = "px",
                       noRStudioGD = FALSE
                      )

    # Set graphics arrangement
    graphics::par(mfrow = c(1,5))
    # Plot climatic classification units
    terra::plot(d[[2]],
                col = grDevices::hcl.colors(4, "Spectral"),
                mar = c(20, 2, 1.5, 4),
                main = "Climatic Classification Units"
              )
    # Plot spatial signatures for climatic classification units
    for(i in 1:4){terra::plot(d[[1]][[i]],
                                    col = grDevices::hcl.colors(300,
                                                                "Oslo",
                                                                rev = TRUE
                                                              ),
                                    mar = c(20, 2, 1.5, 4),
                                    main = base::names(d[[1]])[i]
                                  )
    }

    # Write to disk?
    if(to.disk == TRUE) { grDevices::savePlot("figure_10.png", type = "png") }

    # Restore user's graphical parameters
    base::on.exit(graphics::par(userpar))

  } else if(x == 12) {

    # Get user's graphical parameters
    userpar <- graphics::par(no.readonly = TRUE)
    # Set size of graphic device
    grDevices::dev.new(width = 1200/scaling, height = 520/scaling, unit = "px",
                       noRStudioGD = FALSE
                      )

  # Multi-layer SpatRaster with signatures and landscape similarity for SU = 111
    su111 <- c(d[[1]]$landsim[[1]], d[[3]][[1]], d[[4]][[1]], d[[5]][[1]])
  # Multi-layer SpatRaster with signatures and landscape similarity for SU = 468
    su468 <- c(d[[1]]$landsim[[56]], d[[3]][[4]], d[[4]][[6]], d[[5]][[8]])
    # Spatial boundaries for SU = 111 with terra::classify() and ::as.polygon()
    b111 <- terra::classify(d[[2]], cbind(111, 1), others = NA)
    b111 <- terra::as.polygons(b111)
    # Spatial boundaries for SU = 468 with terra::classify() and ::as.polygon()
    b468 <- terra::classify(d[[2]], cbind(468, 1), others = NA)
    b468 <- terra::as.polygons(b468)
    # Set graphics arrangement
    graphics::par(mfrow = c(2,4))
  # Map of landscape similarity, spatial signatures and boundaries of SU = 1101
    for(i in 1:4){terra::plot(su111[[i]],
                              col = grDevices::hcl.colors(100,
                                                          "Oslo",
                                                          rev = TRUE
                                                        ),
                              mar = c(1.5, 2, 1.5, 4),
                              fun = function() terra::polys(b111,
                                                            col = NA,
                                                            border = "darkred",
                                                            lwd = 1
                                                          ),
                              main = c("Landscape Similarity to SU = 111",
                                       base::paste("Spatial Signature:",
                                                   base::names(su111[[2:4]])
                                                  )
                                      )[i]
                            )
    }
    # Map of landscape similarity, spatial signatures and boundaries of SU = 428
    for(i in 1:4){terra::plot(su468[[i]],
                                    col = grDevices::hcl.colors(100,
                                                                "Oslo",
                                                                rev = TRUE
                                                              ),
                                    mar = c(1.5, 2, 1.5, 4),
                                    fun = function() terra::polys(b468,
                                                           col = NA,
                                                           border = "darkred",
                                                           lwd = 1
                                                          ),
                                    main = c("Landscape Similarity to SU = 468",
                                             base::paste("Spatial Signature:",
                                                      base::names(su468[[2:4]]))
                                            )[i]
                                  )
    }

    # Write to disk?
    if(to.disk == TRUE) { grDevices::savePlot("figure_12.png", type = "png") }

    # Restore user's graphical parameters
    base::on.exit(graphics::par(userpar))

  } else if(x == 13) {

    # Get user's graphical parameters
    userpar <- graphics::par(no.readonly = TRUE)
    # Set size of graphic device
    grDevices::dev.new(width = 1060/scaling, height = 431/scaling, unit = "px",
                       noRStudioGD = FALSE
                      )

    # Set graphics arrangement
    graphics::par(mfrow = c(1,2))
    # Plot stratification units and response observations
    terra::plot(d[[3]], type = "classes",
                col = grDevices::hcl.colors(56, "spectral"),
                mar = c(2,2,2,7),
                legend = FALSE,
                main = base::paste("Soil Organic Carbon Observations"),
                fun = function() c(terra::points(d[[2]],
                                                 pch = 21,
                                                 bg = grDevices::rgb(0,1,0,1)
                                                ),
                                   terra::points(d[[1]]$su_repobs.sp,
                                                 pch = 21,
                                                 bg = grDevices::rgb(0,0,1,1)
                                                )
                                  )
              )
    # Set new graphics arrangement
    graphics::par(mar = c(4.5,4,2,1))
    # Plot histogram of soil organic carbon values from all observations
    graphics::hist(d[[2]]$soc, 4, col = grDevices::rgb(0,1,0,0.8),
                   main = "Histogram of Soil Organic Carbon (%)",
                   xlab = "soil organic carbon (%)"
                  )
    # Plot histogram of soil organic carbon values from representative observations
    graphics::hist(d[[1]]$su_repobs.sp$soc, 4, add = T, col = grDevices::rgb(0,0,1,0.9))
    # Add legend
    graphics::legend("topright",
                     legend = c("All observations", "Representative Observations"),
                     col = c(grDevices::rgb(0,1,0,0.8), grDevices::rgb(0,0,1,0.9)),
                     pch = 20,
                     bty = "n",
                     pt.cex = 2,
                     cex = 1,
                     text.col = "black",
                     horiz = F ,
                     inset = c(-0.01, 0.1)
                    )

    # Write to disk?
    if(to.disk == TRUE) { grDevices::savePlot("figure_13.png", type = "png") }

    # Restore user's graphical parameters
    base::on.exit(graphics::par(userpar))

  } else if(x == 14) {

    # Get user's graphical parameters
    userpar <- graphics::par(no.readonly = TRUE)
    # Set size of graphic device
    grDevices::dev.new(width = 1180/scaling, height = 431/scaling, unit = "px",
                       noRStudioGD = FALSE
                      )

    # Set new graphics arrangement
    graphics::par(mfrow = c(1,2))
    # Plot stratification units, representative sampling locations and buffer areas
    terra::plot(d[[2]], type = "classes",
                col = grDevices::hcl.colors(56, "spectral"),
                legend = FALSE,
                mar = c(2,6,2,6),
                main = base::paste("Representative Sampling Locations for Stratification units"),
                fun = function() c(terra::polys(d[[1]]$buffers,
                                                col = grDevices::rgb(0,1,0,0.5)
                                              ),
                                   terra::points(d[[1]]$locations,
                                                 pch = 21,
                                                 col = "black",
                                                 bg = grDevices::rgb(0,1,0,1)
                                                )
                                  )
              )
    # Set new graphics arrangement
    graphics::par(mar = c(4.5,4,2,1))
    # Plot histogram of landscape similarity values at sampling locations
    graphics::hist(d[[1]]$locations$land_sim, 4,
                   main = "Histogram of Landscape Similarity Values",
                   xlab = "Landscape Similarity at Sampling Location"
                  )

    # Write to disk?
    if(to.disk == TRUE) { grDevices::savePlot("figure_14.png", type = "png") }

    # Restore user's graphical parameters
    base::on.exit(graphics::par(userpar))

  } else if(x == 16) {

    # Get user's graphical parameters
    userpar <- graphics::par(no.readonly = TRUE)
    # Set size of graphic device
    grDevices::dev.new(width = 1200/scaling, height = 500/scaling, unit = "px",
                       noRStudioGD = FALSE
                      )

    # Set graphics arrangement
    graphics::par(mfrow = c(1,2))
    # Plot modeled soil organic carbon
    terra::plot(d[[1]],
                col = grDevices::hcl.colors(100, "Fall", rev = TRUE),
                main = "Modeled Soil Organic Carbon (%)"
              )
    # Evaluate modeling performance
    ## SpatVector with independent set of SOC measurements
    evalobs <- terra::vect(d[[2]])
    ## From terra::extract(), modeled values to each independent sample location
    evalmodel <- terra::extract(d[[1]], evalobs)
    base::names(evalmodel)[2] <- "soc_rassta"
    ## Table with measured and modeled SOC values
    evalobs <- base::cbind(evalobs, evalmodel[2])
    ## RMSE and MAE
    eval.rmse <- base::sqrt(base::mean((evalobs$soc-evalobs$soc_rassta)^2))
    eval.mae <- base::mean(base::abs(evalobs$soc-evalobs$soc_rassta))
    # Set new graphics arrangement
    graphics::par(mar = c(5,4,2,2))
    # Plot measured versus modeled SOC
    graphics::plot(evalobs$soc, evalobs$soc_rassta,
                   xlab = "Measured Soil Organic Carbon (%)",
                   ylab = "Modeled Soil Organic Carbon (%)"
                  )
    graphics::abline(0, 1, lty = "dashed")
    graphics::text(x = 3.6, y = 5.8,
                   base::paste("Root mean squared error: ",
                               base::round(eval.rmse,2), "%",
                               sep = ""
                              )
                  )
    graphics::text(x = 3.485, y = 5.6,
                   base::paste("Mean Absolute Error: ",
                               base::round(eval.mae,2), "%",
                               sep = ""
                              )
                  )

    # Write to disk?
    if(to.disk == TRUE) { grDevices::savePlot("figure_16.png", type = "png") }

    # Restore user's graphical parameters
    base::on.exit(graphics::par(userpar))

  } else if(x == 17) {

    # Get user's graphical parameters
    userpar <- graphics::par(no.readonly = TRUE)
    # Set size of graphic device
    grDevices::dev.new(width = 1399/scaling, height = 214/scaling, unit = "px",
                       noRStudioGD = FALSE
                      )

    # Plot binary layers
    terra::plot(d,
                nc = 6,
                col = grDevices::hcl.colors(100, "Oslo", rev = TRUE)
              )

    # Write to disk?
    if(to.disk == TRUE) { grDevices::savePlot("figure_17.png", type = "png") }

    # Restore user's graphical parameters
    base::on.exit(graphics::par(userpar))

  } else {

    if(verbose == TRUE){
      base::warning("Nothing to plot. Check figure number and/or inputs")
    }

  }

}
