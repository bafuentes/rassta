#' @title
#' Select Constrained Univariate Distribution Functions
#'
#' @description
#' Selection of distribution functions for continuous raster layers that were
#' used to create a raster layer of classification units. The distribution
#' functions currently supported are the probability density function (PDF), the
#' empirical cumulative density function (ECDF), and the inverse of the
#' empirical cumulative density function (iECDF). Please note that
#' \code{\link{select_functions}} DOES NOT calculate the aforementioned
#' distribution functions. The sole purpose of \code{\link{select_functions}} is
#' to assist in the knowledge-driven selection of the most appropriate
#' distribution function for each variable of a given classification unit (see
#' \strong{Details}).
#'
#' @param cu.rast SpatRaster, as in \code{\link[terra]{rast}}. Single-layer
#'   SpatRaster representing the classification units occurring across
#'   geographic space. The cell values (i.e., numeric IDs) for classification
#'   units must be integer values.
#' @param var.rast SpatRaster. Multi-layer SpatRaster containing the \emph{n}
#'   continuous raster layers of the variables used to create the classification
#'   units.
#' @param fun Character. Descriptive statistical measurement (e.g., mean, max).
#'   See \code{\link[terra]{zonal}}. Default: mean
#' @param varscale Character. Variable scaling method. See \emph{scale} argument
#'   in \code{\link[GGally]{ggparcoord}}. Default: "uniminmax"
#' @param mode Character. String specifying the selection mode for univariate
#'   distribution functions. Possible values are "inter" for interactive
#'   selection, and "auto" for automatic selection (see Details). Default:
#'   "auto"
#' @param verbose Boolean. Show warning messages in the console? Default: FALSE
#' @param ... Additional arguments as for \code{\link[GGally]{ggparcoord}}.
#'
#' @return
#' If \emph{mode = "inter"}:
#'
#' \strong{distfun}: A DT table (DataTables library) with the following
#' attributes: (1) \emph{Class.Unit} = numeric ID for classification units, (2)
#' \emph{Variable} = each of the \emph{n} continuous raster layers of a
#' classification unit, and (3) \emph{Dist.Func} = Empty column whose cells can
#' be filled with the following strings: "PDF, "ECDF", and "iECDF" (unquoted).
#' This table can be saved on disk through the Shiny interface.
#'
#' \strong{parcoord}: A ploty-based parallel coordinate plot which can be saved
#' on disk through \code{\link[htmlwidgets]{saveWidget}}.
#'
#' If \emph{mode = "auto"}:
#'
#' \strong{distfun}: Same as \strong{distfun} when \emph{mode = "inter"}, except
#' for column "Dist.Func" whose cells were automatically filled.
#'
#' \strong{parcoord}: Same as \strong{parcoord} when \emph{mode = "inter"}.
#'
#' @details
#' The selection of distribution functions is univariate, that is, for each
#' variable, and it is constrained, meaning that the selection has to be made
#' for each classification unit. The rationale behind the selection process is
#' that given a classification unit and a variable, the 'optimal' conditions (in
#' feature space) for the occurrence of that classification unit can be
#' associated with the lowest (iECDF), highest (ECDF), or any value between the
#' lowest and highest values of the variable (PDF).
#'
#' In order to assist the selection process, when \emph{mode = "inter"}, this
#' function displays an interactive parallel coordinates plot (see
#' \code{\link[plotly]{ggplotly}}) and a writable table (built in Shiny). For
#' each variable, the parallel coordinates plot shows a trend of a descriptive
#' statistical measurement (argument \emph{fun}) across all of the
#' classification units. Using this trend, one can then select the most
#' appropriate distribution function for each variable based on the
#' occurrence/absence of \strong{"peaks"} and \strong{"pits"} in the observed
#' trend. For instance, a peak (highest point in the trend) would indicate that
#' the given classification unit contains on average, the highest values of that
#' variable. Conversely, a pit (lowest point in the trend) would indicate that
#' the given classification unit contains on average, the lowest values of that
#' variable. Thus, an ECDF and an iECDF can be selected for the peak and the
#' pit, respectively. The PDF can be selected for classification units whose
#' trend does not show either a peak or a pit. Please consider that peaks and
#' pits are only reference points and thus, one should validate the selection of
#' distribution functions based on domain knowledge.
#'
#' When \emph{mode = "auto"}, the criteria for the selection of distribution
#' functions will be based on peaks and pits in the parallel coordinates plot.
#'
#' The output table (\strong{distfun}) is intended to be used as input in the
#' \code{\link{predict_functions}} function.
#'
#' The selection of distribution functions is similar to the selection of
#' membership functions in fuzzy logic. For example, if one wants to describe a
#' phenomenon through distribution functions of continuous variables, then the
#' functions can be considered to be membership curves. Accordingly, the PDF,
#' ECDF, and iECDF will be equivalent to the Gaussian, S, and Z membership
#' functions, respectively.
#'
#' @examples
#' require(terra)
#' p <- system.file("exdat", package = "rassta")
#' # Multi-layer SpatRaster of topographic variables
#' ## 3 topographic variables
#' tf <- list.files(path = p, pattern = "^height|^slope|^wetness",
#'                  full.names = TRUE
#'                 )
#' tvars <- terra::rast(tf)
#' # Single-layer SpatRaster of topographic classification units
#' ## 5 classification units
#' tcf <- list.files(path = p, pattern = "topography.tif", full.names = TRUE)
#' tcu <- terra::rast(tcf)
#' # Automatic selection of distribution functions
#' tdif <- select_functions(cu.rast = tcu, var.rast = tvars, fun = mean)
#' # Parallel coordinates plot
#' if(interactive()){tdif$parcoord}
#'
#' @export
#' @family
#' Landscape Correspondence Metrics
#' @rdname
#' select_functions
#'
select_functions <- function(cu.rast, var.rast, fun = mean,
                             varscale = "uniminmax", mode = "auto",
                             verbose = TRUE, ...)
{

  #-----Binding variables to prevent devtools::check() notes-----#
  i <- Variable <- Class.Unit <- NULL
  #--------------------------------------------------------------#

  # Raster-based constrained statistics per classification unit
  cu.stat <- terra::zonal(var.rast, cu.rast, fun = fun)

  # Data table with constrained statistics
  cu.stat <- data.table::data.table(cu.stat)

  # Classification units as factors
  cu.stat[[1]] <- base::as.factor(cu.stat[[1]])
  base::colnames(cu.stat)[1] <- "CU"

  # Parallel coordinates plot
  .data <- rlang::.data
  pcp <- GGally::ggparcoord(cu.stat,
                            columns = 2:base::ncol(cu.stat),
                            groupColumn = 1,
                            scale = varscale,
                            showPoints = TRUE,
                            alphaLines = 1,
                            mapping = ggplot2::aes(colour = .data[["CU"]]),
                            ...
                          ) + ggplot2::labs(colour = "CU")

  # Construct table for class-constrained, univariate distribution functions
  ## Get names of continuous variables
  vars <- base::colnames(cu.stat)[2:base::ncol(cu.stat)]
  ## First version
  ### Expansion of rows based on variables * classification units
  distfunc <- data.table::as.data.table(base::expand.grid(cu.stat[[1]],
                                                          vars
                                                        )
                                      )
  ## Second version
  ### Sorted by classification units
  distfunc <- dplyr::arrange(distfunc, distfunc[[1]])
  ## Third version
  ### Column for distribution functions
  dif <- base::rep(NA, length = base::nrow(distfunc))
  distfunc <- base::cbind(distfunc, dif)
  ## Final version
  ### Renamed columns
  data.table::setnames(distfunc, c("Class.Unit", "Variable", "Dist.Func"))

  if (mode == "inter") {

    # Interactive plot of parallel coordinates
    parcoord <- plotly::ggplotly(pcp)

    # Interactive table of unit-wise statistics
    ## Set UI
    ui <- shiny::fluidPage(
      shiny::titlePanel("Constrained Univariate Distribution Functions"),
      shiny::mainPanel(
        shiny::tabsetPanel(
          id = 'dataset',
          shiny::tabPanel(
            "Variables and Distribution Functions per Classification Unit",
            # Graphic elements
            DT::dataTableOutput("distfunc"),
            shiny::br(),
            shiny::actionButton("viewBtn","View"),
            shiny::br(),
            shiny::actionButton("saveBtn","Save"),
            shiny:: br(),
            DT::dataTableOutput("distfunc_updated")
          )
        )
      )
    )

    ## Define server function
    server <- function(input, output) {
      # Table
      output$distfunc <- DT::renderDataTable(
        DT::datatable(distfunc,
                      editable = TRUE,
                      options = base::list(paging = FALSE)
                    )
      )

      # 'Interactiveness'
      ## Editing cells
      shiny::observeEvent(
        input$distfunc_cell_edit,
        {
          distfunc[input$distfunc_cell_edit$row,
              input$distfunc_cell_edit$col] <<- input$distfunc_cell_edit$value
        }
      )

      ## Pressing buttons
      ### 'View' button
      view_fun <- shiny::eventReactive(
        input$viewBtn,
        {
          if(base::is.null(input$saveBtn)||input$saveBtn == 0) {
            base::returnValue()
          } else {
            DT::datatable(distfunc, selection = 'none')
          }
        }
      )
      ### 'Save' button
      shiny::observeEvent(
        input$saveBtn,
        {
          utils::write.csv(distfunc, "cu_distfun.csv", row.names = FALSE)
        }
      )
      ### Update after 'view'
      output$distfunc_updated <- DT::renderDataTable({view_fun()})
    }

    ## Shiny app
    distfun <- shiny::shinyApp(ui = ui, server = server)

    # Return objects (parallel coordinates plot & Shiny App)
    base::list(parcoord = parcoord, distfun = distfun)

  } else if (mode == "auto") {

    # Interactive plot of parallel coordinates
    parcoord <- plotly::ggplotly(pcp)

    # Loop through variables to set distribution functions...
    # ...per classification unit
    `%do%` <- foreach::`%do%`
    `%>%` <- dplyr::`%>%`
    distlist <- foreach::foreach(i = 2:base::ncol(cu.stat)) %do% {

      # Get name of current variable
      var <- base::colnames(cu.stat)[i]

      # Get ID of classification unit for which the statistic is minimized
      min_cu <- base::order(cu.stat[[i]])[1]

      # Get ID of classification unit for which the statistic is maximized
      max_cu <- base::order(cu.stat[[i]], decreasing = TRUE)[1]

      # Inverse ECDF for classification unit that minimizes the statistic
      a <- distfunc %>%
        dplyr::filter(Variable == var) %>%
          dplyr::filter(Class.Unit == min_cu)
      a$Dist.Func <- 'iECDF'

      # ECDF for classification unit that maximizes the statistic
      b <- distfunc %>%
        dplyr::filter(Variable == var) %>%
          dplyr::filter(Class.Unit == max_cu)
      b$Dist.Func <- 'ECDF'

      # PDF for the rest of classification units
      if (nrow(cu.stat) > 2) {

        # Select PDF
        c <- distfunc %>%
          dplyr::filter(Variable == var) %>%
            dplyr::filter(Class.Unit != min_cu & Class.Unit != max_cu)
        c$Dist.Func[] <- 'PDF'

        # Build variable table
        d <- base::rbind(a,b,c)
        d <- dplyr::arrange(d, d[[1]])

      } else {

        # Build variable table
        d <- base::rbind(a,b)
        d <- dplyr::arrange(d, d[[1]])

      }

    }

    # Build final table (all variables, all classification units)
    distfun <- base::Reduce(function(...) base::merge(...,
                                                  by = colnames(distlist[[1]]),
                                                  all = T
                                                ),
                          distlist
                        )

    # Return objects (parallel coordinates plot & final table)
    base::list(parcoord = parcoord, distfun = distfun)

  } else {

    if(verbose == TRUE){
      base::warning("Nothing was done. Please select a valid selection mode.")
    }

  }

}
