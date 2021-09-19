
# Create SpatRasters of continuous variables following a single Brownian motion
set.seed(963)
x <- rnorm(n = 99, sd = sqrt(0.01))
x <- c(1, cumsum(x))
v1 <- terra::rast(ncol = 10, nrow = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
v1[] <- 0 # To suppress warning
terra::values(v1) <- x
v2 <- terra::flip(terra::flip(v1), direction = "horizontal")
vars <- c(v1, v2)
names(vars) <- c("v1", "v2")

# Dimensionality reduction and clustering with estimation of optimum k
set.seed(963)
vs <- somgap(var.rast = vars, xdim = 5, ydim = 5, K.max = 3, topo = "hexagonal",
             neighbourhood.fct = "gaussian", rlen = 600, spaceH0 = "original"
            )
vc <- sompam(ref.rast = vars[[1]], kohsom = vs$SOM, k = vs$Kopt)

# Manual dimensionality reduction and clustering with estimation of optimum k
mm <- terra::as.matrix(vars)
mg = kohonen::somgrid(xdim = 5, ydim = 5, topo = "hexagonal",
                      neighbourhood.fct = "gaussian",
                    )
set.seed(963)
ms = kohonen::supersom(data = mm, grid = mg, rlen = 600, mode = "pbatch")
msc <- kohonen::getCodes(ms)
mc <- cluster::pam(x = msc, k = vs$Kopt, diss = FALSE, metric = "manhattan",
                   cluster.only = TRUE, do.swap = TRUE
                  )
mr <- mc[ms$unit.classif]

# Test equality for clustering solutions
expect_equal(as.vector(mr), as.vector(terra::values(vc$sompam.rast[[2]])))
