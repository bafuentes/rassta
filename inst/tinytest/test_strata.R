
# Create SpatRasters of classification units
cu1 <- terra::rast(ncol = 8, nrow = 8, xmin=0, xmax=1, ymin=0, ymax=1)
cu1[] <- 0 # To suppress warning
cu3 <- cu2 <- cu1
terra::values(cu1)[1:32] <- 1
terra::values(cu1)[33:64] <- 2
terra::values(cu2)[1:16] <- 1
terra::values(cu2)[17:64] <- 2
terra::values(cu3)[1:48] <- 1
terra::values(cu3)[49:64] <- 2
cu4 <- terra::trans(cu1)
cus <- (c(cu1, cu2, cu3, cu4))
names(cus) <- c("cu1", "cu2", "cu3", "cu4")

# Create stratification units
su <- strata(cus)

# Manual creation of stratification units
mcus <- terra::as.matrix(cus)
msu <- (mcus[,1]*1000)+(mcus[,2]*100)+(mcus[,3]*10)+(mcus[,4])

# Test equality for clustering solutions
expect_equal(as.vector(msu), as.vector(terra::values(su$su.rast)))
