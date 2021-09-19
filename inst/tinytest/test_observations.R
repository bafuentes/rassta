
# Create SpatRaster of stratification units (2 blocks of categorical values)
su <- terra::rast(ncol = 4, nrow = 4, xmin=0, xmax=1, ymin=0, ymax=1)
su[] <- 0 # To suppress warning
terra::values(su)[1:8] <- 1
terra::values(su)[9:16] <- 2

# Create SpatRasters of landscape similarity to stratification units
su_1 <- terra::rast(ncol = 4, nrow = 4, xmin=0, xmax=1, ymin=0, ymax=1)
su_1[] <- 0
su_2 <- su_1
set.seed(963)
terra::values(su_1)[1:8] <- rnorm(8, 85, 5)
terra::values(su_1)[9:16] <- rnorm(8, 15, 5)
terra::values(su_2)[1:8] <- rnorm(8, 15, 5)
terra::values(su_2)[9:16] <- rnorm(8, 85, 5)
ls <- c(su_1, su_2)
names(ls) <- c("su_1", "su_2")

# Create vector of observations and edit attributes
obs <- terra::as.points(su)
names(obs) <- "ID"                     # Numeric ID for observations
obs$ID <- seq(1:length(obs$ID))
obs$var <- rnorm(length(obs$ID), 6, 3) # Dummy variable

# Select representative observation for stratification units
su.obs <- observation(su.rast = su, obs = obs, col.id = 1, col.resp = 2,
                      method = "mls", ls.rast = ls
                    )

# Test equality of similarity values at locations of selected observations
expect_equal(max(terra::values(ls[[1]])), su.obs$su_repobs$land_sim[1])
expect_equal(max(terra::values(ls[[2]])), su.obs$su_repobs$land_sim[2])
