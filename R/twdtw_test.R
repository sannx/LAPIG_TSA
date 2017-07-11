devtools::install_github('vwmaus/dtwSat')

library(dtwSat)
# Create and plot object time series 
ts <- twdtwTimeSeries(MOD13Q1.ts)
class(ts)
plot(ts, type = "timeseries")


#MOD13Q1.ts[,1:2]

ts <- twdtwTimeSeries(MOD13Q1.ts[,1:2])
class(ts)
plot(ts, type = "timeseries")


# Create and plot object time series 
patt <- twdtwTimeSeries(MOD13Q1.patterns.list)
class(patt)
plot(patt, type = "patterns") 


# MOD13Q1.patterns.list[[1]][,1:2]
# MODclass = list()
# for(i in 1:3){
#   MODclass[[i]] <- MOD13Q1.patterns.list[[i]][,1:2]
# }
# Create and plot object time series 
patt <- twdtwTimeSeries(MODclass)
class(patt)
plot(patt, type = "patterns") 



# Define logistic time-weight, see Maus et al. (2016)
log_fun <- logisticWeight(alpha = -0.1, beta = 100) 
# Run TWDTW analysis 
matches <- twdtwApply(x = ts, y = patt, weight.fun = log_fun, keep = TRUE) 

plot(x = matches, type = "alignments")


plot(x = matches, type = "matches", attr = "evi", patterns.labels = "Soybean", k <- 4) 

plot(x = matches, type = "paths", k <- 1:4) 

plot(x = matches, type = "classification",
     from = "2009-09-01", to = "2013-09-01", 
     by = "6 month", overlap = 0.5)


evi  <- brick(system.file("lucc_MT/data/evi.tif",  package = "dtwSat"))
ndvi <- brick(system.file("lucc_MT/data/ndvi.tif", package = "dtwSat"))
red  <- brick(system.file("lucc_MT/data/red.tif",  package = "dtwSat"))
blue <- brick(system.file("lucc_MT/data/blue.tif", package = "dtwSat"))
nir  <- brick(system.file("lucc_MT/data/nir.tif",  package = "dtwSat"))
mir  <- brick(system.file("lucc_MT/data/mir.tif",  package = "dtwSat"))
doy  <- brick(system.file("lucc_MT/data/doy.tif",  package = "dtwSat"))


timeline <- scan(system.file("lucc_MT/data/timeline", package = "dtwSat"), what = "date")



rts <- twdtwRaster(evi, ndvi, red, blue, nir, mir, timeline = timeline, doy = doy)



field_samples <- read.csv(system.file("lucc_MT/data/samples.csv", package = "dtwSat"))
proj_str <- scan(system.file("lucc_MT/data/samples_projection", package = "dtwSat"), what = "character")



library(caret)
set.seed(1)
I <- unlist(createDataPartition(field_samples$label, p = 0.1))
training_samples <- field_samples[I, ]
validation_samples <- field_samples[-I, ]



training_ts <- getTimeSeries(rts, y = training_samples, proj4string = proj_str)
validation_ts <- getTimeSeries(rts, y = validation_samples, proj4string = proj_str)


###
###
#'Os padrões serão criados aqui (SAN)
#'No exemplo do Vitor Maus, ele já sabia o que era cada dado.
temporal_patterns <- createPatterns(training_ts, freq = 8, formula = y ~ s(x))
plot(temporal_patterns, type = "patterns") 


# Define logistic time-weight, see Maus et al. (2016)
log_fun <- logisticWeight(-0.1, 50)

# Run serial TWDTW analysis 
# r_twdtw <- twdtwApply(x = rts, y = temporal_patterns, weight.fun = log_fun, progress = 'text')

# or Run parallel TWDTW analysis
beginCluster()
r_twdtw <- twdtwApplyParallel(x = rts, y = temporal_patterns, weight.fun = log_fun, progress = 'text')
endCluster()


r_lucc <- twdtwClassify(r_twdtw, progress = 'text')


plot(x = r_lucc, type = "maps")

plot(x = r_lucc, type = "area")

plot(x = r_lucc, type = "changes")


twdtw_assess <- twdtwAssess(object = r_lucc, y = validation_samples, 
                            proj4string = proj_str, conf.int = .95, rm.nosample = TRUE) 
show(twdtw_assess)


plot(twdtw_assess, type = "accuracy")

plot(twdtw_assess, type = "area")


################
################
################
library(raster)
r <- brick("F:\\DATASAN\\raster\\BHRV\\BHRV_CROP_pa_br_ndvi_ALL.tif")

shp <- shapefile("F:\\DATASAN\\shapefile\\bhrv_uso_e_cobertura\\BHRV_uso_cobertura_2016_lapig_dissolve.shp")
extr <- extract(r[[1]], shp[shp$Class_3 = '', ], cellnumbers = TRUE)





















