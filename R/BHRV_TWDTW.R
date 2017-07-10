####################################################################################
####################################################################################
#Author: Oliveira-Santos, claudinei
#
#Script para rodar twdtw para o SIAD
#
####################################################################################
####################################################################################

#Pacotes e funcoes
library(raster)
library(forecast)
library(bfast)
library(caret)
library(dtwSat)
library(mFilter)
rasterOptions(tmpdir = "E:\\DATASAN\\temp")
source("functions/MaxMinFilter.r")
source("functions/DateFromNames.r")
###
###
#Arquivos
#Declividade
# pmask = raster('F:\\DATASAN\\raster\\resampled_mask_past_v07_modis_5m.tif')
# 
# r <- brick("F:\\DATASAN\\raster\\BHRV\\BHRV_CROP_pa_br_ndvi_ALL.tif")
# 
# pndv <- crop(pmask, r)
# pndv[is.na(pndv)] <- 0
# 
# #ShapeFile
# rdf <- as.data.frame(pndv, xy = TRUE)
# names(rdf)[3] <- "label"
# rdf$CellNumbers <- cellFromXY(pndv, cbind(rdf$x, rdf$y))
# 
# CellSample <- sample(1:nrow(rdf), 1000)
# 
# samples <- rdf[CellSample,]
# names(samples) <- c("longitude", "latitude", "label", "CellNumbers")
# samples$from <- "2015-10-01"
# samples$to <- "2016-10-01"
# 
# samples$label <- ifelse(samples$label == 1, 'pastagem', "outro")
# write.csv(samples, file = "../data/samples_bhrv.csv", row.names = FALSE)
samples <- read.csv("../data/samples_bhrv.csv")

###
###
#NDVI Extract for All Period
# ST <- Sys.time()
#  ndvi <- r[samples$CellNumbers]
# Sys.time() - ST
# 
# save(ndvi, file = "../data/NDVI_MOD13Q1.rdata")
# write.csv(ndvi, file = "../data/NDVI_MOD13Q1.csv", row.names = FALSE)
ndvi <- read.csv("../data/NDVI_MOD13Q1.csv")

 ls.files <- dir("H:\\DATASAN\\raster\\NDVI\\BRASIL\\NDVI\\")
 timeline <- DateFromNames(ls.files, 16)
# write.table(timeline, file = "../data/timeline", row.names = FALSE, col.names = FALSE)

pix <- MaxMinFilter(na.interp(as.numeric(ndvi[2,])), 3)
# pix2 <- na.interp(as.numeric(ndvi[2,]))
# pixm <- as.matrix(cbind(pix, pix2))
pixm <- as.matrix(pix)
colnames(pixm) <- 'ndvi'

TSzoo <- zoo(pixm, timeline)
head(TSzoo)

ts <- twdtwTimeSeries(TSzoo)
plot(ts, type = 'timeseries')

####
####
field_samples <- samples

set.seed(1)
I <- unlist(createDataPartition(field_samples$label, p = 0.5))
training_samples <- field_samples[I, ]
validation_samples <- field_samples[-I, ]


###
###
rts <- twdtwRaster(r, timeline = timeline, doy = NULL)

proj_str <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

training_samples_nt <- training_samples[training_samples$label == "pastagem", ]
training_samples_dt <- training_samples[training_samples$label == "outro", ]


ST <- Sys.time()
training_ts <- getTimeSeries(rts, y = training_samples, proj4string = proj_str)
# training_ts_nt <- getTimeSeries(rts, y = training_samples_nt, proj4string = proj_str)
# training_ts_dt <- getTimeSeries(rts, y = training_samples_dt, proj4string = proj_str)
validation_ts <- getTimeSeries(rts, y = validation_samples, proj4string = proj_str)
Sys.time() - ST

###
###
temporal_patterns <- createPatterns(training_ts, freq = 8, formula = y ~ s(x))
plot(temporal_patterns, type = "patterns") 
#temporal_patternst@timeseries

# temporal_patterns_nt <- createPatterns(training_ts_nt, freq = 8, formula = y ~ s(x))
# plot(temporal_patterns_nt, type = "patterns") 
# temporal_patterns_dt <- createPatterns(training_ts_dt, freq = 8, formula = y ~ s(x))
# plot(temporal_patterns_dt, type = "patterns") 


###
###
# Define logistic time-weight, see Maus et al. (2016)
log_fun <- logisticWeight(-0.1, 50)

# or Run parallel TWDTW analysis
beginCluster()
r_twdtw <- twdtwApplyParallel(x = rts, y = temporal_patterns, weight.fun = log_fun, progress = 'text')
endCluster()

###
###
# Define logistic time-weight, see Maus et al. (2016)
log_fun <- logisticWeight(alpha = -0.1, beta = 100) 
# Run TWDTW analysis 
matches <- twdtwApply(x = ts, y = temporal_patterns, weight.fun = log_fun, keep = TRUE) 
class(matches)
# plot(x = matches, type = "alignments")
# plot(x = matches, type = "paths", k <- 1:4) 

plot(x = matches, type = "classification",
     from = "2001-01-01", to = "2017-01-01", 
     by = "6 month", overlap = 0.5) 

####################################################################################
####################################################################################


pix <- MaxMinFilter(na.interp(as.numeric(ndvi[50,])), 3)
TS.or = ts(pix, start = 2001, frequency = 23)
TS.hp.trend = as.numeric(mFilter(TS.or,filter = "HP", freq = 1)$trend)  # Hodrick-Prescott filter

pixm <- as.matrix(TS.hp.trend)
colnames(pixm) <- 'ndvi'
TSzoo <- zoo(pixm, timeline)
ts <- twdtwTimeSeries(TSzoo)
plot(ts, type = 'timeseries')
matches <- twdtwApply(x = ts, y = temporal_patterns, weight.fun = log_fun, keep = TRUE) 
plot(x = matches, type = "classification",
     from = "2001-01-01", to = "2017-01-01", 
     by = "6 month", overlap = 0.5) 

