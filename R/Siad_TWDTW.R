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
rasterOptions(tmpdir = "E:\\DATASAN\\temp")
source("functions/MaxMinFilter.r")
source("functions/DateFromNames.r")
###
###
#Arquivos
# r <- raster("H:\\DATASAN\\raster\\NDVI\\BRASIL\\NDVI\\pa_br_ndvi_250_2016353_lapig.tif")
# crs(r)
r = brick("E:\\pa_br_mod13q1_ndvi_250_2000_2017.tif")

#ShapeFile
#shp <- shapefile("S:\\PORTAL\\Desmatamento\\bi_ce_alertas_desmatamento_500_lapig\\bi_ce_alertas_desmatamento_500_2015_lapig.shp")

# CellNumbers <- extract(r, shp, cellnumbers = TRUE, df = TRUE)
# CellNumbers <- CellNumbers[, -3]

#write.csv(CellNumbers, file = "../data/CellNumbers_SIAD_2015.csv")
CellNumbers <- read.csv('../data/CellNumbers_SIAD_2015.csv')

#CellSample <- sample(CellNumbers$cell, 100)

# samples <- as.data.frame(xyFromCell(r, CellSample))
# names(samples) <- c("longitude", "latitude")
# samples$from <- "2015-01-01"
# samples$to <- "2016-01-01"
# samples$label <- "Desmatamento"
# 
# samples_nativa <- samples
# samples_nativa$from <- "2009-01-01"
# samples_nativa$to <- "2011-01-01"
# samples_nativa$label <- "VegNativa"
# 
# samples <- rbind(samples, samples_nativa)

#write.csv(samples, file = "../data/samples.csv")
samples <- read.csv("../data/samples.csv")

###
###
#NDVI Extract for All Period

# ST <- Sys.time()
#  ndvi <- r[CellSample]
# Sys.time() - ST
# 
# save(ndvi, file = "../data/NDVI_MOD13Q1.rdata")
# write.csv(ndvi, file = "../data/NDVI_MOD13Q1.csv")
ndvi <- read.csv("../data/NDVI_MOD13Q1.csv")[,-1]

# ls.files <- dir("H:\\DATASAN\\raster\\NDVI\\BRASIL\\NDVI\\")
# timeline <- DateFromNames(ls.files, 16)
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

training_samples_nt <- training_samples[training_samples$label == "VegNativa", ]
training_samples_dt <- training_samples[training_samples$label == "Desmatamento", ]


ST <- Sys.time()
training_ts <- getTimeSeries(rts, y = training_samples, proj4string = proj_str)
# training_ts_nt <- getTimeSeries(rts, y = training_samples_nt, proj4string = proj_str)
# training_ts_dt <- getTimeSeries(rts, y = training_samples_dt, proj4string = proj_str)
validation_ts <- getTimeSeries(rts, y = validation_samples, proj4string = proj_str)
Sys.time() - ST

###
###
temporal_patternst <- createPatterns(training_ts, freq = 8, formula = y ~ s(x))
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
pixm <- as.matrix(pix)
colnames(pixm) <- 'ndvi'
TSzoo <- zoo(pixm, timeline)
ts <- twdtwTimeSeries(TSzoo)
plot(ts, type = 'timeseries')
matches <- twdtwApply(x = ts, y = temporal_patterns, weight.fun = log_fun, keep = TRUE) 
plot(x = matches, type = "classification",
     from = "2001-01-01", to = "2017-01-01", 
     by = "6 month", overlap = 0.5) 

