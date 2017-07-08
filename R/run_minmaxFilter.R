#!/usr/bin/Rscript
####################################################################################
####################################################################################
#Author: Oliveira-Santos, claudinei
#Instituto de Pesquisa Ambiental da Amaz?nia (IPAM)
#Abril 18, 2013
#
#Script para calcular indice de vegetcao iplementado pelo prof. Luis Baumann
#
####################################################################################
####################################################################################
options(scipen = 999)
#Pacotes e funcoes
library(raster)
library(forecast)
library(parallel)
library(animation)
source("functions/MaxMinFilter.r")
source("functions/ExtractSeasonalMetrics.r")
source("functions/ZonalParallel.r")
source("functions/DateFromNames.r")
rasterOptions(tmpdir = "E:\\DATASAN\\temp")
###
###
#ndvi campo lapig embrapa
ndvi = read.csv("../data/NDVI_Campo_LapEmb_2017_pixmodis.csv")

ndvi[,9:400]

###
###
pix <- as.numeric(ndvi[1,9:400])
pix_mm <- minmaxFilter(pix)

plot(pix, t = 'l', col = 'black')
lines(pix_mm, t = 'l', col = 'red')


ST = Sys.time()
ndvi_mm = t(apply(ndvi[,9:400], 1, minmaxFilter, n = 2))
Sys.time() - ST #0.002615826 by pixel

ST = Sys.time()
szmtt = apply(ndvi_mm, 1, SeasonalMetrics)
Sys.time() - ST #0.1859628 by pixel

cl <- makeCluster(detectCores() - 1)
ST = Sys.time()
szmtt = parApply(cl = cl, ndvi_mm, 1, SeasonalMetrics)
Sys.time() - ST #0.04440017 by pixel (4 x faster)
stopCluster(cl)


r = brick("F:\\DATASAN\\raster\\BHRV\\NDVI_BHRV_crop_Pasture.tif")
ndvi_mask = raster("F:\\DATASAN\\raster\\resampled_mask_past_v07_modis_5m.tif")
ndvi_mask = crop(ndvi_mask, r)

tst = zone_parallel(r, ndvi_mask, 1, minmaxFilter)

cl <- makeCluster(detectCores() - 1)
ST = Sys.time()
szmtt = parApply(cl = cl, ndvi_mm, 1, SeasonalMetricsGRAPH)
Sys.time() - ST #0.04440017 by pixel (4 x faster)
stopCluster(cl)




cl <- makeCluster(detectCores() - 1)
ST = Sys.time()
NDVI_Interp = t(parApply(cl = cl, ndvi[, 9:400], 1, na.interp))
Sys.time() - ST #0.04440017 by pixel (4 x faster)
stopCluster(cl)


ST = Sys.time()
saveGIF(apply(ndvi_mm, 1, SeasonalMetricsGRAPH), interval = .8)
Sys.time() - ST #0.1859628 by pixel

###
###
#Date From Names
ls.files <- dir("S:\\QUALIDADE_PASTAGEM\\NDVI_TSA\\1_BRASIL\\NDVI\\")
ls.files = Sys.glob('*ndvi*.tif')
DateFromNames(ls.files, 16)

####################################################################################
####################################################################################