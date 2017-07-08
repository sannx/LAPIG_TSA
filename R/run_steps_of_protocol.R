#!/usr/bin/Rscript
####################################################################################
####################################################################################
#Author: Oliveira-Santos, claudinei
#Instituto de Pesquisa Ambiental da Amaz?nia (IPAM)
#Abril 18, 2013
#
#'Passos para classificar pixels de pastagens
#'1 - remover outliers
#'2 - suavizar a serie temporal
#'3 - segmentar a serie temporal em anos
#'4 - clusterizar a serie temporal
#'5 - modelar a serie de cada cluster
#'6 - classificar a series de validacao
#'7 - analisar eficacia
#
####################################################################################
####################################################################################
options(scipen = 999)
#Pacotes e funcoes
library(raster)
library(forecast)
library(parallel)
library(mFilter)
library(ggplot2)
source("functions/MaxMinFilter.r")
source("functions/ExtractSeasonalMetrics.r")
source("functions/ZonalParallel.r")
source("functions/DateFromNames.r")

rasterOptions(tmpdir = "E:\\DATASAN\\temp")
###
###
#ndvi campo lapig embrapa
ndvi = read.csv("../data/NDVI_Campo_LapEmb_2017_pixmodis.csv")

###
###
#Original time serie
pix <- as.numeric(ndvi[1,29:396])

###
###
#Filter with MaxMinFilter to remove outliers
pix_mm <- MaxMinFilter(pix, 3)

  plot(pix, t = 'l', col = 'black')
  lines(pix_mm, t = 'l', col = 'red')

###
###
 #Smooth time serie to find min and max values.
TS.or = ts(pix, start = 2001, frequency = 23)
  TS.hp.trend = mFilter(TS.or,filter = "HP", freq = 1)$trend  # Hodrick-Prescott filter
  
  plot(TS.or, t = 'b', col = 'black')
  lines(TS.hp.trend, t = 'l', col = 'red', lwd = 2)

###
###
#Extract seasonal metrics from time serie
SeasonalMetricsGRAPH(pix)  

###
###
#split time serie by cycle
plot(TS.hp.trend, t = 'b', col = 'black')
for (i in 0:15) {
  TSWd <- window(TS.hp.trend, (start(TS.hp.trend)[1] + i), c(start(TS.hp.trend)[1] + i, frequency(TS.hp.trend)))
  lines(TSWd, col = 'red', lwd = 3)
}

####################################################################################
####################################################################################

#Apply filter Max min in matrix
  ST = Sys.time()
  ndvi_mm = t(apply(ndvi[,374:396], 1, MaxMinFilter, n = 2))
  Sys.time() - ST

#Smooth time serie to find min and max values.
  TS.or = ts(pix, start = 2016, frequency = 23)
  TS.hp.trend = mFilter(TS.or,filter = "HP", freq = 1)$trend  # Hodrick-Prescott filter

#Cluster
  
  
  hc <- hclust(dist(ndvi_mm), "ave")
  plot(hc, hang = -1)

  cl <- kmeans(ndvi_mm, 4)  
  plot(x, col = cl$cluster)
  points(cl$centers, col = 1:2, pch = 8, cex = 2)
 
  (cl <- kmeans(ndvi_mm, 5, nstart = 25))
  plot(x, col = cl$cluster)
  points(cl$centers, col = 1:5, pch = 8)
   
####################################################################################
####################################################################################