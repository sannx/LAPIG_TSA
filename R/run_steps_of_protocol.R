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
pix <- as.numeric(ndvi[1,9:400])

#Filter with MaxMinFilter to remove outliers
pix_mm <- MaxMinFilter(pix, 3)

  plot(pix, t = 'l', col = 'black')
  lines(pix_mm, t = 'l', col = 'red')

#Smooth time serie to find min and max values.
TS.or = ts(pix, start = 2001, frequency = 23)
  TS.hp.trend = mFilter(TS.or,filter = "HP", freq = 1)$trend  # Hodrick-Prescott filter
  
  plot(TS.or, t = 'l', col = 'black')
  lines(TS.hp.trend, t = 'l', col = 'red', lwd = 2)
  ##Figura por parcela
  ggplot(TS.or, aes(y = as.numeric(TS.or), x =time(TS.or))) +
    geom_point(size = 1)+
    geom_line(size=.4, color = "blue")+
    labs(list(x = "Data", y = "NDVI",size=3, title = "Pixel NDVI"))+
    theme_bw(base_size = 10)

  
    
NCycmx = cbind(c(2001:2016), rep(20,16)) # começar no mês 10[19] ou 4[7] | ((23/12)*10)
NCycmn = cbind(c(2001:2016), rep(10,16)) # começar no mês 10[19] ou 4[7] | ((23/12)*10)

  MEAPIX = mean(TS.or)
 
  c(end(TS.hp.trend) - start(TS.hp.trend))[1]
  
  2000.49:2017.49
  
  window(TS.hp.trend, start(TS.hp.trend), start(TS.hp.trend))
  
  lapply(split(TS, cycle(TS)), mean)
  
   
lapply()
  
  for (i in 1:nrow(NCycmx) )
  {
    # cat('loop', 'CICLO', i, '\n')
    TSYmn = window(TS.hp.trend, NCycmn[i,], c(NCycmn[i,][1], (NCycmn[i,][2] + 10) ))
    DTmn = time(TSYmn)
    VLMIN[i] = min(na.omit(TSYmn))
    DTMIN[i] = DTmn[ which(TSYmn %in% VLMIN[i]) ]
    
    
    TSYmx = window(TS.hp.trend, NCycmx[i,], c(NCycmx[i,][1], (NCycmx[i,][2] + 13) ))
    DTmx = time(TSYmx)
    VLMAX[i] = max(na.omit(TSYmx))
    DTMAX[i] = DTmx[ which(TSYmx %in% VLMAX[i]) ]
  }
  

ls.f = list.files(path = "H:\\DATASAN\\NDVI_RO")

DateFromNames(ls.f, 24)

substr(ls.f[1], 24,30)


####################################################################################
####################################################################################



####################################################################################
####################################################################################