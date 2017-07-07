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

####################################################################################
####################################################################################

y = as.numeric(ndvi[1,])


YearlySegment <- function(y, n)
{
  yf = y
  sz = length(y)
  n = 23
  TS = ts(yf[9:400], start = 2000.49, frequency = 23)
  
  window(TS, start())
  
         
  cycle(TS)
  
  2000.49:2017.49
  
         split(TS, cycle(TS))
  
         lapply(split(TS, cycle(TS)), mean)
         
  for (i in 1:sz) {
    
    istart <- max(1, i - n) #Nice(san)!!
    iend	 <- min( sz, i + n)
    id     <- (istart:iend)
    dt     <- y[id]
    
    mm	<- min( y[id] );
    MM	<- max( y[id] );
    
    if (y[i] == mm || y[i] == MM) {
      yf[i] <- (sum( y[id] ) - y[i])/(length(id) - 1);
    } else {
      yf[i]
    }
  }
  return(yf)
}



library(raster)
library(doSNOW)

#create list containing test rasters

x <- raster(ncol = 10980, nrow = 10900) 
x <- setValues(x,1:ncell(x)) 

list.x <- replicate( 9 , x )
list.x2 = stack(list.x)
#setting up cluster

NumberOfCluster <- 8
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)
junk <- clusterEvalQ(cl,library(raster))

#perform calculations on each raster

list.x <- parLapply(cl,list.x,function(x) calc(x,function(x) { x * 10 }))

#stop cluster

stopCluster(cl)


ff <- parSapply(cl, list.x2, function(x) { 
  calc(x, sum)
  f
})

s <- stack(ff)


f2 <- function(x) {
    time = 1:length(x)
    bptest(lm(x ~ time))$p.value
  }


clusterR(cl)