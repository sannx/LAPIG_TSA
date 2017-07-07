####################################################################################
####################################################################################

SeasonalMetricsGRAPH <- function(x) {
library(mFilter) 
  DTMIN = NULL # data do valor minimo
  VLMIN = NULL # valor minimo
  DTMAX = NULL # data do valor maximo
  VLMAX = NULL # valor maximo
  MEAPIX = NULL #Media da serie temporal
  
pix <- as.numeric(x)
#Filtar serie
TS.or = ts(pix, start = 2001, frequency = 23)
TS.hp.trend = mFilter(TS.or,filter = "HP", freq = 1)$trend  # Hodrick-Prescott filter

###
###
  NCycmx = cbind(c(2001:2016), rep(20,16)) # começar no mês 10[19] ou 4[7] | ((23/12)*10)
  NCycmn = cbind(c(2001:2016), rep(10,16)) # começar no mês 10[19] ou 4[7] | ((23/12)*10)
###
###
MEAPIX = mean(TS.or)

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

plot(TS.hp.trend, t = 'l', ylim = c(0,1), width = 12, heigth = 6)
MEANALL = mean(na.omit(TS.hp.trend))
abline(h = MEANALL, col = 'black', lwd = 1, lty = 2)
points(DTMIN, VLMIN, lwd = 2, pch = 15, col = 'brown')
points(DTMAX, VLMAX, lwd = 2, pch = 15, col = 'darkgreen')
abline(h = mean(VLMAX), col = 'darkgreen', lwd = 1, lty = 2)
abline(h = mean(VLMIN), col = 'red', lwd = 1, lty = 2)

return(c(DTMIN, VLMIN, DTMAX, VLMAX, MEAPIX))
} 
####################################################################################
####################################################################################

SeasonalMetrics <- function(x) {
 library(mFilter)
  DTMIN = NULL # data do valor minimo
  VLMIN = NULL # valor minimo
  DTMAX = NULL # data do valor maximo
  VLMAX = NULL # valor maximo
  MEAPIX = NULL #Media da serie temporal
  
pix <- as.numeric(x)
#Filtar serie
TS.or <- ts(pix, start = 2001, frequency = 23)
TS.hp.trend <- mFilter(TS.or,filter = "HP", freq = 1)$trend  # Hodrick-Prescott filter

###
###
  NCycmx = cbind(c(2001:2016), rep(20,16)) # começar no mês 10[19] ou 4[7] | ((23/12)*10)
  NCycmn = cbind(c(2001:2016), rep(10,16)) # começar no mês 10[19] ou 4[7] | ((23/12)*10)
###
###
MEAPIX = mean(TS.or)

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

return(c(DTMIN, VLMIN, DTMAX, VLMAX, MEAPIX))
} 
####################################################################################
####################################################################################