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
library(mFilter)
source("functions/MaxMinFilter.r")
source("functions/ExtractSeasonalMetrics.r")
source("functions/ZonalParallel.r")
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
  







####################################################################################
####################################################################################

pix


df<-data.frame(xmin=as.Date(c("2008-10-01","2009-10-01","2010-10-01","2011-10-01","2012-10-01","2013-10-01","2014-10-01")),
               xmax=as.Date(c("2008-04-01","2009-04-01","2010-04-1","2011-04-1","2012-04-01","2013-04-01","2014-04-01")),
               ymin=c(-Inf,-Inf,-Inf,-Inf,Inf,Inf,Inf),
               ymax=c(Inf,Inf,Inf,Inf,Inf,Inf,Inf),
               years=c("2008s","2009s","2010s","2011s","2012s","2013s","2014s"))


##Figura por parcela
##
p.d=ggplot(smetd.r[smetd.r$Var=="windSpd" ,], aes(y = Value, x=DT, shape=Var,color=Var)) +
  #geom_point(size = 1)+
  geom_line(size=.4, color = "orange")+
  facet_grid(Var~., scales = "free_y")+
  labs(list(x = "Data", y = expression(paste("Valores"))),size=3)+
  scale_x_date(breaks = "1 year", minor_breaks = "1 month", labels=date_format("%Y%m%d")) +
  geom_rect(data=df,aes(xmin=xmin,xmax=xmax,ymin = -Inf, ymax = Inf),alpha=0.2,inherit.aes=FALSE)+
  scale_fill_manual(values=c("gray","gray","gray","gray","gray","gray","gray"))+
  theme_bw(base_size = 10)
#theme(legend.justification=c(1.1,-.8), legend.position=c(1.01,0.5), legend.title = element_text(colour="black", size=16, face="bold"))
#ggsave(p.d,file="H:\\Dropbox\\trabalho\\dados_by_colab\\Grupo_Maggi\\dados_di�rios.jpg", dpi = 300, width=10, height=6 )						
ggsave(p.d,file="H:\\Dropbox\\trabalho\\dados_by_colab\\Marcia_Macedo\\dados_diarios.jpg", dpi = 300, width=10, height=4 )						
