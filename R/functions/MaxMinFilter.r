#' Filtro 'minmaxFilter' para spar remover outliers em vetores numericos
#' Quanto menor o valor de 'n' dados observados a direta e esquerda, maior a remocao de dados/outliers
MaxMinFilter <- function(y, n)
{
	yf = y
	sz = length(y)
	n = n
	
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
