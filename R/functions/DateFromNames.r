DateFromNames <- function (x, ST) { 

ST = ST

ano = substr(ls.files , 16,19)
juliano = as.numeric(substr(ls.files , 20,22))
DT = as.Date(juliano, origin = as.Date(paste(ano, 01, 01, sep = "-")))
DTm = floor_date(DT, 'month')

}