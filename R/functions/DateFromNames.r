DateFromNames <- function (x, START) { 
  
  STARTy = START
  ENDy = START + 3
  STARTj = START + 4
  ENDj = START + 6
  
  ano = substr(x , STARTy, ENDy)
  juliano = as.numeric(substr(x , STARTj, ENDj))
  DT = as.Date(juliano, origin = as.Date(paste(ano, 01, 01, sep = "-")))
  return(DT)
}