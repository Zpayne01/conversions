#' Percc to Mixing Ratio
#' 
#' This function can have a mixing ratio input i.e. ppb, and output a concentration that in
#' per cubic centimeter. All concentrations should be stated. All temperatures should be in K
#' all pressures in atm. 
#' 
#' @param conc numeric containing concentration value
#' @param temp tmeperature in K
#' @param pressure Pressure in atm
#' @param mixingratio Mixing ratio units i.e. ppth, ppm, ppb, ppt, ppq
#' @return A numeric value in molecules cm-3
#' @export
percc_to_mixingratio <- function(conc, temp = 298, pressure = 1, mixingratio = 'ppb') {
  #Concentration in ppb
  #Temperature in kelvin
  #pressure in atm
  if(temp < 250) {
    print('Is your temperture in K?')
  }
  factor <- switch(
    mixingratio,
    'ppth' = 1e4,
    'ppm' = 1e6,
    'ppb' = 1e9,
    'ppt' = 1e12,
    'ppq' = 1e15
  )
  output <- conc*factor/6.022E23*0.08205/pressure*temp*1000
  return(output)
}

