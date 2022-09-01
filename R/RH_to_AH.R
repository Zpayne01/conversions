#' AHtoRH
#'
#' This function converts RHtoAH given a specfic temperature, pressure and RH
#'
#' @param RH #Numeric containing percentage Relative Humidity
#' @param temp #numeric containing temperature in Celcius
#' @return A numeric value in g/m3
#' @export
#'
RH_to_AH <- function(RH, temp = 25) {
  #RH as a percentage
  #Temperature in Celcius

  PWS <- 6.116441 * 10^(7.591386*temp/(temp + 240.7263))
  PW <-(RH)/100*PWS*100
  AH<- (2.16679*PW)/(temp+273.15)

  return(AH)

}
