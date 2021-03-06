#' NCEP NARR Wind 
#' 
#' These time series include mean surface wind speed and direction ("uwnd" and "vwnd" respectively), 
#' surface turbulent kinetic energy (TKE), storm relative helicity (HLCY), and total wind speed (TWS) 
#' in the Northeast Large Marine Ecosystem. 
#' 
#' @format Data set inclues 2385 rows and 5 columns
#' 
#' \itemize{
#'     \item Var: Specifies variable and season of interest.
#'     \item Value: Value of variable \code{Var}.
#'     \item Units: Units of variable \code{Var}.
#'     \item EPU: Ecological Production Unit (EPU) from which data were collected.
#'     \item Time: Year.
#' }
#' 
#' @details Indicator for total wind speed is calculated as TWS = sqrt(u^2 + v^2), where u = surface wind speed
#' and v = surface wind direction. Seasons are defined as: Fall = October, November, December;
#' Winter = January, February, March; Spring = April, May, June; Summer = July, August, September.
#' 
#' @source \url{https://www.esrl.noaa.gov/psd/data/gridded/data.narr.monolevel.html}
"ne_wind"