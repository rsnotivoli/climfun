
#' Aggregates daily data to seasonal. Based on "dm2seasonal" function from "hydroTSM" package.

#' @param x daily data (vector or matrix) 
#' @param dates sequence of "Date" class. '%Y-%m-%d'
#' @param season character vector indicating the grouping months (DJF : December, January, February; MAM : March, April, May; JJA : June, July, August; SON : September, October, November.
#' @param FUN function to be applied to daily data by seasons
#' @note This function is based on the "dm2seasonal" function from "hydroTSM" package by Mauricio Zambrano-Bigiarini.
#' 
#' 

dailyToSeasonal <- function(x, dates, season, FUN){
  
  if(season == 'DJF'){
    # selection of dates with winter months
    w <- which(substr(dates, 6,7) == '12' | substr(dates, 6,7) == '01' | substr(dates, 6,7) == '02')
    datessC <- dates[w]
    
    # removing of first months of first year to start in December
    w12 <- which(substr(datessC, 6, 7) == '12')[1]
    datessC <- datessC[-c(1:(w12-1))]
    
    # same with data
    pre <- x[w, ]
    pre <- pre[-c(1:(w12-1)), ]
    
    # replace the number of the year of December by the next year
    years <- substr(datessC, 1, 4)
    decindex <- which(substr(datessC, 6, 7) == '12')
    decyears <- years[decindex]
    decyears <- as.numeric(decyears) + 1
    years[decindex] <- decyears
  } else if(season == 'MAM'){
    w <- which(substr(dates, 6,7) == '03' | substr(dates, 6,7) == '04' | substr(dates, 6,7) == '05')
    pre <- x[w, ]
    datessC <- dates[w]
    years <- substr(datessC, 1, 4)
  } else if(season == 'JJA'){
    w <- which(substr(dates, 6,7) == '06' | substr(dates, 6,7) == '07' | substr(dates, 6,7) == '08')
    pre <- x[w, ]
    datessC <- dates[w]
    years <- substr(datessC, 1, 4)
  } else if(season == 'SON'){
    w <- which(substr(dates, 6,7) == '09' | substr(dates, 6,7) == '10' | substr(dates, 6,7) == '12')
    pre <- x[w, ]
    datessC <- dates[w]
    years <- substr(datessC, 1, 4)
  }
  
  # aggregation by function
  res <- aggregate(pre, by = list(years), FUN = FUN)
  return(res)
}
