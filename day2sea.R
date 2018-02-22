
#' aggregates daily dates sequence to custom seasons using months of 
#' previous and current years

#' @param x daily dates sequence of "character" class. '%Y-%m-%d'
#' @param prev_year_mons indices of the months of the previous year (from 1 to 12) that will be included in the season
#' @param prev_year_mons indices of the months of the current year (from 1 to 12) that will be included in the season

day2sea <- function(x, prev_year_mons, curr_year_mons){ 
    years <- unique(substr(x, 1, 4))
    tmp <- list()
    for(i in 1:(length(years)-1)) tmp[i] <- paste(years[i], years[i+1], sep='-')
    tmp <- unlist(tmp)
    
    ff <- function(d, tmp, prev_year_mons, curr_year_mons){
      res <- NA
      y <- as.numeric(substr(d, 1, 4))
      m <- as.numeric(substr(d, 6, 7))
      wy1 <- which(y == as.numeric(substr(tmp,1 ,4)))
      wm1 <- which(m == prev_year_mons)
      if(sum(wy1, wm1) == 2) res <- tmp[wy1] else{
        wy2 <- which(y == as.numeric(substr(tmp,6 ,9)))
        wm2 <- which(m == curr_year_mons)
        if(sum(wy2, wm2) == 2) res <- tmp[wy2]
      }
      res
    }
    
    ss <- unlist(sapply(x, FUN = ff, tmp, prev_year_mons, curr_year_mons))
    ss
  }

#example:
# x <- as.character(seq.Date(as.Date('1981-01-01'), as.Date('2017-12-31'), by ='day'))
#  prev_year_mons <- 10:12
#  curr_year_mons <- 1:5
#   day2sea(x, prev_year_mons, curr_year_mons)  
