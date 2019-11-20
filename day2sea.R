
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
  
  yy <- list()
  for(i in 1:(length(years)-1)){
    yy[[i]] <- c(paste(years[i],formatC(prev_year_mons, width = 2, flag = '0'), sep ='-'),
                 paste(years[i+1],formatC(curr_year_mons, width = 2, flag = '0'), sep='-'))
  }
  
  res <- numeric()
  for(i in 1:length(yy)){
    w <- sapply(yy[[i]], function(x, y){ 
      which(x == y)
      }, y = substr(x, 1,7))
    w <- as.numeric(unlist(w))
    res[w] <- tmp[i]
  }
    res
}

#example:
# x <- as.character(seq.Date(as.Date('1981-01-01'), as.Date('2017-12-31'), by ='day'))
#  prev_year_mons <- 10:12
#  curr_year_mons <- 1:5
#   day2sea(x, prev_year_mons, curr_year_mons)  
