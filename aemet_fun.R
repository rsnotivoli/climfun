#' some functions to handle datasets in wide format of AEMET (Spanish Met. Agency)

#' #######################
#' TRANSPONSE FUNCTION 
#' #######################
#' This function transponses climate data on daily scale but grouped monthly on rows to a new
#' structure with a day on each row.

#' @param data data frame with original data, arranged daily but with one row per month
#' @param ini initial date when you want your final data frame begin. Must be Date object (daily)
#' @param end final date when you want your final data frame end. Must be Date object (daily)
#' @param idcol index of the column wich contains the identifiers of each station. Default==1.
#' @param yearcol index of the column wich contains the year of each observation. Default==2.
#' @param moncol index of the column wich contains the month of each observation. Default==3.
#' @param inidata index of the column wich contains the data of the first day of the month. Default==9. Note that 
  #the function has been programmed to register 31 days all months, so is important to keep all columns
  #even if there is not data



    transpose <- function(data, ini, end, idcol=1, yearcol=2, moncol=3,inidata=9){
      
      fechas <- format(seq.Date(from=as.Date(ini),to=as.Date(end),by="day"),"%Y-%m-%d")
      ids <- levels(as.factor(data[,idcol]))
      var <- as.data.frame(fechas)
      mat <- matrix(NA, ncol = length(ids), nrow = length(fechas))
      colnames(mat) <- ids
      for(j in 1:length(ids)){
        sub <- subset(data,data[,idcol]==ids[j]); sub <- sub[,c(yearcol,moncol,inidata:(inidata+30))]; colnames(sub)[3:33] <- 1:31
        lsub <- melt(sub, id.vars=c(1,2), variable_name='day', na.rm=TRUE); lsub$fechas <- format(as.Date(paste(lsub[,1],lsub[,2],lsub[,3],sep='-')),'%Y-%m-%d')
        lsub <- data.frame(lsub$fechas,lsub$value); names(lsub) <- c('fechas','value')
        kk <- merge(lsub,var,by='fechas',all=TRUE); kk <- kk[order(levels(as.factor(kk$fechas))),]
        mat[,j] <- kk[,2]
      }
      return(mat)  
    }


#' #######################
#' STATIONS FUNCTION 
#' #######################
#' This function creates a data frame with individualized information for each station
 
#' @param data data frame with original data, arranged daily but with one row per month
#' @param idcol index of the column wich contains the identifiers of each station. Default==1.
#' @param namcol index of the column wich contains the name of each station. Default==4.
#' @param altcol index of the column wich contains the elevation of each station. Default==5.
#' @param xcol index of the column wich contains the X coordinate of each station. Default==6.
#' @param ycolindex of the column wich contains the Y coordinate of each station. Default==7.

    stations <- function(data, idcol=1, namcol=4, altcol=5, xcol=6, ycol=7){
            ids <- levels(as.factor(data[,idcol]))
            sts <- as.data.frame(matrix(NA,ncol=5,nrow=length(ids)))
            names(sts) <- c('ID','NAME','ALT','X','Y'); sts$ID <- ids
            for(i in 1:length(ids)){
              sub <- subset(data,data[,idcol]==ids[i])
              sts$NAME[i] <- as.character(data[which(data[,idcol]==ids[i])[1],namcol])
              sts$ALT[i] <- as.character(data[which(data[,idcol]==ids[i])[1],altcol])
              sts$X[i] <- as.character(data[which(data[,idcol]==ids[i])[1],xcol])
              sts$Y[i] <- as.character(data[which(data[,idcol]==ids[i])[1],ycol])
            }
            sts <- sts[which(!is.na(sts$NAME)),]
            rm(sub,i)
            return(sts)
    }
    
