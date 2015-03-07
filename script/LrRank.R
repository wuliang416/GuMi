LrRank <- function (object , x , y)
{
  quantitativeVarName <- names(which(!sapply(x,is.factor)))
  tmpVarImp <- varImp(object , scale = F)
  tmpVarImp <- tmpVarImp[order(tmpVarImp$Overall , decreasing = T) , , drop = F]
  vimp <- data.frame()
  len <- length(tmpVarImp$Overall)
  rowName <- row.names(tmpVarImp)
  
  # for qualitative var, a dict map its design name to origin name
  qualitativeVarLevels <- object$xlevels
  qualiVarLen <- length(qualitativeVarLevels)
  qualitativeVarMap <- vector()
  for( idx in 1:qualiVarLen)
  {
    levelLength <- length(qualitativeVarLevels[[idx]])
    qualitativeVarMap <- append(qualitativeVarMap , 
                                setNames(rep(names(qualitativeVarLevels[idx]) , levelLength - 1 ) , 
                                         paste(names(qualitativeVarLevels[idx]) , qualitativeVarLevels[[idx]][2:levelLength] ,sep = "")))
  }
  #qualitativeVarMap[rowName[idx]]
  
  # a dictionary with qualitative var name as its key
  qualitativeVarName <- names(which(sapply(x,is.factor)))
  qualitativeVarIsAdded <- setNames(rep(F , length(qualitativeVarName)) , qualitativeVarName)
  
  for( idx in 1:len )
  {
    if( rowName[idx] %in% quantitativeVarName)
      vimp <- rbind(vimp , data.frame( row.names = rowName[idx] , Overall = tmpVarImp[idx,'Overall']))
    else
    {
      if( !qualitativeVarIsAdded[qualitativeVarMap[rowName[idx]]] )
      {
        vimp <- rbind(vimp , data.frame( row.names = qualitativeVarMap[rowName[idx]] , Overall = tmpVarImp[idx,'Overall']))
        qualitativeVarIsAdded[qualitativeVarMap[rowName[idx]]] <- T
      }
    }
  }
  vimp$var <- rownames(vimp)
  vimp
}