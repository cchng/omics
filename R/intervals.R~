# @cchng


# function for computing whether point x
# is between point a and b in 1-dimensional space.
is.between <- function(x,a,b){
  if(x>=a & x<=b)return(TRUE)
  return(FALSE)
}

# vectorized function for computing whether two intervals overlap
is.overlap <- Vectorize(function(startA,endA,startB,endB){
  if(endA<=startB|startA>=endB){ # not overlapping
    return(FALSE)}

  return(TRUE)
})


# function for computing whether two intervals
# reciprocally overlap each other
mutual.overlap <- Vectorize(function(startA,endA,startB,endB,perc.overlap){

  if(endA<=startB|startA>=endB){ # not overlapping
      return(FALSE)

  }else if(is.between(startB,startA,endA)&is.between(endB,startA,endA)){ # if B nested in A
      return(TRUE)

  }else if(is.between(startA,startB,endB)&is.between(endA,startB,endB)){ # if A nested in B
      return(TRUE)

  }else if(is.between(startB,startA,endA)){
    ioverlap <- (endA-startB)/(endA-startA)
    roverlap <- (endA-startB)/(endB-startB)

    if(ioverlap>=0.9&roverlap>=perc.overlap) { # mutual overlap
      return(TRUE) # 90% overlap
    }

    return(FALSE)

  }else if(is.between(startA,startB,endB)){
    ioverlap <- (endB-startA)/(endA-startA)
    roverlap <- (endB-startA)/(endB-startB)

    if(ioverlap>=0.9&roverlap>=perc.overlap) { # mutual overlap
      return(TRUE) # 90% overlap
    }
    return(FALSE)
  }
  gc()
  return(FALSE)
})


