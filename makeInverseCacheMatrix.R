## library mass is used to calculate non square matrix  too
## this s program consist of two function
## Function1 - makeCacheMatrixwhich consist of get,set,getInverse,setInverse
## Function2 cachesolve solves the matrix








makeCacheMatrix <- function(x = matrix()){
      inv <- NULL                               ## initializing as null
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() {x}             ## to get matrix
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){        ## gets cached matrix
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)    ## calculate inverse matrix
      x$setInverse(inv)
      inv                       ## returns inverse matrix
}
