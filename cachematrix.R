
makeCacheMatrix <- function(x = matrix()) {
    matrixInv <- NULL
    set <- function(y) {
      x <<- y
      matrixInv <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) matrixInv <<-inverse
    getinverse <- function() matrixInv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrixInv <- x$getinverse()
    if (!is.null(matrixInv)) {
        message("getting inverse matrix is cached")
        return(matrixInv)
    } else {
      matrixInv <- solve(x$get())
      x$setinverse(matrixInv)
      return(matrixInv)
    }
}

#set.seed(1110201)
#r = rnorm(1000000)
#mat1 = matrix(r, nrow=1000, ncol=1000)
#temp = makeCacheMatrix(mat)
# cacheSolve(temp)
#after
#cacheSolve(temp)

#CachedMarix <- makeCacheMatrix(mat1)
#cacheSolve(CachedMarix) 

