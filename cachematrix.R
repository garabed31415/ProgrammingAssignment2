##  Short comment describing makeCacheMatrix() function:

##  makeCacheMatrix() function creates a "special vector" x, which is actually 
##  a list that contains four functions, referenced by:
##     1- x$set():         sets the value of the matrix
##     2- x$get():         gets the value of the matrix
##     3- x$setinverse():  sets the value of the inverse
##     4- x$getinverse():  gets the value of the inverse

##   It initializes the inverse matrix to NULL (cached)

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)  
}


##  Short comment describing cacheSolve() function:

##  1- if the matrix has been changed, it will calculate the new inverse,
##     cache the inverse, and return the inverse.

##  2- if the matrix has not changed, it will return the existig cached
##     inverse matrix -- NO inverse calculations are necessary!
##     It also prints out notification: "getting cached data"

##  3- if an IDENTICAL matrix with DIFFERENT name is introduced, it will
##     then RECALCULATES the inverse; NOT TO BE CONFUSED with case-2 above!

cacheSolve <- function(x, ...) {

    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
