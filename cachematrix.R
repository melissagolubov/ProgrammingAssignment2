## I created two functions "makeCacheMatrix and cacheSolve, to cache the inverse
## of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function (x= matrix() ) {
  i<- NULL ##Initialize objects
  set<- function (y) {   ##Set the function of the objects
    x <<- y
    i <<- NULL
  }
  get <- function () { ## Method to get the matrix
    x
  } ##Set and get the inverse
  setInverse <- function (inverse) i<<- inverse
  getInverse <- function () i
  list (set=set, get=get, setInverse=setInverse, getInverse=getInverse) ##Returns a list, assigns each function as an element of such list
}



## Returns the inverse of the matrix returned by myCacheMatrix

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)){ ##If the inverse is already set, return it
    message ("getting cached data :)")
    return (i)
  }
  data <- x$get() ##If not, it calculates the inverse and sets it with setInverse
  i <- solve (data, ...)
  x$setInverse(i)
  i
}
