# makeCacheMatrix function creates a "matrix" object 
# that can cache its inverse.
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve function returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data.")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setinverse(inver)
  inver
}

#test
> x = rbind(c(1, 2), c(-2, 1))
> m = makeCacheMatrix(x)
>m$get()
 [,1] [,2]
[1,]    1    2
[2,]   -2    1
> cacheSolve(m)
     [,1] [,2]
[1,]  0.2 -0.4
[2,]  0.4  0.2



