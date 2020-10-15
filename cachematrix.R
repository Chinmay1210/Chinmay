## This function makeCacheMatrix creats a apecial "matrix',which is really as list containing a function to  
## 1. set the value of the special matrix
## 2. get the value of the special matrix
## 3. set the value of inverse special matrix
## 4. get the value of the mean special matrix

## Let us set and the value of special matrix

makeCacheMatrix <- function(x = matrix()) {
  g <- NULL
  set <- function(y){
    x <<- y
    g <- NULL
  }
  get <- function()x
  setInverse <- function(inverse) g <<- inverse
  getInverse <- function()g
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Let us compute the inverse of the special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  g <- x$getInverse()
  if(!is.null(g)){
    message("getting cached data")
    return(g)
  }
  mat <- x$get()
  g <- solve(mat, ...)
  x$setInverse(g)
  g
}
x
