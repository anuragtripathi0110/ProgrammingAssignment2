makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL #initializing inverse as null
  }
  get <- function() {x} #function to get matrix x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
cacheSolve <- function(x, ...) #gets cache data
{
  inv <- x$getInverse()
  if(!is.null(inv)) #checking whether inverse is null
  {
    message("getting cached result/data")
    return(inv) #returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...) #calculate the inverse value
  x$setInverse(inv)
  inv #returns the inverted matrix x
}