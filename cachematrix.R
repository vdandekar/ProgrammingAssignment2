# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  # get the value of the matrix
  get<-function() x
  # set the inverse of the matrix
  setmatrix<-function(solve) m<<- solve
  # get the inverse of the matrix
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  # inverse already calculated?
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  # calculate the inverse
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}