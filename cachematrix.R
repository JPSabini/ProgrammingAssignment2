#
# MakeCacheMatrix is a function which sets up a list which is an envirnment 
# containing the matrix into local data cache. The Solver stores the inverse of the matrix
#into the invA varaible. THe InvA variable is set to false and is accessed via setinva method.
# it also sets up the get/set interfaces to get the data for the solver and to
# 

makeCacheMatrix <-function(x=matrix(),...){
  invA<-NULL
  setinvA<-function(y){
    A<<-y
    invA <<-NULL
    
  }
  get <- function() x
  setinvA <- function(solve) { invA <<-solve }
  getinvA <- function() invA  
  list(setinvA = setinvA, get = get, 
       setinvA =setinvA,
       getinvA = getinvA)
  
}


## Write a short comment describing this function

#cacheSolve invokes the structure environment, A , created by makeCacheMatrix
# it invokes the solver and returns a matrix inverse. If the same matrix is invoked
# then the cacheSolve process get s the results from the cache and prints a message.

cacheSolve<-function(x, ...){
  invA<-x$getinvA()
  if (!is.null(invA)){
    message("getting cached data")
    return(invA)
  }
  data <- A$get()
  invA <- solve(data)
  A$setinvA(invA)
  invA
}