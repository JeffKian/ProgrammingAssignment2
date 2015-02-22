makeCacheMatrix <- function(x = matrix()) {
  ## this generates a list of functions that are stored in a list you can call and pass values to
  ## It is expecting an input matrix 'x'
  ## function sets local m variable as null
    
  m<-NULL
    
  set<-function(y){
    ## set function is expecting 1 input parameter
    ## x is super assigned value of input input parameter
    ## x will be available globally if called
    x<<-y
    
    ## m is super assigned the value of null and will remain null even after the function exits unless something else re-assigns it
    m<<-NULL
  }
  get<-function() x
  
  ## empty get function just returns the value of the input vector
  
  setmatrix<-function(solve) m<<- solve
  ## setmatrix function has input parameterand re-ssigns m to the value of the input parameter
  ## m is super assigned and this value is visible globally
  getmatrix<-function() m
  
  ##get matrix empty function that returns the value of m
  
  ## renames the columns of the generated list so they can be addressed by name
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)}

cacheSolve <- function(x=matrix(), ...) {
  # cacheSolve is expecting an input matrix which is the result the makeCacheMatrix function
  ## The output of makeCacheMatrix is special vector of which the values are functions that can be called inside CacheSolve  ## m is assigned the value of x$getmatrix() function and the 1st time will be empty
  ## x$getmatrix() just returns value of m from the makevector function
  ## m gets assigned current global value of m
  ##if value of m is not null then use the cached value of m
  
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)  }
  
  ##if value of m is null then get the input matrix by calling the x$get function
  ## solve the matrix for its inverse and assign that value to m
  ## by calling set matrix you assign m the value of the solve in the global / parent environment
  
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m}
