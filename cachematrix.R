#FUNCTION 1: makeCacheMatrix
#creates an R object that stores a matrix and its inverse.

makeCacheMatrix <- function(x = numeric()) {
   #MakeCacheMatrix environment comes under global environment
  
    m <- NULL # intitializing m, x already initialized as function argument above
   # x and m are data objects inside MakeCacheMatrix environment
  
    #FUNCTION 1A: Has it's own environment inside MakeCacheMatrix environment
     set <- function(y) {
      x <<- y
      m <<- NULL
      # This function does the following:
      # 1.Assign the input argument to the x object in the parent environment &
      # 2.Assign the value of NULL to the m object in the parent environment & clears any value of m that 
        #had been cached by a prior execution of cachSolve().
   }
   
     #FUNCTION 1B:Has it's own environment inside MakeCacheMatrix environment
     #defines the getter for the matrix x.
     #lexical scoping features in R allows this function to retrieve x from makeCacheMatrix environment
      get <- function() x
   
      #FUNCTION 1C:Has it's own environment inside MakeCacheMatrix environment
      #This function sets the vaule of the inverse to the variable m which is defined in the parent environment
      setinverse <- function(inverse) m <<- inverse
   
      #FUNCTION 1D:Has it's own environment inside MakeCacheMatrix environment
      #This function retrieves the correct value of m using lexical scoping
      getinverse <- function() m
   
   # assigns each of these functions as an element within a list(),
   #and returns it to the parent environment bacause of whihc we can use $ to extract information
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

#FUNCTION2:cacheSolve
#requires an argument that is returned by makeCacheMatrix()
#in order to retrieve the inverse from the cached value 
#that is stored in the makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
   
   #attempts to retrieve a inverse from the object passed in as the argument
   #and stores in m
   m <- x$getinverse()
   
   if(!is.null(m)) {
      #checks ot see if m is a null or not
      #if not then it retrieves the cache data
      message("getting cached data")
      return(m)
   }
   
   # if the value is false then it finds inverse using "solve" function 
   #and then returns the value of the inverse to the parent environment 
   #by printing the inverse object.
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
}