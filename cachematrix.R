## R programming assignment 2
## Created Date: December 8, 2019
# 
# Description:
# This function is able to cache potentially time-consuming 
#       computations, namely calculating the inverse of a
#       matrix. This function takes advantage of the 
#       scoping rules of the R language and how they can be 
#       manipulated to preserve state inside of an R object.
# First, we create a matrix object

makeCacheMatrix <- function(x = matrix()) {   
  m <- NULL        
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# In code above, we initialized x as function argument.
# We then initialized m and set it to NULL for later use.
# The code above also provides for "getters and settters." 
#        Getters retrieve data within an object, and 
#        setters set the data within an object.
# Last section of code assigns each of the functions created 
#       as an element within a list(), and returns it 
#       to the parent environment.
# The fully formed object makeCacheMatrix will be used by 
#       the next section of code.
# The next section of code returns a matrix that is 
#       the inverse of 'x'.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
  }

## TEST  
# Example: A simple matrix m1 with a simple matrix inverse n1
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# Running this code ...
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
# ... should return n1
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)


