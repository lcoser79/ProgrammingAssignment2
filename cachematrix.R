## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The function below creates a mechanism to cache a matrix and its inverse. You
# can set the matrix using the set function, retrieve the matrix using the get
# function, set the inverse using the setinverse function, and retrieve the
# inverse using the getinverse function.

# Defines the function named makeCacheMatrix. It takes an optional argument x,
# which defaults to an empty matrix if not provided.
makeCacheMatrix <- function(x = matrix()) {
        
        # Sets a variable i to store the matrix inverse with NULL initially
        i <- NULL
        
        # The set function is defined within makeCacheMatrix. It takes one
        # argument y, representing a matrix. Inside the function, it assigns
        # the input matrix y to the variable x using <<-, which is the
        # assignment operator that assigns a value to a variable in an
        # environment that is different from the current environment (i.e.,
        # it assigns y to x in the parent environment of the function).
        # It also resets the inverse (i) to NULL.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # get returns the cached matrix x, i.e., it simply returns the current
        # value of x.
        get <- function() x
        
        # setinverse takes one argument inverse, which is the inverse of the
        # matrix. It assigns the inverse to the variable i using <<-, similar
        # to how set assigns y to x.
        setinverse <- function(inverse) i <<- inverse
        
        # getinverse returns the cached inverse i, i.e., it simply returns the
        # current value of i
        getinverse <- function() i
        
        # Creates a list containing the four functions defined above: set, get,
        # setinverse, and getinverse. It returns this list.
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
