# In this appointment, we practice the use of the <<- operator to assign a value to an object in environment
## Below are first two functions that are used to create a special object that stores a numeric vector and cache's its mean.
## The first function, makeVector creates a special "vector", which is really a list containing a function to
### set the value of the vector
### get the value of the vector
### set the value of the mean
### get the value of the mean

makeMatrix <- function(x = matrix()) {
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



## The next function calculates the mean of the special "vector" created with the above function. 
## it first checks to see if the mean has already been calculated. 
## If the mean has already been calcuated, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

# Application in a scenario

my_matrix <- matrix(1:4,2,2)

cacheMatrix <- makeMatrix(my_matrix)
cacheMatrix$get()

cacheSolve(cacheMatrix)
