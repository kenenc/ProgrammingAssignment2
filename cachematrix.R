## The following functions are designed to set and store a given matrix, and to 
# return the inverse of that matrix (respectively). Both of these functions rely
# on each other in some way; cacheSolve requires an object of type 
# 'makeCacheMatrix' (a regular matrix will not suffice as an argument) in order
# to return the proper inverse matrix, and makeCacheMatrix relies on cacheSolve 
# since it alone cannot perform the inverse of the matrix (notice how the 
# solve() function is only being executed within the cacheSolve function)


# Our first function, makeCacheMatrix, takes the argument x (a matrix) and
# returns a list of functions that can be used in conjunction with the next
# function, cacheSolve. First, it assigns a NULL value to a placeholder variable,
# 'inv'. Then, it creates a sub-function, 'set' as our "setter function" by 
# assigning the next argument, y, to overwrite the previous x value, and reset 'inv'
# to NULL. The 'get' function acts as our "getter function", which simply returns
# our x value. The 'setinverse' function overwrites any previous 'inv' value by 
# letting you input any new matrix into the function, and the 'getinverse' function
# returns the inverse. Lastly, the function calls back a list of items named after
# each function so that each function can easily be called by using the $ operator
# (ex: ExampleMatrix$set(matrix(c(4:2), nrow = 2, ncol = 2)))

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve first calls the getinverse() function to see if there is already one
# stored (cached) in local memory. If there is a valid value there (!is.null),
# then the function will simply prompt a message and return that cached inverse 
# matrix. If there isnt, the function continues: the x argument originally 
# submitted gets returned and assigned to a new variable 'data'. From there,
# it gets plugged into the solve() function, which then stores the inverse 
# matrix back into our 'inv' variable. Lastly, $setinverse() is called to 
# overwrite/cache the old 'inv' value with the new one, and is finally returned 
# at the end.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

