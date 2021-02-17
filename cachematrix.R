########1#########2#########3#########4#########5#########6#########7###########
#                                                                              #
#  cachematrix (02/15/2021)                                                    #
#                                                                              #
#  This program calculates and caches the inverve of a matrix.                 #
#  The program is Programming Assignment #2.                                   #
#                                                                              #
#########1#########2#########3#########4#########5#########6#########7##########


#  funtion makeCacheMatrix creates funtions to calculate the inverse of a
#  given matrix and returns the functions as a list


    makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
    
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
    
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv)
    
    }


#  function cacheSolve checks to see if matrix inverse is in cache.
#  If so, it prints a confirmation message.
#  If not, it calculates the inverse.

    cacheSolve <- function(x, ...) {
        m <- x$getinv()
    
        if (!is.null(m)) {
            message("getting cached data")
            return(m)
        }
    
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
    
}


#  define matrix and compute inverse (as a test)

    my_matrix <- matrix(c(4, 7, 3, 6), ncol = 2)
    my_inverse <- solve(my_matrix)
    my_inverse


#  now calculate the inverse using the two functions

    my_inverse2 <- makeCacheMatrix(my_matrix)
    cacheSolve(my_inverse2)
