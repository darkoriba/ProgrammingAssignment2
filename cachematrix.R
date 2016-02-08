 ## The first function, makeVector creates a special "vector",
 ## which is really a list containing a function to

 ## set the value of the vector
 ## get the value of the vector
 ## set the value of the mean
 ## get the value of the mean


 makeCacheMatrix <- function(x = matrix()) {
        d <-NULL
        ## set value of the martix
        set <- function(y) {
                x <<- y
                d <<- NULL
        }
        ## get value of the matrix
        get <- function() x
        
        ## set and get value of the inverse
        set_inverse <- function(inverse) d<<- inverse
        get_inverse <- function () d
        
        ## list of all functions above
        list(set = set, 
             get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)

        }


 ## The following function calculates the mean of the special "vector" 
 ## created with the above function. However, it first checks to see if 
 ## the mean has already been calculated. If so, it gets the mean from 
 ## the cache and skips the computation. Otherwise, it calculates the mean 
 ## of the data and sets the value of the mean in the cache via the setmean 
 ## function.

 cacheSolve <- function(x, ...) {
        ##check if the inverse is alredy cached and if so the inverse is from
        ## cache directly
        d <- x$get_inverse()
                if(!is.null(d)) {
                        message("getting cached data")
                        return (d)
                }
       ## get matrix first
        data<- x$get()
       ## calculate the inverse
        d <- solve(data, ...)
       ## cache the inverse of matrix
        x$set_inverse(d)
       ## return the result
        d
}
