makeCacheMatrix <- function(x = matrix()) {
        
        # creates dummy variable "m"
        m <- NULL
        
        # "set" function - modifies existing
        # source matrix. Both "x" and "m"
        # variables are set at the parent
        # level, or the "cacheSolve" function
        # level.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # "get" function - obtains source matrix
        get <- function() {x}
        
        # "setSolve" function - computes inverse
        # of matrix when called by "cacheSolve".
        #
        # "setSolve" function - receives "solve" function
        # as the "inv_args" variable from "cacheSolve"
        # function, which is pushed back to "m"
        # variable at the "cacheSolve" level using
        # the superassignment (<<-) operator.
        setSolve <- function(inv_args) {m <<- inv_args}

        # "getSolve" function - calls matrix inversion
        # results cached in "m" variable.
        getSolve <- function() {m}
        
        # Output from "makeCacheMatrix" container is
        # stored as a list to be called by "cacheSolve"
        # function using subsetting.
        list(set = set,
             get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

cacheSolve <- function(x, ...) {
        
        # calls "getsolve" function to obtain
        # value of "m" variable, the matrix inversion.
        m <- x$getSolve()
        
        # if the "m" variable is not null,
        # returns cached data using the 
        # of "getSolve" function in "makeCacheMatrix"
        # container. If true, execution ends here.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if the "m" variable is null,
        # retrieves new data using the "get" function
        # in "makeCacheMatrix" container, and stores
        # the new source as "data" variable.
        data <- x$get()
        
        # stores the instructions to obtain matrix
        # inversion of "data" variable into the
        # "m" variable, using the internal R
        # function of "solve".
        m <- solve(data, ...)
        
        # calls "setSolve" function in "makeCacheMatrix"
        # container. Sends "m" variable to "setSolve"
        # function, and saves it as cached variable
        # in "cacheSolve" function using super assignment
        # (<<=) operator. (See notes in "SetSolve" function
        # in "makeCacheMatrix" container for cross-reference)
        x$setSolve(m)
        
        # returns the saved inverted matrix results
        # as "m" variable.
        m
}
