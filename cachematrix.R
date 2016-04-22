## make CacheMarix Creates a special List, this List in fact contains 4 functions:
## set the Matrix
## get the Matrix
## set the inv of the matrix
## get the inv of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(){
        x<<-y
        m<<-NULL
    }
    get <- function() x
    setinv <- function(inv) m<<-inv
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv =getinv)
}


## the cachheSolve first tries to see if the inv has already been stored
## if its true, return the cached data
## if not, use the function "solve"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <-x$get()
    m<-solve(data, ...)
    x$setinv(m)
    m
}
