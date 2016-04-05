## Put comments here that give an overall description of what your
## functions do

#creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        # create name to store inversion result and initialize to null
        xinverse <- NULL
        
        #set the matrix object
        set <- function(y){
                x <<- y
                xinverse <<- NULL #initialize to NULL
        }
        #get the input matrix
        get <- function() x 
        
        #set the inverse matrix
        setInverse <- function(inv) xinverse <<- inv
        
        #return the matrix as its inverse
        getInverse <- function() xinverse 
        
        #return a list of these functions
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)

}

#This function checks cache for an inverse of the matrix object and 
#returns it if yes, else it creates one and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse() #get the inverse matrix created in the first function
        
        if(!is.null(m)){
                #this means it was previously calculated
                message("Getting cached data")
                return(m)
        }
        data <- x$get() #if m was null, get the matrix object
        m <- solve (data) #create the inverse
        x$setInverse(m)  #set the inverse to the object (cache it)
        m  #return the inverse
}
