
#the first function 'makeCacheMatrix' creates a special matrix which is a list containing a function to
#(i)set the matrix (ii)get the matrix (iii)set the inverse of the matrix (iv) get the inverse of the matrix.

makeCacheMatrix <- function(x = numeric()) {    #makeCacheMatrix function creates a special matrix object x that can cache its inverse
         i <- NULL                              #assigns value null to i variable in the current environment
        set<-function(y){
                x<<-y
                i<<-NULL                       #assigns value null to i variable in the environment that is different from the current environment
        }
        get <- function() x                    #get function gets the mean from the cache
        setinverse<-function(inverse) i <<- inverse  #setinverse function sets the value of the inverse in the cache
        getinverse<-function() i
        list(set = set, get = get,
             setinverse=setinverse,
             getinverse=getinverse)
}

#The function 'cacheSolve' computes the inverse of the special matrix returned by 'makeCacheMatrix above.
#If the inverse has already been calculated and the matrix has not changed ,then the cacheSolve retrieves the inverse from the cache.

cacheSolve<- function(x, ...) {                 #cachesolve function retrieve the inverse from the cache
        i<-x$getinverse()
        if(!is.null(i)) {                  
                message("retrieving cached data for inverse of matrix")
                return(i)
        }
        data<-x$get()
        i<- solve(data, ...)                   #calculates inverse of matrix using solve() function 
        x$setinverse(i)  
        i                                       
}
