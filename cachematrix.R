#The functions below are to find out the inverse of a matrix.
#If the contents of a matrix are not changing, it may make sense to cache the inverse matrix
#so that when we need it again, it can be looked up in the memory rather than recomputed.


# makeCacheMatrix function creates a special "matrix" object that can store its inverse.
#This special matrix has list containing functions to
#set the values for the matrix (set function), 
#get the values of a matrix (get function), 
#set the inverse of the matrix (setinverse function) and 
#get the inverse of a matrix (getinverse function)
#so that it can store the elements of a matrix and return the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y) 
    {
        x<<-y
        inv<<-NULL
    }
    
    get<-function()
    {
        x
    }
    setinverse<-function(inverse)
    {
        inv<-inverse
    }
    getinverse<-function()
    {
        inv
    }
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
    

}


#cacheSolve function is to find out the inverse of thespecial matrix created above.
#It will check if inverse is already stored in the above function.
#if it is there, it will return the the values, 
#otherwise will calucalute the inverse of the matrix using Sovle() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

