## create a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y){
         x <<- y
       inv <<- NULL
    } 
   get <- function()x
   setInverse <- function(inverse) inv <<- inverse
   getInverse <- function() inv
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse
}


## create cacheSolve function to compute the inverse. 
 
cacheSolve <- function(x, ...) {
        
      inv <-x$getInverse()
      
      #if the inverse has been calculated,get it from cached and skip computation
     
      if(!is.null(inv)){
         message("getting cached data")
         return(inv)
       }
       
       #otherwise, calculate the inverse 
      
       mat <- x$get()
       inv <- solve(mat,...)
       x$setInverse(inv)
       inv

}
