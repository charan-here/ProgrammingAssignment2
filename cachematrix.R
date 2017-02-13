# Function to make Cache Matrix
makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	
	# Caching th evalue of matrix
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
			#get original matrix
            get <- function() x
			
			#Caching the inverse of matrix
            setinverse <- function(solve) m <<- solve
			#To get back the cached inverse matrix
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}


#Function to retrieve cache matrix if already evaluated and compute and store

cacheSolve <- function(x, ...) {
        
		m <- x$getinverse()
			#Checking existence of cahe matrix
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
			# Deriving inverse of matrix
            m <- solve(data, ...)
			#setting inverse into cache
            x$setinverse(m)
			#outputting the inverse
            m
}
