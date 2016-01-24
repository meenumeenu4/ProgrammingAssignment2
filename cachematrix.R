## There are two functions here: 1. makeCacheMatrix: This function takes a matrix as 
## input, and returns a list of 4 functions which can be called on this matrix. 
## 2. cacheSolve: This function calculates the inverse of the matrix created above and caches it
## and caches the invere of the matrix. 
## So if the inverse is already calculated once, the same value will be returned instead of calulating again.


## The function takes a matrix as input and returns a special matrix object with 4 operations on it, to get/set the matrix and get/set the 
## inverse of the matrix. It uses superassigment operator (<<-) to store the inverse of the matrix so it can be cached 
## and returned whenever required. 

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL;
  set <- function(y){
    x <<- y;
    invMat <<- NULL;
  }
  get <- function() x;
  
  setInverse <- function(inverse) invMat <<- inverse;
  getInverse <- function() invMat;
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse);

}


## The function works to check if the inverse of the matrix is already calculated 
## or not. If it is already calculated and is not null, then same value is 
## returned without calculating again. Else the inverse is calculated and 
## stored in the invMat object and returned. 

cacheSolve <- function(x, ...) {
  invMat <- x$getInverse();
  if(!is.null(invMat)){
    message("getting cached data");
    return(invMat);
  }
  data <- x$get();
  invMat <- solve(data, ...);
  x$setInverse(invMat);
  invMat;
}
