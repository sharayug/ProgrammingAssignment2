# The below function creates R object (matrix) that caches its inverse 
makeCacheMatrix <- function(our_matrix = matrix()) {
  inverse_matrix <- NULL
  
  set <- function(y) {
    c_our_matrix <<- y
    c_inverse_matrix <<- NULL
  }
  get <- function() { our_matrix }
  
  get_c_our_matrx <- function() {
    if(!exists("c_our_matrix")) return(NULL) else return(c_our_matrix) 
  }
  
  set_inv <- function(inverse_mat) { c_inverse_matrix <<- inverse_mat }
  
  get_inv <- function() { c_inverse_matrix }
  
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv,get_c_our_matrx=get_c_our_matrx)
}

# The below function will compute the inverse of matrix x. If the argument has been originally passed,
# the inverse will be retrieved from the cache .
cacheSolve <- function(x, ...) {
  
  if(!identical(x$get(),x$get_c_our_matrx()))
    x$set(x$get())
  
  inverse_matrix <- x$get_inv()
  
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  data <- x$get()
  message("calculating inverse matrix")
  inverse_matrix <- solve(data, ...)
  x$set_inv(inverse_matrix)
  inverse_matrix
}


# cacheSolve(makeCacheMatrix(x))