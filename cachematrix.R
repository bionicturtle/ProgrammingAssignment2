## The purpose here is to get some practice with lexical scoping,
## functions as first-class objects, and lazy evaluation. We are
## doing this by writing (modifying a template) a function that
## caches the inverse of a matrix; i.e., retrieves the stored matrix 
## rather than re-solves it. A key idea here is lexical scoping and
## the use of the super-assignment operator (<--). For example, the
## x in x <<- y is a "free variable" (not defined in its function) such that,
## under lexical scoping, it is retreived (found) in the parent environment.

## To use these functions:
## instance.1 = makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
## instance.1$get() ## Returns original matrix
## cacheSolve(instance.1) ## Computes, caches, and returns    matrix inverse
## instance.1$get.inverse()  ## Returns matrix inverse
## cacheSolve(instance.1)   ## Returns cached matrix inverse using previously computed matrix inverse

## makeCacheMatrix is a function as a first-class object
## The command "instance.1 = makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))"
## creates a variable instance.1, which is an instance of makeCacheMatrix, and
## it contains a list of four functions (aka, methods or members)
## As methods, they will are accessed using the superassignment operator.
## The set function, after checking to ensure the matrix has been changed, 
## assigns the passed matrix, y, and assigns NULL to the cache variable, m.
## set.inverse assigns the inverted matrix to the cache variable, m

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            if (identical(x,y)) return
            x <<- y
            m <<- NULL
      }
      get <- function() x
      
      ## The below is an alternative to: set.inverse <- function(i_matrix) m <<- i_matrix
      set.inverse <- function(i_matrix) assign("m", i_matrix, parent.env(environment()))
      
      get.inverse <- function() m
      list (set = set, get = get, 
            set.inverse = set.inverse,
            get.inverse = get.inverse)
}

## The original matrix instance is passed to this function; e.g., cacheSolve(instance.1)
## If the cache variable, m, is not NULL, then m is simply retrieved and returned
## If the cache is empty, then 1. the inverse is solved via solve(),
## 2. the inverse matrix is stored to the cache variable, m, via the set.inverse(m) call
## and 3. the inverse matrix is returned

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$get.inverse()
      
      ## If inverse matrix is already cached in m, then simply return m
      if (!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      ## If inverse matrix does not exist yet, get original, solve(), and
      ## call set.inverse() method
      data <- x$get()
      m <- solve(data, ...)
      x$set.inverse(m)
      m
}