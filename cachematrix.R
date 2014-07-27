
# Assignment: Caching and calculating the inverse of a matrix

## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a special matrix that stores a matrix and a cached version.
# The global function contains a secondary function that uses scoping rules to store the inverse
# of a matrix.

# A list of functions within the global function defines whether to set or get the initial matrix,
# whereby setInv and getInv stores and retrieves the inverse of the matrix, respectively.


makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setInv <- function(inv) invM <<- inv
  getInv <- function() invM
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


# Steps
#1. Create a random matrix called m
m<-matrix(1:10,2,5)

#2. Create a second matrix to test the cache function
m_2<-matrix(11:19,3,3)
m_2

#3. Create the first matrix using the function 
create<-makeCacheMatrix(m)

#4. Change the m matrix to the m_2 matrix (cache)
create$set(m_2)

#5. Then inverse the cached matrix:
create$setInv(solve(m_2))

#6. Then retrieve it
create$getInv()



## Using cacheSolve 


cacheSolve <- function(x, ...) {
  # If inverse in cache then return it
  check_inv <- x$getInv()
  if (!is.null(check_inv)) {
    message("getting cached inverse")
    return(check_inv)
  }
  solve_matrix <- solve(x$get())
  x$setInv(solve_matrix)
  solve_matrix
}


#1.  Calculate and cache an empty matrix via the function
create <- makeCacheMatrix()

#2. Create new matrix
m<-matrix(20:29,nrow=2,ncol=5)
m
#3. Set new matrix to function
create$set(m) 

#4. Cache and solve the inverse of the matrix 
cacheSolve(create) # returns a NULL

#5. Make the second, final matrix
final <- matrix(30:35,nrow=2,ncol=2)
final

#6. Set the final matrix within the function
create$set(final)

#7. Solve the inverse of the final matrix
cacheSolve(create)

