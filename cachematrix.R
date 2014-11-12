## =================================================================
## These functions calculate the inverse of a matrix 
## for use in long-running sessions with large data sets
## There are 2 pairs of functions that do the same thing 
## in slightly different ways
##
makeCacheMatrix <- function(x=numeric()) {
  m <- NULL
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse=  getinverse)
  
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  mx <- x$getinverse()
  if(!is.null(mx)) {
    message("getting cached data")
    mx
  } else 
  {
    message("calculating from matrix data")
    dta <- x$get()
    tmp <- solve(dta)
    x$setinverse(tmp)
    tmp 
  }
}


print(" Test Suite 101")

print("[101][01]: Create Matrix")
om <- matrix(c(11,12,13,14),2,2)
print(om)
print("--------------------------------------------------------------------------")
print("                                                                          ")

print("[101][02]: Call makeCacheMatrix(om)")
cm <- makeCacheMatrix(om)
print("--------------------------------------------------------------------------")
print("                                                                          ")

print("[101][03]: Show Initial Cache Matrix Content")
print(cm$get())
print("--------------------------------------------------------------------------")
print("                                                                          ")

print("[101][04]: Call cacheSolve and Show Inverse As Return Value Of cacheSolve")
print(cacheSolve(cm))
print("--------------------------------------------------------------------------")
print("                                                                          ")

print("[101][05]: Show Inverse Stored In Cache Matrix cm")
print(cm$getinverse())
print("--------------------------------------------------------------------------")
print("                                                                          ")

print("[101][06]: Call cacheSolve Again To Show Use Of Cached Value")
print(cacheSolve(cm))
print("--------------------------------------------------------------------------")
print("                                                                          ")

print("[101][07]: Show Inverse Stored In Cache Matrix cm")
print(cm$getinverse())
print("--------------------------------------------------------------------------")
print("                                                                          ")

print("[101][08]: Modify Original Matrix Stored In Cache Matrix cm")
cm$set(matrix(c(3,0,111,68),2,2))
print("--------------------------------------------------------------------------")
print("                                                                          ")
print("[101][09]: Show New Matrix Stored In Cache Matrix cm")
print(cm$get())
print("--------------------------------------------------------------------------")
print(" ")
print("[101][10]: Call cacheSolve and Show Inverse As Return Value Of cacheSolve")
print(cacheSolve(cm))
print("--------------------------------------------------------------------------")
print("                                                                          ")
print("[101][11]: Show New Inverse Stored In Cache Matrix cm")
print(cm$getinverse())
print("--------------------------------------------------------------------------")
print("                                                                          ")
print("[101][12]: Call cacheSolve Again To Show Use Of Cached Inverse Value")
cacheSolve(cm)
print("--------------------------------------------------------------------------")
print("                                                                          ")
print("[101][13]: Show Inverse Stored In Cache Matrix cm Using getinverse()")
print(cm$getinverse())
print("                                                                          ")
print("===========================================================================")
print("                                                                          ")
print("=== Test Suite #2 : USING SAMPLE SESSION ===")

print("[102][01]: Create Matrix Of Test Values (amatrix")
morg <- matrix(c(1,2,3,4),2,2)
print(morg)
print("--------------------------------------------------------------------------")
print("                                                                          ")

print("[102][02]: Call makeCacheMatrix(amatrix)")
amatrix <- makeCacheMatrix(morg)
print("--------------------------------------------------------------------------")
print("                                                                          ")

print("[102][03]: Show Initial Cache Matrix Content")
print(amatrix$get())         # Returns original matrix
print("--------------------------------------------------------------------------")
print("                                                                          ")

# EXPECTED OUTPUT
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4

print("[102][04]: Call cacheSolve and Show Inverse As Return Value Of cacheSolve")
print(cacheSolve(amatrix))   # Computes, caches, and returns    matrix inverse
print("--------------------------------------------------------------------------")
print("                                                                          ")

# EXPECTED OUTPUT
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

print("[102][05]: Show Inverse Stored In Cache Matrix cm")
print(amatrix$getinverse())  # Returns matrix inverse
print("--------------------------------------------------------------------------")
print("                                                                          ")

# EXPECTED OUTPUT
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

print("[102][06]: Call cacheSolve Again To Show Use Of Cached Value")
print(cacheSolve(amatrix))   # Returns cached matrix inverse using previously computed matrix inverse
print("--------------------------------------------------------------------------")
print("                                                                          ")

# EXPECTED OUTPUT
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

print("[102][07]: Show Inverse Stored In Cache Matrix cm")
print(amatrix$getinverse())  # Returns matrix inverse
print("--------------------------------------------------------------------------")
print("                                                                          ")

print("[102][08]: Modify Original Matrix Stored In Cache Matrix cm")
amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
print("--------------------------------------------------------------------------")
print("                                                                          ")

print("[102][09]: Show Content Of New Matrix Stored In Cache Matrix cm")
print(cm$get())
print("--------------------------------------------------------------------------")
print("                                                                          ")

print("[102][10]: Call cacheSolve and Show Inverse As Return Value Of cacheSolve")
print(cacheSolve(amatrix))   # Computes, caches, and returns new matrix inverse
print("--------------------------------------------------------------------------")
print("                                                                          ")

# EXPECTED OUTPUT
# print(cacheSolve(amatrix))
# [,1] [,2]
# [1,] -0.13333333  0.2
# [2,]  0.01010101  0.0

print("[102][11]: Show New Inverse Stored In Cache Matrix cm")
print(amatrix$getinverse())
print("--------------------------------------------------------------------------")
print("                                                                          ")
# [,1] [,2]
# [1,]    0   99
# [2,]    5   66

print("[102][12]: Call cacheSolve Again To Show Use Of Cached Inverse Value")
print(cacheSolve(amatrix))  # Returns matrix inverse
print("--------------------------------------------------------------------------")
print("                                                                          ")

# EXPECTED OUTPUT
# print(amatrix$getinverse())
# [,1] [,2]
# [1,] -0.13333333  0.2
# [2,]  0.01010101  0.0

print("[102][13]: Show Inverse Stored In Cache Matrix cm Using getinverse()")
print(cm$getinverse())
print("                                                                          ")
print("========================== TESTS COMPLETE ================================")
print("                                                                          ")

