## R Programming Assignment 2 - Karan Mehta

## Following functions seed up time consuming computations like
## caluculating the inverse of a matrix by caching results the first time
## the inverse of the matrix is calculated. This avoids the situation where
## the matrix inverse is computed repeatedly.

## The functions below used to claculate the inverse of a matrix and
## cache the results for late use.

## FunctionName: makeCacheMatrix

## Input: 'x' where 'x' is a matrix - assumes the 'x' is invertable

## Output: None. The makeCacheMatrix function does the following 
## computations:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

## Note: the inverse of the input matrix is NOT calculated in the
## makeCacheMatrix(m) function

makeCacheMatrix <- function(x = matrix()) {
	mInv <- NULL
	set <- function(y){
		x <<- y
		mInv <<- NULL
	}
	get <- function() x
	setInv <- function (inv) mInv <<- inv
	getInv <- function () mInv
	list( set = set, get = get,
		setInv = setInv, getInv = getInv)	
}


## Write a short comment describing this function
## FunctionName: cacheSolve
## Input: 'x' where 'x' is a matrix - assumes the 'x' is invertable

## Output: Inverse of the matrix 'x'. The function first call the getInv function
## form the previous function (example of lexial scoping) and checks if 
## the invese of a matrix has a value.If a value exists, it is returned.

## If the getInv returns a NULL value then the inverse of matrix 'x' is 
## computed using the solve(x) function. At this point the setInv function
## in the makeCacheMatrix function is also called to set the results (cache the result)
## of the matrix inverse for future use.


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	mInv <- x$getInv()
	if (!is.null(mInv)){
		message("getting cached data")
		return (mInv)
	}

	matrixData <- x$get()
	mInv <- solve(matrixData)
	x$setInv(mInv)
	mInv
}
