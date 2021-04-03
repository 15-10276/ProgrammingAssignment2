## A very introductory note: I'm a latinoamerican guy who is also learning 
## english, I'm sorry if I made some grammar or spelling mistake.  

##### A Functions Description
## With the first function you can create a function which has in its environment 
## four defined functions which allows to store the value of matrix and its
## inverse.
## With second function we can calculate the inverse of a matrix. If the inverse
## is already calculated, it will give us the value. Otherwise, if it is not 
## calculated, it is going to. 


###### MakeCacheMatrix Description
## When you call the function you create a list of functions with a matrix in 
## its arguments. This will allow us to keep the value in the memory. If its
## inverse has not been calculated, "i" is going to be NULL. But, if it is
## calculated lated, this function is going to store the value. You also can 
## reset the value with the "set()" function in its inside.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {      ##Use it, if you need to restart the value
                x <<- y
                i <<- NULL
                }
        get <- function(){ x }   ##To know or retrieve what is the matrix in it
        set_inverse <- function(inverse) {i} <<- inverse  ##To store the inverse
        get_inverse <- function() {i}      ##Obtain the inverse if it's calculated
        list(set = set, get = get,
                set_inverse = set_inverse,
                get_inverse = get_inverse)
}



## This function is allow us to verify if the inverse of a matrix has been 
## calculated. If it is not, then it is going to be calculated. 

cacheSolve <- function(x, ...) {
        i <- x$get_inverse()   ##Obtain the inverse of the matrix
        if(!is.null(i)) {      ##Verify if the inverse already calculated
                message("getting cached data")     
                return(i)         ##If it was, then returned the inverse
                }
        data <- x$get()          ##Else, calculated the inverse and store the value
        i <- solve(data, ...)
        x$set_inverse(i)
        i
}
