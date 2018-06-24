## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inverse<-NULL
 set<-function(y) {
 x<<-y
 inverse<<-NULL }
 setInv<-function(inv) inverse<<-inv
 getInv<-function() inverse
 list(set=set, get=get, setInv=setInv, getInv=getInv) }
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inverse<-x$getInv()
if(!is.null(inverse)) {
message("Retrieving data from Cache")
return(inverse) }
data<-as.vector(x$get()) ##data variable assigned as a vector and will be converted to matrix inside solve function
len=sqrt(length(data)) ## len will be used to set the number of rows for the square matrix.
inverse<- solve(matrix(data,nrow=len))
x$setInv(inverse)
inverse }
}
