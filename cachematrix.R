## The aim is to cache the computed inverse of a matrix so that the next time the same matrix is fed as input, it is not calculated
#but just retrieved from the cache. 
##2 functions - makeCacheMatrix and cacheSolve. 

##makeCacheMatrix is the function that caches the input matrix and its inverse 
makeCacheMatrix <- function(x = matrix()) {
inverse<-NULL
 set<-function(y) {
 x<<-y
 inverse<<-NULL }
 setInv<-function(inv) inverse<<-inv
 getInv<-function() inverse
 list(set=set, get=get, setInv=setInv, getInv=getInv) }
}

##cacheSolve is the function that retrieves the inverse of the given matrix if already cached. If not, it computes the inverse
## and caches the result while returning the inverse as output. 
cacheSolve <- function(x, ...) {
       inverse<-x$getInv()
if(!is.null(inverse)) {
message("Retrieving data from Cache")
return(inverse) }
data<-as.vector(x$get()) ##data variable assigned as a vector and will be converted to matrix inside solve function
len=sqrt(length(data)) ## len will be used to set the number of rows for the square matrix.
inverse<- solve(matrix(data,nrow=len))
x$setInv(inverse)
 ## Return a matrix that is the inverse of 'x'
inverse }
}

##example for execution: 
## a<-makeCacheMatrix(c(3,2,0,0,0,1,2,-2,1))
## cacheSolve(a)
## cacheSolve(a) ##executing this again will display the message "Retrieving data from Cache" 
