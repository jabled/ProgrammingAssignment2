
## Create the makeCacheMatrix matrix function, which creates a set
## of stepped functions for the cacheSolve function to use
## functions create matrix, get matrix value, invert and store value,
## then get inverted value from cache and return value to working environment

makeCacheMatrix <- function (x=matrix()){
cache<-NULL

set<-function(y){
x<<-y
cache<<-NULL}

get<-function()x
setMatrix<-function(inverse) cache<<-inverse
getInverse<-function()cache

list(set=set,get=get,setMatrix=setMatrix,getInverse=getInverse)
}

##cacheSolve function depends on the makeCacheMatrix function
## cacheSolve tests to see if cached data is present and then
## creates Matrix, runs through error checking of matrix, then
## inverts, stores and returns matrix

cacheSolve<-function(x,...){
cache<-x$getInverse()

if(!is.null(cache)){
message("retrieving cached data if present")
return(cache)}

matrix<-x$get()

tryCatch(
{cache<-solve(matrix,...)},
error=function(error_output)
{message("error:")
message(error_output)
return(NA)},
alert=function(error_output)
{message("warning:")
message(error_output)
return(NA)},
results={x$setMatrix(cache)}
)
return(cache)
}

