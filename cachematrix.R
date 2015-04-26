
## Create the makeCacheMatrix matrix function, which creates a set
## of stepped functions for the cacheSolve function to use
## functions create matrix, get matrix value, invert and store value,
## then get inverted value from cache and return value to working environment

makeCacheMatrix <- function (x=matrix()){
data_cache<-NULL

set<-function(y){
x<<-y
data_cache<<-NULL}

get<-function()x
setMat<-function(inverse)data_cache<<-inverse
getInv<-function()data_cache

list(set=set,get=get,setMat=setMat,getInv=getInv)
}

## cacheSolve function depends on the makeCacheMatrix function
## cacheSolve tests to see if cached data is present and then
## creates Matrix, runs through error checking of matrix, then
## inverts, stores and returns matrix

cacheSolve<-function(x,...){
data_cache<-x$getInv()

if(!is.null(data_cache)){
message("retrieving cached data if present")
return(data_cache)}

final_matrix<-x$get()

data_cache<-solve(final_matrix,...)

x$setMat(data_cache)

return(data_cache)
}
