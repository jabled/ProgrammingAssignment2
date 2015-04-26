
## Create the makeCacheMatrix matrix function, which creates a set
## of stepped functions for the cacheSolve function to use
## functions create matrix, get matrix value, invert and store value,
## then get inverted value from cache and return value to working environment

# create the cache matrix function
makeCacheMatrix <- function (x=matrix()){

# create the cache function and initialize it (to null)
data_cache<-NULL

# create the function to set the matrix
set<-function(y){
x<<-y
data_cache<<-NULL}

# create the additional functions to grab the matrix, invert it and then store it
get<-function()x
setMat<-function(inverse)data_cache<<-inverse
getInv<-function()data_cache

# create a list with all of the matrix values
list(set=set,get=get,setMat=setMat,getInv=getInv)
}

## cacheSolve function depends on the makeCacheMatrix function
## cacheSolve tests to see if cached data is present and then
## creates Matrix, runs through error checking of matrix, then
## inverts, stores and returns matrix

cacheSolve<-function(x,...){
data_cache<-x$getInv()

# check to see if the cache contains data or not
# assumption is that the cache will contain data per assignment instructions so error checking is not necessary

if(!is.null(data_cache)){
message("retrieving cached data if present")
return(data_cache)}

final_matrix<-x$get()

#employ the solve function to create the final inverse matrix
data_cache<-solve(final_matrix,...)

#set/store and return the value of the cached inverted matrix
x$setMat(data_cache)

return(data_cache)
}
