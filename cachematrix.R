## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##initialisation
  inverse_x<-NULL
  
  ##set matrix
  set<-function(y){
    x<<-y
    inverse_x<<-NULL
  }
  ##get matrix
  get<-function()x
  
  ##cache inverse matrix
  setInverse<-function(inverse)inverse_x<<-inverse
  
  ##get cached inverse matrix
  getInverse<-function()inverse_x
  
  ##return a list object which contains all methods that can be used to 
  ##get/modify attributes of this "CacheMatrix" i.e. interface APIs
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}




## This function calculate inverse matrix for an "CacheMatrix"

cacheSolve <- function(x, ...) {
  ##retrive data in "CacheMatrix" object x
  inverse_x<-x$getInverse()
  
  if(!is.null(inverse_x)){
    ##cached inverse found, no need to calculate again
    message("getting cached Inverse matrix")
    return(inverse_x)
  }
  
  ##no cached result,get matrix and calculate inverse
  mx<-x$get()
  inverse_x<-solve(mx)
  
  ##set cache in x
  x$setInverse(inverse_x)
  
  ##return
  inverse_x
}
