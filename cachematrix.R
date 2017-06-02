# 'makeCacheMatrix' does the following:
## Creates four functions (set, get,setmatrix and getmatrix)
## Creates two objects (x and m)
## Creates list of names for all four functions
makeCacheMatrix<-function(x=matrix()){
  m<-NULL
  set<-function(y)
  {x<<-y
  m<<-NULL}
  get<-function()x
  setmatrix<-function(matrix)
    m<<-matrix
  getmatrix<-function()m
  list(set=set,
       get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

# 'cacheSolve' does the following:
## Retreives the getmatrix input from above object 'makeCacheMatrix'
## Checks if the result is set to NULL ,if not new matrix is introduced
## Calculates and returns the inverse of matrix to parent environment

cacheSolve<-function(x,...){
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data.")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setmatrix(m)
  m
}



# Testing the object 'makeCacheMatrix' and 'cacheSolve

# Creating square matrix
## sample<-matrix(c(2,3,4,1),nrow=2,ncol=2,byrow=TRUE)

# Printing sample
## sample
##        [,1] [,2]
## [1,]    2    3
## [2,]    4    1

#Printing inverse of sample required later while comparing cacheSolve.
##solve(sample)
##      [,1] [,2]
##[1,] -0.1  0.3
##[2,]  0.4 -0.2

# Creating 'testmatrix'object taking 'makeCacheMatrix' with an input 'sample'
## testmatrix<-makeCacheMatrix(sample)

# Testing if 'testmatrix' includes all the functions within 'makeCacheMatrix'
##testmatrix$get()
##       [,1] [,2]
##[1,]    2    3
##[2,]    4    1

## testmatrix$getmatrix() 
# (First run)
## NULL

# setting the sample matrix
# testmatrix$set(sample)

# cacheSolve(testmatrix) 
# ( the results matches to solve(sample) above)
##      [,1] [,2]
## [1,] -0.1  0.3
## [2,]  0.4 -0.2

# testmatrix$getmatrix()
#( the results matches to solve(sample)above,Second run)
##      [,1] [,2]
## [1,] -0.1  0.3
## [2,]  0.4 -0.2


