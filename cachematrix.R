## These two functions allow for faster processing on matrix inversion

## Makes a list of functions so that when a matrix is given to the overall
## function, when inverted, the inverse is stored in cached memory

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function(){x}
  setinverse<-function(inverse){m<<-inverse}
  getinverse<-function(){m}
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Inverts a matrix, 
## saving time by first checking to see if the inversion is already in memory

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
