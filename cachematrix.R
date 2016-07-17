makeCacheMatrix <- function(m = matrix()) {
    
    inv <-NULL
    setMatrx<-function(mat){
        m<<-mat
        inv<-NULL
    }
    getMatrix<-function() m
    
    cacheInverse<-function(temp){
        inv<<-temp
    }
    getInverse<-function(){
        if (nrow(m) != ncol(m)) {print('matrix is not square')}
        inv
    }
    
    list(setMatrix=setMatrix,getMatrix=getMatrix,cacheInverse=cacheInverse,getInverse=getInverse)
    
}


cacheSolve <- function(m) {
    inv<-m$getInverse()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    inv<-solve(m$getMatrix())
    m$cacheInverse(inv)
    inv
}
