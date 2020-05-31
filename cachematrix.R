## Las funciones siguientes se usan para crear una matriz y la inversa en memoria caché. 

## La primera función crea una matriz especial:

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Esta función calcula la inversa de la matriz que se creo antes. Esta funcion primero verifica si la matriz es invertible y lo guarda en memoria caché.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
