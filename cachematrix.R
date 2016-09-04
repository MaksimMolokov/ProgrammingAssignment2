#задача заключается в написании функций, которые кэшируют обратную матрицу/
# функция makeCacheMatrix. Эта функция создает специальную "матрицу" объект, который может кэшировать объект в обратный.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

# функция cacheSolve. Эта функция вычисляет обратную специальной «матрицы», возвращенного makeCacheMatrix выше. 
#Если обратное уже вычислено (а матрица не изменилась), то cachesolve должен получить обратную из кэша.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("polu4it keIIIurovannyy dannye")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}
