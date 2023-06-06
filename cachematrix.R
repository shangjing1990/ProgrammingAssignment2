makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(inverse) {
    inv <<- inverse
  }
  return(list(
    set = set,
    get = get,
    getinverse = getinv,
    setinverse = setinv
  ))
}
cacheSolve <- function(x,...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    return(inverse)
  }
  m <- solve(x$get())
  x$setinverse(m)
 
}
library(DiagrammeR)
nodes <- create_node_df(
  n = 10,
  type  = c(rep("m",5),rep('o',3),rep("c",2)),
  label = c("set","get","get inverse","set inverse","cache constructor","object in RAM","matrix","inverse matrix",
            "solve function","if not cached solve/save"),
  color = c(rep('red',5),rep('blue',3),rep('green',2))
)
edges <- create_edge_df(
  c(1,2,3,4,5,6,7,8,6,9,10),
  c(5,5,5,5,6,5,6,6,9,6,9),
  color = c(rep('red',5),rep('blue',4),rep('green',2)))
graph <- create_graph(
  nodes_df = nodes,
  edges_df = edges,attr_theme = "lr",directed = FALSE,graph_name = "Mind Map")

# View the graph
render_graph(graph)

I <- rbind(c(0,1,2),c(1,1,1),c(-4,2,1))
m <- makeCacheMatrix(I)
identical(m$get(),I)

m$getinverse()
cacheSolve(m)
## now cacheSolve found out that the inverse in the object is NULL (<<- going one level up) 
## and replace the NULL value with the inverse of the matrix

m$set(rbind(sqrt(c(2,2)),c(-1,2)))
## notice that every time we enter a new matrix the inverse is turned to NULL with the <<- operator that 
## changes the m object's inverse value in the parent frame

