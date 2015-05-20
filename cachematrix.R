## the overall function calculates the inverse of a matrix, if the inverse of the matrix
## already exist, the cached value is returned

## the following function takes a matrix as an input and return
## a list containing 4 functions

makeCacheMatrix <- function(x = matrix()) {
  solved_matrix<-NULL
  set<-function(y){                                   ##set function
    x<<-y
    solved_matrix<<-NULL
  }
  get<-function(){x}                                  ##get function
  set_matrix<-function(s_mat){solved_matrix<<-s_mat}  ##set_matrix function
  get_matrix<-function(){solved_matrix}               ##get_matrix function
  list(set=set, get=get,
       set_matrix=set_matrix,
       get_matrix=get_matrix)                         ##the output of the function is a
}                                                     ##list containing the functions


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  solved_matrix<-x$get_matrix()     ##use the get_matrix function on the data
  if(!is.null(solved_matrix)){      ##if the data exist (not NULL) then
    message("getting cached data")  ##write "getting cached data" in terminal
    return(solved_matrix)           ##return the solved matrix as the result of the function
  }                                 ##if the data does not exist
  data<-x$get()                     ##use the get() function to retrieve the non-solved matrix
  solved_matrix<-solve(data,...)    ##solve it
  x$set_matrix(solved_matrix)       ##cache the new result using the set_matrix() function
  solved_matrix                     ##return the solved matrix as a result of the function
}
