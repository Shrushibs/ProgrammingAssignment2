
##This function creates a special matrix that can cache its inverse.
makeCacheMatrix<-function(x=matrix()){
	inv<-NULL
	set<-function(y){
	x<<-y
	inv<<-NULL
}
	get<-function()x
	setInv<-function(inverse) inv<<-inverse  ## set inverse
	getInv<-function() inv				## get Inverse
	list(set=set, get=get, setInv=setInv,getInv=getInv)
	
}
## This function returns inverse for the special matrix created.
## If the inverse is already calculated, then the cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve<-function(x,...){
	inv<-x$getInv()
	if (!is.null(inv)){
		message("getting cached data")
		return (inv)
	}
	mat<-x$get()
	inv<-solve(mat,...)
	x$setInv(inv)
	inv
}
