seasonal <- function(object) UseMethod("seasonal")
trend <- function(object) UseMethod("trend")
remainder <- function(object) UseMethod("remainder")

getraw <- function(object) {
   object$data$raw
}

remainder.stl3 <- function(object) {
   if(object$pars$fc.number==0) {
      object$data$remainder      
   } else {
      object$fc$remainder      
   }
}

fitted.stl3 <- function(object, ...) {
   if(object$pars$fc.number==0) {
      object$data$seasonal + object$data$trend
   } else {
      object$data$seasonal + apply(object$fc[,1:object$pars$fc.number], 1, sum)
   }
}

predict.stl3 <- function(object, ...) {
   if(object$pars$fc.number==0) {
      object$data$seasonal + object$data$trend
   } else {
      object$data$seasonal + apply(object$fc[,1:object$pars$fc.number], 1, sum)
   }
}

seasonal.stl3 <- function(object) {
   object$data$seasonal
}

trend.stl3 <- function(object) {
   object$data$trend
}

# get frequency components:
fc <- function(object, fcnum=1) {
   if(! "stl3" %in% class(object))
      stop("object not of class stl3")
   
   if(is.null(object$fc))
      stop("there are no post-trend frequency components")
   
   if(fcnum > ncol(object$fc))
      stop("there are not that many frequency components")
      
   object$fc[,fcnum]
}

# setGeneric("time")
time.stl3 <- function(x, ...) {
   if(length(x$t) == x$n) {
      x$t
   } else {
      c(1:x$n)
   }
}

# now for objects of class stl

remainder.stl <- function(object) {
   as.numeric(object$time.series[,3])
}

seasonal.stl <- function(object) {
   as.numeric(object$time.series[,1])
}

trend.stl <- function(object) {
   as.numeric(object$time.series[,2])
}

# setGeneric("time")
time.stl <- function(x, ...) {
   as.numeric(time(x$time.series))
}

predict.stl <- function(object, ...) {
   seasonal(object) + trend(object)
}

fitted.stl <- function(object, ...) {
   seasonal(object) + trend(object)
}
