##' This is a function that prints out hello world to the console
##'
##' The function is simply for illustration.
##'
##' @param n Number of times hello world should be printed
##' @export
##'
##' @examples
##' helloWorld(10)
##'

hellowWorld = function(n){
    for(i in 1:n){
        cat("Hello World! \n")
    }
}
