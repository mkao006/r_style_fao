## This script produced by Jim Maas at FAO, Rome on 18/12/2013

## Last revised on 18/12/2013

## This is a script to demonstrate the use of Parallel processing with R.  The
## motivation is that after working with several members of the ESS group at
## FAO, I realised that many people were interested in using R to do jobs that
## can be classified as "embarrassingly parallel".  What they want to accomplish
## is the ability to do data manipulation or statistical calculations in a
## looping structure where the code performs the same operations on data from
## individual countries.  This is perfect for a parallel loop, using R, a
## parallel backend programme such as message passing interface (MPI), and a R
## package such as "foreach"

## To run this little script example you will need a computer with a parallel
## backend programme on it such as "MPI" or "SNOW".  This will work on MS
## Windows but is much easier to set up and operate, and is more stable on
## Linux. This example was created with Linux, however should run on Windows or
## other systems if the correct backend software is installed.

## load the necessary libraries

library(doMPI)
library(foreach)
library(ggplot2)

## Register the small "cluster" that you want to operate as a test system on
## your PC.  The cluster will be called "cl" and I've asked to use two of the
## cores available for my little test cluster.  If you have more cores in your
## computer you can ask for a larger number here and the job will run faster.

cl <- startMPIcluster(count = 2)
registerDoMPI(cl)

## set variables to control size of data set create an empty data frame to store
## data in

row.number <- 100
column.number <- 10

## number of iterations to perform in the loop
iteration.number <- 10

## create the looping structure so it will run in parallel this tells R to run
## the loop and combine the resuls by row binding them.  The "%dopar% tells R to
## run the code in parallel using the cluster already registered

final.results <- foreach (i = 1:iteration.number, .combine = cbind) %dopar% {

## create a little data set to work with based on random variables

newdata <- data.frame (matrix ( (runif (n = (row.number * column.number), min =
                       0, max = 500) ), ncol = column.number, nrow =
                       row.number))

## take the mean of all values in our data set
mean.of.data <- mean(as.matrix(newdata))

}
## end of the foreach looop


## plot the final results
graph.data <- as.data.frame(t(final.results))

new.levels <- paste('iter', 1:10, sep=':')
graph.data$name <- factor(new.levels, levels=new.levels, ordered=T)


ggplot(graph.data, aes(x=name, y=V1, fill=name)) + geom_bar(stat='identity') +
theme(legend.position="none") + xlab("Iteration Number") +
ylab("Mean of data in data frame")

## finally, turn off the cluster
closeCluster(cl)
mpi.quit()
