install.packages("ggplot2")
rm(list = ls())
gc()

library(ggplot2)



#matrix for normal sea ice year

A <- matrix(
  c(
    0, 0, 0, 0, 0, 0.581,
    0.986, 0, 0, 0, 0, 0,
    0, 0.986, 0, 0, 0, 0,
    0, 0, 0.986, 0.506, 0.379, 0.9918,
    0, 0, 0, 0.486, 0.068, 0,
    0, 0, 0, 0, 0.543, 0
  ),
  nrow=6, byrow=TRUE,
  dimnames=list(c("juveniles-2year", "juveniles-3year", "juveniles-4year", "adult    ", 
                  "adult with cub", "adult with yearling"),
                c("juveniles-2year", "juveniles-3year", 
                  "juveniles-4year", "adult    ", "adult with cub", "adult with yearling")))


#matrix for poor sea ice year

Ap <- matrix(
  c(
    0, 0, 0, 0, 0, 0.277,
    0.658, 0, 0, 0, 0, 0,
    0, 0.658, 0, 0, 0, 0,
    0, 0, 0.658, 0.537, 0.424, 0.759,
    0, 0, 0, 0.222, 0.033, 0,
    0, 0, 0, 0, 0.269, 0
  ),
  nrow=6, byrow=TRUE,
  dimnames=list(c("juveniles-2year", "juveniles-3year", "juveniles-4year", "adult    ",
                  "adult with cub", "adult with yearling"),
                c("juveniles-2year", "juveniles-3year", "juveniles-4year", "adult    ", 
                  "adult with cub", "adult with yearling")))

## dimnames define rownames and colnames for the matrix


########################################################
#code to project population size 50 years into the future
########################################################

tend<-50            # Enter the time to project into the future
replicates <-  1000
Nstartreal <- c(106, 68, 106, 461, 151, 108)
savers <- c() #we will have 1000 replicates for the total population size in year 50.  
#This vector will save all the data.  

#start the loop
for(j in 1:replicates) 
{
 Nstart <- Nstartreal
for(t in 1:tend)
  {    
  #I would like to sample a random number between 0 and 1, 
  #with an 80% probability to sample from 0-0.5 (good year) 
  #and 20% probability to sample between 0.5-1 (bad year).
  
  ## explaining: prob: defines the probability, 
  x3 <- sample(c(runif(1,0,0.5), runif(1,0.5,1)), 1, prob=c(0.8,0.2))
    if (x3<0.5)
    {
      a <-  A
    }
  else 
    { 
      a=Ap
    }
    
    nt1 <- a %*% Nstart #multiply the matrix by the vector of population size
    Nstart <- nt1 #Nstart is redefined for the next time step.
    
  }
  
  savers[j]=sum(nt1) ## save the final polar bear population at t = 50
    }


hist(savers, xlab="number polar bears at time 50", main="Histogram of stochastic projection results", 
     col="lightgreen", breaks = seq(0, max(savers) + 1000, 500))

## ggplot  
ggplot(mapping = aes(x = savers)) + 
  geom_histogram(breaks= seq(0, max(savers) + 1000, 500), fill = "lightgreen", col = "black") + 
  xlab("Number polar bears at time 50") + ggtitle("Histogram of stochastic projection results") +
  ylab("Frequency") + theme_classic()

mean(savers) #the mean number polar bears at time 50

#sort the data of number of polar bears at time 50 in ascending order
sorteddata <- sort(savers)
lowerci<-sorteddata[25] #lower confidence interval is the 25th entry in the 1000 data points
upperci<-sorteddata[975] #upper confidence interval is the 975 entry in the 1000 data points

lowerci
upperci

## Function for doing (almost) the same
quantile(savers, probs = c(0.025, 0.975))

## plot ci into plot
ggplot(mapping = aes(x = savers)) + 
  geom_histogram(breaks= seq(0, max(savers) + 1000, 500), fill = "lightgreen", col = "black") + 
  xlab("Number polar bears at time 50") + ggtitle("Histogram of stochastic projection results") +
  ylab("Frequency") + theme_classic() + 
  geom_vline(xintercept = c(quantile(savers, probs = c(0.025, 0.975)), mean(savers)))



