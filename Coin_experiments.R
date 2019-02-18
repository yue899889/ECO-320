#Flip a coin experiment
#Define a function
#Convert head or tail problem to 0/1
library(ggplot2)
CoinGame <-  function(n) {
  seed_n <- 42# set a seed to generate same sequence of 'random' numbers
  set.seed(seed_n)
  sample(0:1,n,rep=T)
}


#Now let us draw a graph
number_seq <- c(10,100,1000,10000)
lst <- list(NaN,NaN,NaN,NaN)
p <- list(NaN,NaN,NaN,NaN)
for (i in c(1,2,3,4)){
  n <- number_seq[i]
  fhead <- 0
  for (j in seq(1,n)){
    en <- CoinGame(j)
    fhead[j] <- round((sum(en)/length(en)),4)
  }
  lst[[i]] <- data.frame(x=seq(1,n),y=fhead)

  p[[i]] <-ggplot(data=lst[[i]],aes(x=x,y=y, group=1))+
    geom_line(color="red")+ylim(0,1)+xlab('Number of Experiments')+
    ylab(paste0('Freq. of Heads with',n,'Flipps'))+
    geom_hline(yintercept=0.5,size=2, color = "pink",alpha=0.8)+
    theme_minimal()
}

library(gridExtra)

grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]])


#-------------
#Let us toss a dice for 2 times, a success is number 1 show up
#For each trial, a one can show up zero times, one time or two times
#check which seed can at least give a 2 show up
for (i in seq(0,100000000)){
  set.seed(i)
  if (2 %in% rbinom(n=10,size=2,prob=1/6)){
    print (i)
    break
  }
}

#Write out Toss Dice function

TossDice <-  function(n,size,prob) {
  seed_n <- 6# set a seed to generate same sequence of 'random' numbers
  set.seed(seed_n)
  rbinom(n=n,size=size,prob=prob)
}




#Visualize the probability of the number of one shown up in each trial
#I.e., how many zero, one, two "one"s shown up in each trial
n=10
size=2
prob=1/6
expe1 <- data.frame(x1=TossDice(n,size,prob))
ggplot(expe1,aes(x1))+
  geom_histogram(aes(y=..density..),colour='black',fill='white',binwidth = 1)+
  geom_step(stat = "ecdf",alpha=0.8,size=3,color='blue')+ylab('Probability')+
  xlab('Number of Ones')





#--------Normal Distribution
#Standard normal distribution
#Normal distribution mean 2, sd=1
ggplot(data = data.frame(x = c(-8, 8)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1),aes(color='Mean 0, sd 1')) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 2, sd = 1),aes(color='Mean 2, sd 1'))+
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 2),aes(color='Mean 0, sd 2'))+
  scale_colour_manual("Legend title", values = c("red", "blue","green"))+
  scale_y_continuous(breaks = NULL)
