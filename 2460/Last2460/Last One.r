#My Exponential

my.rexp<-function(times, lam)
{
  Array<-numeric()
  for(i in 1:times)
  {
    #Preset values
    tau =10
  u =runif(1,0,1)
  #Do the magic
  x = -log(1 - (1 - exp(lam * tau)) * u) / lam
  #Add it to a list we can return
  Array<-append(Array,x)
  }
  #Sort the list high to low
  Sorted<-sort(Array, decreasing = FALSE)
  #Return the sorted list
  Sorted
}
#Graph the 1/3 rate
Distro<-my.rexp(20000,1/3)
hist(Distro)
 RDistro<-rexp(20000, rate=1/3)
 RDistro<-sort(RDistro)
 hist(RDistro)
qqplot(Distro, RDistro,plot.it = TRUE)

#Graph the rate of 1
 Distro<-my.rexp(20000,1)
 hist(Distro)
RDistro<-rexp(20000, rate=1)
RDistro<-sort(RDistro)
hist(RDistro)
qqplot(Distro, RDistro,plot.it = TRUE)

#Graph the Rate of 3
Distro<-my.rexp(20000,3)
hist(Distro)
RDistro<-rexp(20000, rate=3)
RDistro<-sort(RDistro)
hist(RDistro)
qqplot(Distro, RDistro,plot.it = TRUE)

#Start my binomial function
binom<-function(num,p,choose)
{
  histdat<-numeric()
  for(i in 1:num){
    #Sample data based on probability
    histdat[i]=sum(sample(c(0,1),choose,replace=T,prob=c(1-p,p)))
  }
  #Return the array
  histdat
}
#Graph my function
BI<-binom(1000,.8,50)
hist(BI)
#Graph the built in R function
RBI<-rbinom(1000, 50, .8)
hist(RBI)

#QQplot my version vs the R version
qqplot(BI, RBI)

#Run the chi-squared test
BI<-binom(10000,.2,50)
RBI<-rbinom(10000, 50, .2)
chisq.test(BI,RBI)
chisq.test(BI)
chisq.test(RBI)


#Start the Sin-Cos Normal Distribution
normwave<-function(num)
{ 
  #Static values
  u = runif(num)
  v = runif(num)
  
  x=rep(0,num)
  y=rep(0,num)
  
  #Magic happening round 2
  for (i in 1:num){
    x[i] = sqrt(-2*log(u[i]))*cos(2*pi*v[i])
    y[i] = sqrt(-2*log(u[i]))*sin(2*pi*v[i])
  }
  #Put the magic in my array for manipulation
 mList<-x+y
}
#Plot my Graph
wavey<-normwave(10000)
hist(wavey)


#Start the transform Normal Distribtuon 
fun<-function(num)
{
  Array<-numeric()
  for(t in 1:num)
  {
    #Static values again
    rand1<-runif(1)
    rand2<-runif(1)
    #Generate the random nubers
    x = 0.5 * (rand1+ rand2)
    #Add it to the array
    Array<-append(Array,x)
  }
  #Return the whole shebang
  Array
}
#Plot the other Normal graph
rNormal<-fun(10000)
hist(rNormal)

#QQplot the Sin-Cos function
qqnorm(normwave(10000))
#QQplot the transformaton function
qqnorm(fun(10000))




