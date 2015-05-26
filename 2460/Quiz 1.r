diceSim <- function(n=100)
{
  totals<-rep(0,12)
  for(i in 1:n)
  {
    x<-sample(1:6,1)
    y<-sample(1:6,1)
    totals[x+y]=totals[x+y]+1
  }
  totals
}