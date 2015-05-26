my.rand.gen <- function(a, c, m , seed) {
  
  return (LCG<-function(n=1)
  {
    Rand<-numeric()
    if(n==-2)
    {
      Rand<-append(Rand,m)
      Rand<-append(Rand,a)
      Rand<-append(Rand,c)
    }
    if(n==-1)
    {
      Rand<-append(Rand,seed)
    }
    if(n>=0)
    {
      for(i in 1:n)
      {
        seed=((a*seed + c)%% m)
        Rand<-append(Rand,seed)
      }
    }
    Rand
  }
  )
}
LCG<-my.rand.gen(14, 39, 100, 8)
LCG(10)
LCG <- my.rand.gen(a = 15, c = 7, m = 128, seed = 13)
LCG(10)
##
##Question B
##
period<-function(n,LCG)
{
  C1<-numeric()
  C1<-append(C1,LCG(n))  
  print("Period:")
  print(length(C1[!duplicated(C1)]))
}
period(100,LCG())
##
##Question 3
##
LCG <- my.rand.gen(a = 1664525, c =10139042237, m = 2^15, seed = 22)
LCG(5)
period(100000,LCG())

##
## Question 6
##

rbinaryDigits<-function(n, rand = my.rand)
{
  Bin<-numeric()
  Rand<-numeric()
  Bin<-append(Bin,LCG(n)) 
  for(i in 1:n)
  {
    if(Bin[i]%%2==0)
    {
      Rand<-append(Rand,0)
    }
    else
    {
      Rand<-append(Rand,1)
    }
  }
  Rand
}
rbinaryDigits(40)
##
##With orignal Values
##
LCG <- my.rand.gen(a = 15, c = 7, m = 128, seed = 13)
rbinaryDigits(40)
##
## Question 4
##
sBit<-function(n){
  Bin <- c(1,0,0,1,0,0,0)
  N = length(Bin)  
  TAP = 6                          
  T = n
  for(t in 1: T) {
    addMe = (Bin[N-1] ^ Bin[TAP])
    i=N+1
    while(i>1)
    {
      Bin[i] = Bin[i-1]      
      
      Bin[0] = addMe  
      print(Bin)
      i=i-1
    }
    
    
    
  }
}
sBit(7)
FBit<-function(n){
  Bin <- c(1,0,0,0,0,0,0,0,1,0,0,0,0,0)
  N = length(Bin)  
  TAP = 9                         
  T = n
  for(t in 1: T) {
    addMe = (Bin[N-1] ^ Bin[TAP])
    i=N+1
    while(i>1)
    {
      Bin[i] = Bin[i-1]      
      
      Bin[0] = addMe  
      print(Bin)
      i=i-1
    }
    
    
    
  }
}
FBit(14)