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
  Bin <- c(1,0,0,1,0,0,0,1,1,0,0,1,1,0)
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