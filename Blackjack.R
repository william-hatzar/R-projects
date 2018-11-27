blackjack <- function(x,y){
  if(x>21 & y>21){
    x <- 0
    y <- 0
   print(x,y)
  }
  
  if(x>21){
  print(y)
  }

  if(y>21){
  print(x)
  }
  
  if((21-x)<=(21-y)){
    print(x)
  } 
  if((21-x)>=(21-y)){
   print(y) 
  } 
  
}



blackjack(18,19)