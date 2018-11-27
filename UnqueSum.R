uniqueSum <- function(num1,num2,num3){
  if(num1 == num2 & num2 == num3){
    return(0)
  }else if(num1 == num2){
    amount <- num3
  }else if(num2 == num3){
    amount <- num1
  }else if(num1 == num3){
    amount <- num2
  }else{
    amount <- num1 + num2 + num3
  }
return(amount)
}
uniqueSum(1,1,1)
