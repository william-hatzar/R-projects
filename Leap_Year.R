leapyear <- function(year){

  if(year%%400==0 | year%%4==0 & year%%100!=0){
    return(TRUE)
  }else{
    return(FALSE)
  }
  
}

leapyear(2016)
