parsAlpha <- function(object){
  
#This function is to create more parsimonious scale
#if You want to make the scale AS BRIEF AS POSSIBLE while keeping the scale¡¯s coefficient alpha AS HIGH AS POSSIBLE. 
#The best approach for this purpose follows. 
#If an item¡¯s value for ¡®the corrected alpha if item dropped' is equal to or higher than the alpha without dropping 
#AND its corrected item-total correlation is the lowest, drop the item. 
#Repeat this until there is no more such item(proposed by Seungmin Jahng)
#this function follow the step above.
#it will return new parsimonious scale and the result of Alpha
#and will print the list of items which are retained in the scale and the vaule of Cronbach's alpha 

#To get more accurate data, use should first reverse the negative items in the scale.
#Even though it automatically reserve the items when caculating alpha, There is still possiblity that the result would be wrong.
  
#written by Taenyun Kim(taenyun1@gmail.com)


  #Droping Alpha
  dropAlpha <- function(x, object){
    library(psych)
    new_alpha_result <- alpha(object, check.keys=TRUE)
    if ((new_alpha_result$alpha.drop$raw_alpha[x] >= new_alpha_result$total$raw_alpha) && 
        (new_alpha_result$item.stats$r.drop[x] == min(new_alpha_result$item.stats$r.drop))){
      y <- 1
    }else{
      y <- 0
    }
    return(y)
  }
  #judge when to stop dropping
  stopAlpha <- function(object){
    y <- 0
    for (i in 1:length(object)){
      dropAlpha(i,object)
      y <- y + dropAlpha(i,object)
    }
    return(y)
  }
  while (stopAlpha(object) > 0){
    i <- 1
    while (i < length(object)){
      if (dropAlpha(i, object) == 1){
        object[i] <- NULL
      }else{}
      i <- i+1
    }
  }
  alpha_result <<- alpha(object, check.keys=TRUE)
  new_scale <<- object
  result <- cat(paste("The Remaining items are:", paste(colnames(object),collapse = ",")),'\n', "Cronbach's Alpha =", round(alpha_result$total$raw_alpha, 2),'\n')
  return(result)
}


