### COURSERA R PROGRAMING COURSE
### program asigment 3 

datos <- read.csv("outcome-of-care-measures.csv",header= TRUE, stringsAsFactors= FALSE )

rankhospital <- function(state, outcome,num ) 
{
     
     
     
     #### here i check if the state it's correct:
     
     
          
     y <-  grep(state,datos$State)           ### This give me a vector whith as many values as matches state variable has 
     ### on the State column.
     
     
     if (length(y) == 0)
          
     {  stop ("invalid state") }
     
     
     
     
     
     
     #### here i asigned to X the name of the selected column and check if outcome it's correct: 
     
     x<-0
     
     
     if (outcome == "heart attack")   
     {x<-11}    
     
     
     if (outcome == "heart failure")  
     {x<-17}           
     
     
     if (outcome == "pneumonia")      
     {x<-23}
     
     
     
     if (x == 0 ) 
          
     { stop ("invalid outcome") }          
     
     
     
     
     
     
     #### Here i select those results that mach with the state
     
     seleccion <- datos [which (datos$State == state),] 
     
     
     
     
     
     
     ## here i order them bi illnes (x it's the column of the selected illnes), and remove NA.
     
          
     seleccion[,x]<-as.numeric(seleccion[,x])          ### this will ensure that the numbers of the column will be numbers
                                                       ### otherwhise there will be problems between one digit and two digits numbers
     
     
     final <- seleccion [order(seleccion[,x],seleccion[,2]),] 
     
     final<- final[which(complete.cases(final[,x])==TRUE),]
     
     
     
     
     ## here i select the hospital given in the num.
     
     h <- num
     if (h == "best")                                ##if num = "best", print it
          { final[1,2]}
               
     
               else{
                    
                    if (h == "worst")                ##if num =  "worst", print it
                         {final[nrow(final),2]}
     
               
                              else                     ## print whatever other num.
                                   
                                   {final[h,2]}
     
     
            }
     
     
}   