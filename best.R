### COURSERA R PROGRAMING COURSE
### program asigment 3 

datos <- read.csv("outcome-of-care-measures.csv",header= TRUE, stringsAsFactors= FALSE )

best <- function(state, outcome) 
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
     
     
     ## and finally i order them  (x it's the column of the selected illnes)
     
     seleccion[,x]<-as.numeric(seleccion[,x])
     
     
     final <- seleccion [order(seleccion[,x],seleccion[,2]),] 
     
     final[1,2]
     
     
}   