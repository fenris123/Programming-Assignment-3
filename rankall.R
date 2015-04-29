### COURSERA R PROGRAMING COURSE
### program asigment 3 

datos <- read.csv("outcome-of-care-measures.csv",header= TRUE, stringsAsFactors= FALSE )

rankall <- function(outcome,num = "best") 

{      
  
     
     
     
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
  
  
  
  
  
  ### this will ensure that the numbers of the illnes's column will be numbers
  ### otherwhise there will be problems between one digit and two digits numbers
  
  
  datos[,x]<-as.numeric (datos[,x])   
                                     
  
  
  
  
  
  
  #### from here I made a loop that takes one state in every cicle
  
  
  
  
  
  
  
  final <- c()                      ##this create an empty vector, and i will add the datas usinRbind   
    
  states <- c(unique (datos[,7]))   ##create a vector with al the states
  
  
  
  
  for (i in 1:length(states)){
    
         
      selectedstate <- states[i]                            ##selet the data of one diferent state every cicle
      refined <- datos[which(datos[,7] == (states[i])),]      
      
           
    
  
  
  
  
  ## here i order them by illnes (x it's the column of the selected illnes), and remove NA.
  
  
  refined[,x]<-as.numeric (refined[,x])                                       
  
  seleccion <- refined[order(refined[,x],refined[,2]),] 
  
  seleccion<- seleccion[which(complete.cases(seleccion[,x])==TRUE),]
  
  
  
  
  ## here i select the hospital rank given in the function.
  
  h <- num
  if (h == "best")                                ##if num = "best", print it
  { selectedhospital <- seleccion[1,2]}
  
  
  else{
    
    if (h == "worst")                ##if num =  "worst", print it
    {selectedhospital <-seleccion[nrow(seleccion),2]}
    
    
    else                     ## print whatever other num.
      
    {selectedhospital <- seleccion[h,2]}
     
    }
  
  
  ## here i take the hospital and the state of each cicle, make a vector of both, and add a vector as a new row using rbind
  
     combined <- c(selectedhospital,states[i])  
     final<- rbind(final,combined)
  }
  
  
  ## here i just take the final result and coherece it into a data.frame
  ## I order it by alphabetical state order, and add names.
 
  
  
 final <- as.data.frame(final)
 final <- final[order(final[,2]),]
 
 rownames(final) <- states
 colnames(final) <- c("hospital","state") 
 
 
 
 final
}   