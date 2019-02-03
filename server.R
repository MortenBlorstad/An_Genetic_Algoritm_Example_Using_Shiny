library(shiny)
library(ggplot2)
library(lubridate)
library(DT)
library(rlist)

#Breeding function
Breed<-function(parent1, parent2, x){
  dna1 <- x[which(x$id==parent1),c("r","g","b","Size")]
  dna2 <- x[which(x$id==parent1),c("r","g","b","Size")]
  
  if(runif(1,0,1000)>5){
     S <- ifelse(runif(1,0,1)>0.5,dna1$Size, dna2$Size)
     r<- ifelse(runif(1,0,1)>0.5,dna1$r, dna2$r)
     g<- ifelse(runif(1,0,1)>0.5,dna1$g, dna2$g)
     b<- ifelse(runif(1,0,1)>0.5,dna1$b, dna2$b)
  }else{
    #mutation
    S<- sample(x$Size,1)
    r<- runif(1,0,1)
    g<- runif(1,0,1)
    b<- runif(1,0,1)
    }
  
   C <- rgb(r,g,b)
  
  return(cbind(r=r, g=g, b=b, Color=as.character(C),Size = S))
}


#Function for breeding the next generation 
BreedPopulation<- function(x){
  sortedlist <- x[order(x$Timedead),]
  NewSpawn <- list()
  
  
  for (i in 1:(nrow(sortedlist)/2)) {
    NewSpawn<- list.append(NewSpawn,Breed(sortedlist[i,"id"],sortedlist[i+1, "id"],x))
    NewSpawn<- list.append(NewSpawn,Breed(sortedlist[i+sample(1:((nrow(sortedlist)/2)-1),1),"id"],sortedlist[i, "id"],x))
  }
  
  return(data.frame(X=runif(n,1,100000), Y=runif(n,1,100000),Reduce(rbind, NewSpawn),Timedead=-1,id=1:n,stringsAsFactors = F ))
}

#Starting Data and variables
n <- 20
X= runif(n,1,100000) 
Y= runif(n,1,100000) 
r <-runif(n,0,1)
g <-runif(n,0,1)
b <-runif(n,0,1)
Color <- rgb(r,g,b)
Size <- c(15,20,25,30,35)
spawn <- data.frame(X=X, Y=Y,r=r,g=g,b=b,Color=sample(Color,n,replace = T),Size=sample(Size, n, replace=T) , Timedead = -1, id=1:n,stringsAsFactors =F)
Generation <- 0

shinyServer(function(input, output, session) {
  
  #reactive values
  Timer <- reactiveVal(Sys.time()+10) 
  Generation <- reactiveVal(0)
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(spawn)))
  rv <- reactiveValues(spawn = spawn)
  
  
  #Timer, generation and breeding
observe({
     invalidateLater(1000, session)
   Counter <-round(difftime(Timer(), Sys.time(), units='secs'))
   
   if(as.numeric(Counter)<0){
     Timer(Sys.time()+10)
     Generation(Generation()+1)
     vals$keeprows <- rep(TRUE, nrow(spawn))
     rv$spawn <- BreedPopulation(rv$spawn)
   }
   
   output$currentTime <- renderText({
     paste("Time:", (Counter))
    })
   output$Generation <- renderText({
     paste("Generation:", Generation())
   })
   
     
 }) 
  
# view underlying data (for debugging) 
# output$dd <- renderDataTable({
#   spawn <- rv$spawn
#   datatable(spawn)
# 
# 
# })


#-----------Plot-----------------
# View object and click remove clicked objects
output$plot1 <- renderPlot({
  
    spawn <- rv$spawn
  keep    <- spawn[vals$keeprows, , drop = FALSE]
  
  ggplot(keep,aes(X,Y,fill = Color, size = as.factor(Size))) +
    geom_point(shape = 23,alpha=1, show.legend = F)+
    scale_fill_manual(
      values = setNames(spawn$Color,
                        spawn$Color))+
    scale_size_manual(values= setNames(as.numeric(spawn$Size),as.numeric(spawn$Size)))+
    theme_linedraw()+
    theme_light()+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
          axis.text.x=element_blank(),axis.text.y=element_blank(),
          axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
    ylim(0,100000)+xlim(0,100000)
 
  
  
})
#-------------------------------


  
  # Track objects that are clicked and assigned time of "death" to objects
  observeEvent(input$plot1_click, {
    spawn <-rv$spawn
    res <- nearPoints(spawn, input$plot1_click, threshold =15)
    rv$spawn$Timedead[which(spawn$id == res$id)] <- difftime(Timer(), Sys.time(), units='secs')
  })
      
  observeEvent(input$plot1_click, {
    spawn <- rv$spawn
    res <- nearPoints(spawn, input$plot1_click, allRows = TRUE, threshold =15)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })

  
  
    
  })
  
