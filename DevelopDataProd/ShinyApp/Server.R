library(shiny)
packages=c("igraph", 'plyr','dplyr','magrittr')

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
      install.packages(setdiff(packages, rownames(installed.packages())))  
}

lapply(packages,library,character.only=T)

shinyServer(function(input, output) {
      
      
      output$distPlot <- renderPlot({
            
            
            
            Nd= input$Nds #number of nodes
            Lnk=input$Lnks #number of links
            
            Links=data.frame(source=base::sample(seq(1:Nd),Lnk,replace=T),
                             target=base::sample(seq(1:Nd),Lnk,replace=T)
            )
            
            NtDt=simplify(graph.data.frame(Links, directed=F),edge.attr.comb="sum")
            Comm=fastgreedy.community(NtDt)
            V(NtDt)$color=Comm$membership
            V(NtDt)$size=degree(NtDt)*5
            V(NtDt)$label.cex = 1
            
            
            layout.modular <- function(G,c){
                  nm <- length(levels(as.factor(c$membership)))
                  gr <- 2
                  while(gr^2<nm){
                        gr <- gr+1
                  }
                  i <- j <- 0
                  for(cc in levels(as.factor(c$membership))){
                        F <- delete.vertices(G,c$membership!=cc)
                        
                        F$layout <- layout.kamada.kawai(F)
                        F$layout <- layout.norm(F$layout, i,i+0.5,j,j+0.5)
                        G$layout[c$membership==cc,] <- F$layout
                        if(i==gr){
                              i <- 0
                              if(j==gr){
                                    j <- 0
                              }else{
                                    j <- j+1
                              }
                        }else{
                              i <- i+1
                        }
                  }
                  return(G$layout)
            }
            
            NtDt$layout =layout.fruchterman.reingold(NtDt)
            NtDt$layout <- layout.modular(NtDt,Comm)
            
            V(NtDt)$color <- rainbow(length(levels(as.factor(Comm$membership))))[Comm$membership]
            
            
            plot(NtDt,
                 vertex.color = adjustcolor(V(NtDt)$color, alpha.f = .5), 
                 vertex.label.color = adjustcolor("black", .5))
            
      })
})
