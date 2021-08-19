#Scripts used to make the 2D-diagrams
#Requires output from the Mathematica file "01_Evolutionary_Model_analysis"

#Graphical choices------------------------------------------------------------------------------
#Colors chosen each strategies
un.col = "#000000" #unfeasible
bot.col = "#203BC6" #bottom
top.col = "#EBEBEB" #surface
br.col = "#CE3B28" #branching

pch.2 = 3 #2 branches
pch.3 = 24 #3 branches
pch.4 = 23 #4 branches
pch.cycle = 1 #evolutionary cycling

#Base functions--------------------------------------------------------------------------------------------------
valuePal <- colorRampPalette(c(top.col,bot.col))
lgd_ = rep(NA, 100)

make.diagram <- function(result, title){
  "creates the base 2D diagram 
  title : the title given to the figure"
  result$value.col<-1
  result$value.col[which(result$type=="Unfeasible")] = un.col
  result$value.col[which(result$type=="No ES")] = top.col
  result$esval = (result$type=="CSS") * result$ES
  result$value.col[which(result$type=="CSS")] <- valuePal(100)[as.numeric(cut(result$esval[which(result$type=="CSS")],breaks=seq(0,10,by=0.1)))]
  result$value.col[which(result$type=="Branching point")] =br.col
  
  plot(result$u, result$w,col=result$value.col, pch=19, xlab="u, strength of nutrient diffusion", ylab="w, strength of light attenuation", main=title, cex=0.52)
  
  #legend(1,0.65, legend=c("Unfeasible"), bty="n", text.col = "white")
}


#Creation of the graphics (the legend is made separately)
#with b=0, p=0--------------------------------------------------------------------------------------------------------
result <- read.csv("../data/evolution_scenario1.txt", header=TRUE, comment.char="#")
colnames(result) <- c("u","w","zmin","zmax", "ES", "type")
result$type[which(result$w==0)] = "CSS"

make.diagram(result, title="Scenario 1")
points(3,0.3,pch=20,col="purple")


#with b=1, p=0---------------------------------------------------------------------------

result <- read.csv("../data/evolution_scenario2.txt", header=TRUE, comment.char="#")
colnames(result) <- c("u","w","zmin","zmax", "ES", "type")

make.diagram(result, title="Scenario 2")
points(5.1,0.1,pch=20,col="purple")


##with b=1, p=0.5---------------------------------------------------------------------------
result <- read.csv("../data/evolution_scenario3.txt", header=TRUE, comment.char="#")
colnames(result) <- c("u","w","zmin","zmax", "ES", "type")
result[which(result$u>6),6]<-"No ES"
make.diagram(result,title= "")

points(4.5,0.15,pch=19,col="purple")


