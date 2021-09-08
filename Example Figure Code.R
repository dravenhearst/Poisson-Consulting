#####Example Figure 1
##Simmaps of discrete trait
##Aim: Create simmaps illustrating simulated evolution of two discrete binary traits based on transition matrix, Q, extracted from Pagel test model output; also colour code the tip labels of species for which we have ambiguous data
##NOTE: x and y each represent Trait 1 and Trait 2 respectively
##NOTE 2: "N|N", "N|Y", "Y|N", "Y|Y", each represent different combinations of Trait 1 and Trait 2 (Trait 1 | Trait 2) in which Y = trait presence and N = trait absence


#####Run Pagel test with different restrictions on transition rate matrix and then run simmaps based on Q output from Pagel tests
fit.Trait1Trait2<-fitPagel(pruned.turtles, x, y)
fit.Trait1<-fitPagel(pruned.turtles, x, y, dep.var="x")
fit.Trait2<-fitPagel(pruned.turtles, x, y, dep.var="y")

#INTERDEPENDENT MODEL (Trait 1 and Trait 2 evolution are interdependent)
fit.Trait1Trait2
plot(fit.Trait1Trait2)

#Check that names of x = y 
all(names(x)==names(y))
#Make combined tip states of both x + y in format that is returned by Pagel tests
combinedstates <- paste(x,y, sep="|")
names(combinedstates) <- names(x)

#extract transition matrix from this model
Q <- fit.Trait1Trait2$dependent.Q
Qnull <-fit.Trait1Trait2$independent.Q

#Plot simmap
plotsim <- make.simmap(pruned.turtles, combinedstates, Q = Q , nsim=1)
x<-setNames(core_data$Ambiguous, core_data$New.Phylo.Species)
par(fg="transparent")
plotSimmap(plotsim, fsize=0.2, lwd= 0.7, colors=setNames(c("sienna4","sienna2", "darkolivegreen3", "darkolivegreen4"), c("N|N", "N|Y", "Y|N", "Y|Y")), ylim = c(-1,length(plotsim$tip.label)))
lastPP<-get("last_plot.phylo",env=.PlotPhyloEnv)
ss<-sort(unique(x))
colors<-setNames(c("red", "black"),ss)
par(fg="black")
add.simmap.legend(colors=colors,vertical=FALSE,x=0.25,
                  y=-1,prompt=FALSE)
colors<-sapply(x,function(x,y) y[which(names(y)==x)],
               y=colors)
tt<-gsub("_"," ",plotsim$tip.label)
text(lastPP$xx[1:length(tt)],lastPP$yy[1:length(tt)],
     tt,cex=0.20,col=colors,pos=4,offset=0.2,font=1)
add.simmap.legend(colors=setNames(c("sienna4","sienna2", "darkolivegreen3", "darkolivegreen4"), c("N|N", "N|Y", "Y|N", "Y|Y")),prompt=FALSE,
                  x=0,y=220,fsize=1)

#####Example Figure 2
##Comparison of two discrete traits
##Aim: To visually compare two binary traits on identical phylogenetic trees (Y = trait presence, N = trait absence)

# Make new object that removes species from dataset that are not in the phylogeny (if there are any)
# Note, if there aren't any, it won't remove anything
smalldata <- core_data[core_data$New.Phylo.Species%in%pruned.turtles$tip.label, ]
name.check(pruned.turtles, smalldata, data.names=smalldata$Species)
all(pruned.turtles$tip.label %in% names(smalldata$New.Phylo.Species))
all(names(smalldata$New.Phylo.Species) %in% pruned.turtles$tip.label)
length(smalldata$New.Phylo.Species) == Ntip(pruned.turtles)

#Test out our pruned tree
OurTurtles <- pruned.turtles$tip.label
pruned.turtles <- keep.tip(turtletree, OurTurtles)
plot(cophylo(pnastree, pruned.turtles, rotate=FALSE),
     fsize=c(0.2,0.2), link.type="curved")

#Trim and sort data to match tree
rownames(core_data) <- core_data$New.Phylo.Species
core_data <- core_data[pruned.turtles$tip.label,]

#set object names for barbel data and bottom-dwelling data
x <-setNames(core_data$Trait1,rownames(core_data))
y <-setNames(core_data$Trait2,rownames(core_data))

#visualize this data on two trees
par(mfrow=c(1,2))
plot(pruned.turtles,show.tip.label=FALSE,no.margin=TRUE)
par(fg="transparent")
tiplabels(pie=to.matrix(x[pruned.turtles$tip.label],c("Y","N")), piecol=c("blue","red"),
          cex=0.3)
par(fg="black")
add.simmap.legend(colors=setNames(c("blue","red"),c("Y", "N")),prompt=FALSE,
                  x=0,y=10,fsize=1)
par(fg="transparent")
plot(pruned.turtles,show.tip.label=FALSE,no.margin=TRUE,direction="leftwards")
tiplabels(pie=to.matrix(y[pruned.turtles$tip.label],c("Y", "N")),piecol=c("blue", "red"),
          cex=0.3)
par(fg="black")

#####Example Figure 3
##Turtle phylogeny in which each colour represents a specific discrete trait
##Aim: To illustrate the turtles for which we have data for specific discrete traits, and which turtles are lacking data

plotTree(turtletree, lwd=1.25,ftype="off",fsize=0.2)
par(fg="transparent")
cols1 <- setNames(c("transparent", "darkolivegreen4"), 1:2)
plot(trait1,color = cols1, split.vertical=TRUE, add=TRUE,lwd=1.10,
     ftype="off",fsize=0.2)
par(fg="black")
cols <- setNames(c("transparent", "darkorange3"), 1:2)
#Now plot the paintedBF overtop
plot(trait2, color=cols, split.vertical=TRUE, add=TRUE, lwd=0.5,
     ftype="off", fsize=0.2)





