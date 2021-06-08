# remotes::install_github("moodymudskipper/flow")
# remotes::install_github("rkrug/plantuml")
# remotes::install_github("DataKnowledge/DependenciesGraphs")
# webshot::install_phantomjs()
#install ghostscript, phantomjs, install Java, PlantUML and add to PATH.

#For CodeDepends, needs "graph" package. source("https://bioconductor.org/biocLite.R") biocLite("graph")
if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
}
BiocManager::install(version = "3.12")
BiocManager::install(c("graph", "Rgraphviz"))

library(flow)
library(plantuml)


source('02-run_functions.R')
source('01-required_functions.R')



flow_view(batch_run_func, engine = c("nomnoml", "plantuml")[1], out = "Diagrams/Code Reading/run_model_func_flow.png")

?flow_view

library(DependenciesGraphs)
deps <- funDependencies(".GlobalEnv","batch_run_func")
plot(deps)


library(Rgraphviz)
library(graph)
library(CodeDepends)

?readScript
?makeVariableGraph

install.packages("GGally")
library(GGally)
install.packages("network")
install.packages("sna")

library(network)
library(sna)
library(ggplot2)

install.packages("intergraph")
library(intergraph)
library(igraph)

f = c("Github Issues/a_very_long_function.R")
sc = readScript(f[1])
gg = makeVariableGraph( info = getInputs(batch_run_func))
par(bg = 'blue')
plot(gg)
# gg = layoutGraph(gg, layoutType = "circo")
# graph.par(list(nodes = list(fontsize=55)))
# renderGraph(gg) ## could also call plot directly
topgo.igraph <- graph_from_graphnel(gg,name=TRUE,weight=F,unlist.attrs=F)
net_gg <- asNetwork(topgo.igraph)
ggnet2(net_gg, color = "grey15", size = 12, label = TRUE, label.color = "white", arrow.size = 12, arrow.gap = 0.025) +
  theme(panel.background = element_rect(fill = "grey15"))
# A big/long function
info = getInputs(run_model)
dtm = getDetailedTimelines(info = info)
plot(dtm)#, var.cex = .7, mar = 4, srt = 30)

