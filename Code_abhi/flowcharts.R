remotes::install_github("moodymudskipper/flow")
remotes::install_github("rkrug/plantuml")
remotes::install_github("DataKnowledge/DependenciesGraphs")

#install ghostscript, phantomjs, install Java, PlantUML and add to PATH.


library(flow)
library(plantuml)
source('01-required_functions.R')
source('02-run_functions.R')
webshot::install_phantomjs()



flow_view(run_model, engine = c("nomnoml", "plantuml")[2], out = "Diagrams/run_model_plantUML.png")

??flow_view
sessionInfo()

plantuml_update()



library(DependenciesGraphs)

??DependenciesGraphs


deps <- funDependencies(".GlobalEnv","run_model")
plot(deps)
