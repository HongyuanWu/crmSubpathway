##' k_clique
##'
##' @title k_clique algorithm
##' @description Mining metabolic  subpathways through k_clique algorithm.
##' @param file.path Path of KGML files (XML format).
##' @param file.names A character vector of names of KGML files.
##' @param K An integer. A distance similarity parameter.
##' @examples
##' library(graph);
##' library(RBGL);
##' library(igraph);
##' library(XML);
##' file.path<-paste(system.file(package="crmSubpathway"),"/extdata/",sep="")
##' file.names<-c("hsa00010.xml","hsa00020.xml")
##' spwlist<-k_clique(file.path,file.names)
##' @export
k_clique<-function(file.path="",file.names="",K=4){
  havegraph <- isPackageLoaded("graph")
  if(havegraph==FALSE){
    stop("The 'graph' library, should be loaded first")
  }
  haveRBGL <- isPackageLoaded("RBGL")
  if(haveRBGL==FALSE){
    stop("The 'RBGL' library, should be loaded first")
  }
  haveigraph <- isPackageLoaded("igraph")
  if(haveigraph==FALSE){
    stop("The 'igraph' library, should be loaded first")
  }
  haveXML <- isPackageLoaded("XML")
  if(haveXML==FALSE){
    stop("The 'XML' library, should be loaded first")
  }
  Metabolicxml<-getPathway(file.path,file.names)
  MetabolicGraph<-getMetabolicGraph(Metabolicxml)
  MetabolicUGraph<-getUGraph(MetabolicGraph)
  metagl<-filterNode(MetabolicUGraph,nodeType=c("map"))
  simMeta<-simplifyGraph(metagl,nodeType="geneProduct")
  expMeta<-expandNode(simMeta)
  Subpathway<-getKcSubiGraph(k=K,expMeta)
  return(Subpathway)
}
