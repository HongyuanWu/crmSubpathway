##' CalculateDF
##'
##' @title k_clique algorithm
##' @description Calculate the difference in activity values of metabolic subpathways.
##' @param spwmatrix Metabolic subpathway activity matrix (the result of function `SubpathwayMatrix`),
##' @param smaple_class Sample phenotype vector in metabolic subpathway activity matrix.
##' @param casename Disease sample phenotype label.
##' @param controlname Control sample phenotype label.
##' @examples
##' library(limma)
##' # Get the metabolic subpathway matrix.
##' Spwmatrix<-get("Spwmatrix")
##' spwDF<-CalculateDF(Spwmatrix,colnames(Spwmatrix),"cancer","control")
##' @importFrom limma lmFit
##' @importFrom limma makeContrasts
##' @importFrom limma eBayes
##' @importFrom limma contrasts.fit
##' @importFrom limma topTable
##' @export
CalculateDF<-function(spwmatrix,
                      smaple_class,
                      casename="",
                      controlname=""
                      ){
  havelimma <- isPackageLoaded("limma")
  if(havelimma==FALSE){
    stop("The 'limma' library, should be loaded first")
  }
  caseindex<-which(smaple_class==casename)
  controlindex<-which(smaple_class==controlname)
  smaple_class[caseindex]<-"case"
  smaple_class[controlindex]<-"control"
  colnames(spwmatrix)<-smaple_class
  f<-factor(smaple_class,levels = c("case","control"))
  design<-model.matrix(~0+f)
  colnames(design) <- c("case","control")
  fit<-lmFit(spwmatrix,design)
  contrast.matrix<-makeContrasts(case-control,levels=design)
  fit2<-contrasts.fit(fit,contrast.matrix)
  fit2<-eBayes(fit2)
  DFresult<-topTable(fit2,coef = 1,number = Inf)
  return(DFresult)
}
