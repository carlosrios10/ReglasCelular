### create a classifier
ClasificadorReglas <- function(classLabels, data, 
                            measures = c("support", "confidence", "lift"), 
                            parameter = NULL, control = NULL) {
    
    ### IDEA: Maybe we should only focus on rules for the minority classes
    
    ### create appearance: classLabels can only be in rhs
    ap <- as(list(rhs=classLabels,
                  lhs=setdiff(itemLabels(data), classLabels),
                  default="none", 
                  labels=itemLabels(data)), "APappearance")
    
    ### rule base
    rules <- apriori(data, parameter=parameter, appearance=ap, control=control)
    
    ### hay que filtrar las mejores reglas.
    quality(rules)<-cbind(quality(rules),isClosed = is.closed(generatingItemsets(rules)))
    quality(rules)<-cbind(quality(rules),size=size(rules))
    #rules<-addInterestMeasure(rules,data)
    rules<-filterRules(rules)
    
    ### default class
    defaultClass <- "NA"
    
    new("ClasificadorReglas", rules=rules, 
        defaultClass=defaultClass,
        classLabels=classLabels
    )
}
###Elimina todas las reglas que tienen un lfit < 1.5
###y ademas elimiman los superset de un itemset.
filterRules<-function(rules){
    cat("Filtrando Reglas..")
    rules<-subset(rules,isClosed==TRUE & lift>1.5)
    indice<-1
    max<-length(rules)
    finalRules<- new("rules")
    while(indice<=max){
        tt<-is.subset(rules[indice],rules)
        m<-rules[tt]
        m<-m[order(quality(m)$confidence,decreasing=F)]
        regla<-m[1]
        finalRules<-union(finalRules,regla)
        rules<-rules[!tt]
        max<-length(rules)
        }
    cat("Fin-Filtrando Reglas")
    return(finalRules)
    
}

### predict for itemsets
setMethod("predict", signature(object = "ClasificadorReglas"),
          function(object, newdata, ...) {
              cat("Calculando Prediccion..")
              res <- vector("character", length=length(newdata))    
              reglasElegidas<-vector("character", length=length(newdata))    
              for(i in 1:length(newdata)) {
                  ss <- is.subset(lhs(object@rules), newdata[i,])
                  if(sum(ss)==0) res[i] <- object@defaultClass
                  else {
                      w <- which(ss)
                      rulesMatch<-object@rules[w]
                      order<-order(quality(rulesMatch)$confidence,decreasing=TRUE)
                      sortedRules<- rulesMatch[order]
                      rhs <- as(rhs(sortedRules[1]), "list")
                      res[i] <- rhs[[1]]
                      reglasElegidas[i]<-labels(sortedRules[1])
                                           
                  }

              }
              cat("Fin-Calculando Prediccion")
              res[(res=="NA")]<-NA
              match<-(res==unlist(as(newdata[,classLabels], "list")))
             return(data.frame(res,match,reglasElegidas))
          })

confusion <- function(pred, test, model) {
    pred<-factor(pred, levels=classLabels, ordered=T)
    table(unlist(as(test[,model@classLabels], "list")), pred)
}

### accuracy for multi-class classification
accuracy <- function(pred, test, model) {
    t <- confusion(pred, test, model)
    sum(diag(t))/sum(t)
}
#### coverage
coverage<-function(celularTest,pred){
    
    (nrow(celularTest)-sum(is.na(pred)))/nrow(celularTest)    
}
##calcular medidas de interes
addInterestMeasure<-function(rules,data){
    quality(rules)<-cbind(quality(rules),chiSquare = interestMeasure(x=rules,method="chiSquare",transactions=data))
    quality(rules)<-cbind(quality(rules),conviction = interestMeasure(x=rules,method="conviction",transactions=data))
    quality(rules)<-cbind(quality(rules),cosine = interestMeasure(x=rules,method="cosine",transactions=data))
    quality(rules)<-cbind(quality(rules),coverage = interestMeasure(x=rules,method="coverage",transactions=data))
    quality(rules)<-cbind(quality(rules),doc = interestMeasure(x=rules,method="doc",transactions=data))
    quality(rules)<-cbind(quality(rules),gini = interestMeasure(x=rules,method="gini",transactions=data))
    quality(rules)<-cbind(quality(rules),hyperLift = interestMeasure(x=rules,method="hyperLift",transactions=data))
    quality(rules)<-cbind(quality(rules),hyperConfidence = interestMeasure(x=rules,method="hyperConfidence",transactions=data))
    quality(rules)<-cbind(quality(rules),fishersExactTest = interestMeasure(x=rules,method="fishersExactTest",transactions=data))
    quality(rules)<-cbind(quality(rules),improvement = interestMeasure(x=rules,method="improvement",transactions=data))
    quality(rules)<-cbind(quality(rules),leverage = interestMeasure(x=rules,method="leverage",transactions=data))
    quality(rules)<-cbind(quality(rules),oddsRatio = interestMeasure(x=rules,method="oddsRatio",transactions=data))
    quality(rules)<-cbind(quality(rules),phi = interestMeasure(x=rules,method="phi",transactions=data))
    quality(rules)<-cbind(quality(rules),RLD = interestMeasure(x=rules,method="RLD",transactions=data))
    quality(rules)<-cbind(quality(rules),oddsRatio = interestMeasure(x=rules,method="oddsRatio",transactions=data))
    
    return (rules)
}
