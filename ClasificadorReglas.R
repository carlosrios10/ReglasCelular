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
    #rules <- sortRules(rules, data, measures)
    ### hay que filtrar las mejores reglas.
    
    ### default class
    defaultClass <- "NA"
    
    new("ClasificadorReglas", rules=rules, 
        defaultClass=defaultClass,
        classLabels=classLabels
    )
}

sortRules <- function(rules, data, measures = c("support", "confidence", "lift")) {
    # Gives a data frame with all interests needed in order
    df <- as.data.frame(interestMeasure(rules, measures, data, reuse=TRUE))
    
    #Returns a vector with a list of ordered indexes
    order <- do.call(order, c(as.list(df[,measures]), decreasing=TRUE))
    
    sortedRules <- rules[order]
    return(sortedRules)
}

### predict for itemsets
setMethod("predict", signature(object = "ClasificadorReglas"),
          function(object, newdata, ...) {
            
              res <- vector("character", length=length(newdata))    
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
                  }

              }
              res[(res=="NA")]<-NA
             return(res)
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
