# Virtual classifier class
setClass(
    Class = "AssociativeClassifier",
    representation = representation(
        rules = "rules",
        totalRules = "integer",
        classLabels = "character",
        defaultClass = "character",
        parameters = "list",
        "VIRTUAL"
    )
)

setClass(
    Class = "ClasificadorReglas",
    contains = "AssociativeClassifier"
)
