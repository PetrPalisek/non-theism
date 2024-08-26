library(lavaan.mi)
library(restriktor)
data(HS20imps) # import a list of 20 imputed data sets

## specify CFA model from lavaan's ?cfa help page
HS.model <- '
  visual  =~ x1 + a*x2 + x3
  textual =~ x4 + b*x5 + x6
  speed   =~ x7 + c*x8 + x9
  
  abc := a*b*c
'
## fit model to 20 imputed data sets
fit <- lavaan::cfa(HS.model, data = HS20imps$imp1)

VCOV <- lavaan::lavInspect(fit, what = "vcov.def.std.all")

VCOV <- standardizedSolution.mi(fit, return.vcov = TRUE, type = "def.std.all")
x <- standardizedSolution.mi(fit, return.vcov = TRUE, type = "def.joint.std.all")
