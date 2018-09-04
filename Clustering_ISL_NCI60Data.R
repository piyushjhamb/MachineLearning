## performaing PCA and clustering on NCI data set.

library(ISLR)
nci.labs <- NCI60$labs
nci.data <- NCI60$data


## Perform PCA:

pr.out <- prcomp(nci.data, scale. = TRUE)

Cols=function (vec ){
  cols=rainbow (length (unique (vec )))
  return (cols[as.numeric (as.factor (vec))])
  }

par(mfrow =c(1,2))
plot(pr.out$x [,1:2], col =Cols(nci.labs), pch =19,
       xlab ="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3) ], col =Cols(nci.labs), pch =19,
       xlab ="Z1",ylab="Z3")


summary(pr.out)

plot(pr.out)
 

