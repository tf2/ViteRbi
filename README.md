# ViteRbi

Example basic usage:

```R
library(ViteRbi)
ViteRbi()
```
Or if really feeling interested in this change detection method, install CNsolidate and try:

```R
testData <- function(n = 2000, m = 0.001, s = 0.2, p = 0.2) {
	d = syn.genome(data.frame(1,n,m,s,p))$data
	d[,3] = d[,4]
return(d)
}
for(x in 1:500) {
	ViteRbi(testData())
}
```
