ViteRbi <- function(data=NULL, states=c(-1,0,1), normalstate=1, emiss = c(-1, 1, 0, 1, 1, 1), trans=c(0.99, 0.01, 0, 0.005, 0.99, 0.005, 0, 0.01, 0.99), ep=2, tp=3, active=T) {
	if(is.null(data)) { data(test); data = test; }
	jumpy = NULL; normalstates = vector();
	for(x in 1:length(data[,1])) { normalstates[x] = normalstate; }
	u = unique(data[,1])
	for(x in 1:length(u)) {
		d = data[data[,1]==u[x],]	
		res <- .C("ViteRbi"
				,"data" = as.double(d[,3])
				,"states" = as.double(normalstates)
				,"emissions" = as.double(emiss)
				,"transitions" = as.double(trans)
				,"dN" = as.integer(length(d[,3]))
				,"sN" = as.integer(length(states))
				,"eN" = as.integer(ep)
				,"tN" = as.integer(tp)
				,"PACKAGE" = "ViteRbi")
		jumpy = rbind(jumpy, cbind(d[,1:3], res$states))
		
		if(active) {
			par(mfrow=c(2,1))
			plot(d[,3], xlab="Index", ylab="Value", main="Data")
			plot(res$states, xlab="Index", ylab="State", main="Estimated States")
			print("hit return to continue")
			scan("")
		}
	}
	
	invisible(jumpy)
}