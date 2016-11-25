#library(ggplot2)

singular_plot <- function(singulars){
	d <- data.frame(Singular_value = singulars,Index = seq(1,length(singulars)))
	p <- ggplot(d,aes(x=factor(d$Index),y=d$Singular_value)) + 
				geom_bar(stat="identity",width=.5,fill="#3399FF") + 
				geom_text(aes(label = scales::percent(d$Singular_value/sum(d$Singular_value)), y= d$Singular_value), size = 4.5, stat= "identity", vjust = -0.5) + 
				labs(title=paste("Scree plot of the ", length(singulars), " Singular values of W matrix",sep="")) +
				xlab("Singular value index") +
				ylab("Values") +
				theme_minimal() + 
				theme_bw() + 
				theme(plot.title = element_text(size = 20),axis.text=element_text(size=20),
								axis.title=element_text(size=20))
	plot(p)
}

