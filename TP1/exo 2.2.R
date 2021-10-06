
ms=numeric(10000);
p=0.75; 
x=seq(-4,4,0.025);

for (j in(1:50)){
  k=j*j; 
  for (i in (1:10000)){
    sig=1/p; #ecart type 
    mu=1/p; #moyenne
    ms[i]=(mean(rexp(k,p))-mu)*sqrt(k)/sig
  }
  hist(ms, breaks=41, xlab="x-variable", xlim=c(-4,4), prob=TRUE, main=sprintf("exp curver over histograme, n = %d",k))
  curve(dnorm(x), col="darkblue", lwd=2, add=TRUE, yaxt="n")
}