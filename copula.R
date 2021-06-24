countDefaults<- function(companyvalues, threshold){
  result<- companyvalues[companyvalues<threshold]
  return<-length(result)
  
}

numberofpaths<- 1000
numberofcompanies<-100
correlation<-0.2
z<- matrix(rnorm(numberofpaths*numberofcompanies, mean=0, sd=1), numberofpaths,numberofcompanies)
M<-rnorm(numberofpaths, mean=0, sd=1)
M<-rep(M,numberofcompanies)
M<-matrix(M, nrow=numberofpaths, ncol=numberofcompanies)
Companymatrix<- correlation *M + sqrt (1-correlation*correlation)*z
uu<-apply(Companymatrix, 1, countDefaults, threshold = -1.88)
