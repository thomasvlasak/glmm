library(glmm)

#fill vector with weights all 1
i <- 1
weight <- vector()
while(i <= 150){
  weight[i] <- 1
  i <- i+1
}

#calculate with no weights
data(BoothHobert)
set.seed(1234)
mod.mcml1<-glmm(y~0+x1,list(y~0+z1),varcomps.names=c("z1"), data=BoothHobert,
                family.glmm=bernoulli.glmm,m=100,doPQL=TRUE)

#calculate with weights of all 1
set.seed(1234)
mod.mcml2<-glmm(y~0+x1,list(y~0+z1),varcomps.names=c("z1"), data=BoothHobert, weights = weight,
                family.glmm=bernoulli.glmm,m=100,doPQL=TRUE)


all.equal(mod.mcml1,mod.mcml2, check.attributes = TRUE, use.names = TRUE)

#Try it with real weights and binomial
i <- 1
while(i <= 75){
  weight[i] <- .5
  i <- i+1
}

set.seed(1234)
mod.mcml3<-glmm(y~0+x1,list(y~0+z1),varcomps.names=c("z1"), data=BoothHobert, weights = weight,
                family.glmm=bernoulli.glmm,m=100,doPQL=TRUE)

all.equal(mod.mcml1,mod.mcml3, check.attributes = TRUE, use.names = TRUE)

#Test for poisson unweighted vs real weights
set.seed(1234)
mod.mcml4<-glmm(y~0+x1,list(y~0+z1),varcomps.names=c("z1"), data=BoothHobert,
                family.glmm=poisson.glmm,m=100,doPQL=TRUE)

set.seed(1234)
mod.mcml5<-glmm(y~0+x1,list(y~0+z1),varcomps.names=c("z1"), data=BoothHobert, weights = weight,
                family.glmm=bernoulli.glmm,m=100,doPQL=TRUE)

all.equal(mod.mcml4,mod.mcml5, check.attributes = TRUE, use.names = TRUE)
