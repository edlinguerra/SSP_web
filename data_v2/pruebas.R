library(vegan)
library(sampling)

#prueba esponjas
data("sponges")
casos=20
n=40
sites=20


par.esp<-assempar(sponges, "counts")

sim.esp<-simdata(par.esp, 20, 40, 20)

qua.esp<-datquality(sponges, sim.esp, par.esp, "square root", "bray")

#Version lapply
system.time({

samp.esp<-sampsd(sim.esp, par.esp,
                        transformation = "square root",
                        method = "bray",
                        multi.site = TRUE,
                        n=40,
                        p.n = 10,
                        sites = 20,
                        p.s = 10,
                        k=2)

})

res.esp<-summary_ssp(samp.esp)
opt.esp<-ioptimum(res.esp, c1=5, c2=3, c3=1)

#prueba micro

data("micromollusk")
par.mic<-assempar(micromollusk, "P/A")
sim.mic<-simdata(par.mic, 20, 50, 1)

#Version lapply
system.time({

  samp.mic<-sampsd(sim.mic, par.mic,
                        transformation = "P/A",
                        method = "jaccard",
                        multi.site = FALSE,
                        n=50,
                        p.n = 30,
                        sites = 1,
                        p.s = 1,
                        k=3)

})
res.mic<-summary_ssp(samp.mic, multi.site = FALSE)
opt.mic<-ioptimum(res.mic, multi.site = FALSE,c1=5, c2=3, c3=1)
