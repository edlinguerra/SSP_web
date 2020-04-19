library(gt)
library(dplyr)
library(SSP)
library(vegan)

data(sponges)
par <- assempar(sponges, type = "counts")
sim <- simdata(par, 3, 10, 10)
qua <- datquality(sponges, sim, par, "square root", "bray")
qua
class(qua)


qua2 <- round(qua,2)
colnames(qua2) <- c("S.mean", "S.sd", "D.mean", "D.sd", "MVDmin","MVDmax") 

qua2 %>%
  mutate("Data" = c("Pilot", "Simulated")) %>% 
  select(c(7,1:6)) %>% 
  gt () %>%
  tab_header(
    title = "Quality of simulation",
    subtitle = "Comparison of pilot data with simulated data") %>% 
  tab_spanner(label = "Species", columns = matches("S.mean|S.sd")) %>%
  tab_spanner(label = "Simpson Diversity index", columns = matches("D.mean|D.sd")) %>% 
  tab_spanner(label = "Total multivariate dispersion", columns = matches("MVDmin|MVDmax")) %>% 
  cols_label(S.mean = "Mean", S.sd = "SD", "D.mean" = "Mean", "D.sd" = "SD", MVDmin = "Minima", MVDmax = "Maxima") %>%
  cols_align(align = "center") %>% 
  tab_footnote(
    footnote = "Values per sample unit.",
    locations = cells_column_labels(columns = vars(S.mean, S.sd, D.mean, D.sd))) %>% 
  tab_footnote(
    footnote = "The range applies only to simulated data.",
    locations = cells_column_labels(columns = vars(MVDmin, MVDmax)))
  
sam <- sampsd(sim, par, "square root", "bray", 5, 5, 3)

sum <- summary_ssp(sam, T)

sum %>% 
  mutate(mean = round(mean, 2), upper = round(upper, 2),
        lower = round(lower, 2), rel = round(rel, 2),
        der = round(der, 2))




start_time1 <- Sys.time()
data(sponges)
par <- assempar(sponges, type = "counts")
end_time1 <- Sys.time()

dur1<- end_time1 - start_time1
dur1
as.numeric(dur1)



x1 <- seq(from = start_time1, to = end_time1, by = 0.2)

