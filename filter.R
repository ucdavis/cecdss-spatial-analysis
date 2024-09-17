source("index.R")

library(terra)
library(data.table)

filter_GLRBT = function(completed_GLRBT) {
names(GLRBT)[1:3] = c("cluster_no","lng","lat")

GLRBT$treatmentid = rep(1:13, length.out = nrow(GLRBT))

GLRBT_filtered=GLRBT[!(GLRBT$Stem6to9_tonsAcre==0&GLRBT$Stem4to6_tonsAcre==0&GLRBT$Stem9Plus_tonsAcre==0&GLRBT$Branch_tonsAcre==0&GLRBT$Foliage_tonsAcre==0), ]

return (GLRBT_filtered)
}