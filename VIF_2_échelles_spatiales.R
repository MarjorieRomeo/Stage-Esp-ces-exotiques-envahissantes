## VIF ####
broad.filters
# echelle paysage
reg_broad_patch   <- lm(log1p(sp_patch$`rhamn catha`) ~ .,broad.filters)
vif_broad <- vif(reg_broad_patch)

# si variables quantitatives et qualitatives 
# résultats VIF en deux colonnes : prendre la colonne de droite
vif_b <- sort(vif_broad[,3])
vif_b

par(mar=c(4,9,2,2))
barplot(vif_b,
        horiz = T, 
        las = 1,
        col = "steelblue",
        xlim=c(0,8))
abline(v = 5, lwd = 3, lty = 2, col="red")
abline(v = 2, lwd = 3, lty = 2, col="green")
legend(x ="bottom", title="Légende",
       legend=c("VIF = 2", "VIF = 5"),
       col=c("green", "red"),
       lty = 2,
       lwd = 2, 
       cex = 1.5)

# echelle locale
reg_quadrat   <- lm(log1p(sp_quadrat$`rhamn catha`) ~ .,fine.filters)
str(fine.filters)

vif_quadrat <- vif(reg_quadrat)
vif_quadrat
# si variables quantitatives et qualitatives 
# résultats VIF en deux colonnes : prendre la colonne de droite
vif_b <- sort(vif_quadrat)
vif_b

par(mar=c(4,9,2,2))
barplot(vif_b,
        horiz = T, 
        las = 1,
        col = "steelblue",
        xlim=c(0,8))
abline(v = 5, lwd = 3, lty = 2, col="red")
abline(v = 2, lwd = 3, lty = 2, col="green")
legend(x ="bottom", title="Légende",
       legend=c("VIF = 2", "VIF = 5"),
       col=c("green", "red"),
       lty = 2,
       lwd = 2, 
       cex = 1.5)