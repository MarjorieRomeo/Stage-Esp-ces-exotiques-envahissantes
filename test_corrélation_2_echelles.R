# Reprendre le script nettoyage donnees
# test corrélation variables quantitatives #### 
Cor_broad <-  cor(broad.filters[,1:13])
Cor_broad
Cor_fine  <-  cor(fine.filters[,1:7])

# afficher résultat test de corrélation
par(cex =0.7)
corrplot(Cor_broad, method="color",
         type="upper", 
         order="hclust",
         addCoef.col = "black",
         tl.col="black", tl.srt=45,
         insig = "blank",
         diag=F)

par(cex =0.7)
corrplot(Cor_fine, method="color",
         type="upper", 
         order="hclust",
         addCoef.col = "black",
         tl.col="black", tl.srt=45,
         insig = "blank",
         diag=F)