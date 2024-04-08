library(readxl)
library(dplyr)
library(corrplot)
library(car)
library(adespatial)
library(tidyverse)
library(vegan)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(vegan)


# Ouvrir les fichiers de donnees ####

sp_traits <- read_excel("Bergeron.A.(2018).Montreal.flora.patch.quadrat.understory.traits.509X8.xlsx")

sp_patch <- read_excel("Bergeron.A.(2018).Montreal.flora.patch.understory.abundance.50X509.xlsx")

sp_quadrat <- read_excel("Bergeron.A.(2018).Montreal.flora.quadrat.understory.abundance.431X509.xlsx")

broad.filters <- read_excel("Bergeron.A.(2018).Montreal.flora.patch.environment.50X22.xls")

fine.filters <- read_excel("Bergeron.A.(2018).Montreal.flora.quadrat.environment.431X19.xls")


broad.filters$Stone    <- as.factor(broad.filters$Stone)
broad.filters$Building <- rowSums(broad.filters[,18:20])
broad.filters <- select(broad.filters, -c("sites","X","Y","Limestone","LowBuilt", "ModeBuilt","HighBuilt",
                                          "Dolostone","Schist","Sandstone","Water"))

fine.filters <- select(fine.filters, -c("PlotCode","X", "Y","ShrubCov"))
str(broad.filters)
plot(broad.filters$Stone)
# déplacer une colonne à la fin
broad.filters <- broad.filters  %>% relocate(Stone, .after = last_col())
broad.filters$Stone


str(fine.filters)
colnames(fine.filters) <- c("MoyenneCanopée", "SurfaceBasale", "Couvertherbacée", "Pierrosité", "SubstratSupérieur",
                            "pH", "CE", "Humidité", "NbRelief", "Coupe", "VandOrdures", "Murets","Castor","Déracinés",
                            "Prox50mlisières","Prox30mOuverture")

str(broad.filters)
colnames(broad.filters) <- c("Aire", "RPA", "Pente", "IRS","Ouverture", "NbDecCov82%","DistFPP","Verdure","UHI",
                             "PopDensité","Imperméable","Rural","Pierre" )

