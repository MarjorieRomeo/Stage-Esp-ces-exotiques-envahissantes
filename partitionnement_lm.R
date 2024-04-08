### coefficients ###

forest      = c("Area", "PAR", "Slope.range", "SRI", "Stone")

matrix      = c("DistNFP", "Green", "UHI", "PopDensity")

history     = c('OpenMean', "NbDecCov82%", "ImperMean", "RuralMean")


### REGRESSION MODELS ####

### Rhamnus ####
(var_rhamnus<- (unique(c(forest, matrix, history))))

lm_rhamnus  <- lm(rhamnus_patch  ~ ., broad.filters[,var_rhamnus])


# coefficient frangula ~ forest
(sum_rhamnus <- summary(lm_rhamnus))

rhamnus_pval <- sum_rhamnus$coefficients[-1, 4]
rhamnus_est  <- lm_rhamnus$coefficients[-1]
rhamnus_se   <- sum_rhamnus$coefficients[-1, 2]



### frangula ####
(var_frangula<- (unique(c(forest, matrix, history))))

lm_frangula  <- lm(frangula_patch  ~ ., broad.filters[,var_frangula])


# coefficient frangula ~ forest
(sum_frangula <- summary(lm_frangula))

frangula_pval <- sum_frangula$coefficients[-1, 4]
frangula_est  <- lm_frangula$coefficients[-1]
frangula_se   <- sum_frangula$coefficients[-1, 2]



### lonicera ~ toutes variables ####
(var_lonicera <- (unique(c(forest, matrix, history))))

lm_lonicera <- lm(lonicera_patch  ~ ., broad.filters[,var_lonicera])


# coefficient frangula ~ forest
(sum_lonicera <- summary(lm_lonicera))

lonicera_pval <- sum_lonicera$coefficients[-1, 4]
lonicera_est  <- lm_lonicera$coefficients[-1]
lonicera_se   <- sum_lonicera$coefficients[-1, 2]



### SELECTED VARIABLES ####

(all_var <- (unique(c(broad.filters[,forest], broad.filters[,matrix], broad.filters[,history]))))

all_var <- all_var[order(match(all_var, c("Area", "PAR", "Slope.range", "SRI", "Stone",
                                          "DistNFP", "Green", "UHI", "PopDensity",
                                          'OpenMean', "NbDecCov82%", "ImperMean", "RuralMean")))]




### LABELS ####

labels_sig <- c("Area", "PAR", "Slope.range", "SRI", "Stone",
                "DistNFP", "Green", "UHI", "PopDensity",
                "OpenMean", "`NbDecCov82%`", "ImperMean", "RuralMean")

.expressions <- labels_sig
labs_expressions <- parse(text = .expressions )


### COMBINE REGRESSION RESULTS ####

lonicera_reg <- frangula_reg <- rhamnus_reg <- data.frame(var = all_var, est = 0, se = 0, pval = 1)

for(i in 1:length(all_var)) {
    tmp <- gsub("`", "", names(rhamnus_est)) == all_var[i]
    if(any(tmp)) {
        rhamnus_reg[i, 2] <- rhamnus_est[[which(tmp)]]
        rhamnus_reg[i, 3] <- rhamnus_se[[which(tmp)]]
        rhamnus_reg[i, 4] <- rhamnus_pval[[which(tmp)]]
    }
    
    tmp1 <- gsub("`", "", names(frangula_est)) == all_var[i]
    if(any(tmp1)) {
        frangula_reg[i, 2] <- frangula_est[[which(tmp1)]]
        frangula_reg[i, 3] <- frangula_se[[which(tmp1)]]
        frangula_reg[i, 4] <- frangula_pval[[which(tmp1)]]
    }
    
    tmp2 <- gsub("`", "", names(lonicera_est)) == all_var[i]
    if(any(tmp2)) {
        lonicera_reg[i, 2] <- lonicera_est[[which(tmp2)]]
        lonicera_reg[i, 3] <- lonicera_se[[which(tmp2)]]
        lonicera_reg[i, 4] <- lonicera_pval[[which(tmp2)]]
    }
    
}


### VARPART ####
history     = c('OpenMean', "`NbDecCov82%`", "ImperMean", "RuralMean")

form_a <- formula(paste0(" ~ ", paste(c(forest,matrix,history), collapse = "+")))
form_b <- formula(paste0(" ~ ", paste(forest, collapse = "+")))
form_c <- formula(paste0(" ~ ", paste(matrix, collapse = "+")))
form_d <- formula(paste0(" ~ ", paste(history, collapse = "+")))

### Model matrix ####

mm_a <- as.data.frame(model.matrix(form_a, broad.filters))[,-1]
mm_b <- as.data.frame(model.matrix(form_b, broad.filters))[,-1]
mm_c <- as.data.frame(model.matrix(form_c, broad.filters))[,-1]
mm_d <- as.data.frame(model.matrix(form_d, broad.filters))[,-1]

head(mm_b)
# rhamnus

vp_rhamnus <- varpart_fun(Y = rhamnus_patch,
                          x1 = mm_b,
                          x2 = mm_c,
                          x3 = mm_d)

# frangula

vp_frangula <- varpart_fun(Y = frangula_patch,
                           x1 = mm_b,
                           x2 = mm_c,
                           x3 = mm_d)

# lonicera

vp_lonicera <- varpart_fun(Y = lonicera_patch,
                           x1 = mm_b,
                           x2 = mm_c,
                           x3 = mm_d)

save(rhamnus_reg, frangula_reg, lonicera_reg,
     vp_rhamnus, vp_frangula, vp_lonicera, labs_expressions,
     file = "~/ABergeron_data_urban_forestsresult_reg.rda")

load("~/ABergeron_data_urban_forestsresult_reg.rda")