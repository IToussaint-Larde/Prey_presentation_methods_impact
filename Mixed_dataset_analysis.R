# ------------------------------------------------------------------------------
# PCA and diparity analysis on the all data to test the impact of tweezer
# ------------------------------------------------------------------------------




# ------------------------------------------------------------------------------
# PART I: DATA PREPARATION
# ------------------------------------------------------------------------------


###OPEN FILE ("Mixed_dataset.csv")
data_mixed<-read.csv(file.choose(), header= T, sep=";", dec=".")

###CONVERT SPEED IN ABSOLUTE VALUE (MSGC, MSTR, MSEhd1, MSEhd2)
position_column_MSGC<- which(colnames(data_mixed) == 'MSGC')#to find the position of column 'MSGC' in the data frame
position_column_MSEhd1<- which(colnames(data_mixed) == 'MSEhd1')
position_column_MSEhd2<- which(colnames(data_mixed) == 'MSEhd2')
columns_to_be_converted_in_absolute_value<-c(position_column_MSGC,position_column_MSEhd1,position_column_MSEhd2)
data_mixed[,columns_to_be_converted_in_absolute_value]<-abs(data_mixed[,columns_to_be_converted_in_absolute_value])

###NORMALIZE THE DATA
# Apply log10 only on numerical columns
data_mixed_prepared<-data_mixed
data_mixed_prepared[] <- lapply(data_mixed_prepared, function(x) {
  if (is.numeric(x)) {
    log10(x)  # Apply log10 only when it is a numerical column
  } else {
    x  # else, keep the column unchanged
  }
})







# ------------------------------------------------------------------------------
# STEP 2 : PCA Principal Componant analysis to reduce the number of variable
# ------------------------------------------------------------------------------


###LIBRARY
library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)
library(ggplot2)
library(ggalt)
library(factoextra)
library(patchwork)
library(dplyr)


### PCA
pca_result <- prcomp(data_mixed_prepared[, 5:29], scale. = TRUE)
summary(pca_result)# Visualize variance explained by each componant


# Scree plot
plot(pca_result, type = "l", main = "Scree Plot")
abline(h = 1, col = "red", lty = 2)# line to find standard deviation = 1
#threshold: Kaiser-Guttman criterion used to decide how many PC axes to keep

#Scree plot with the percentage of explained variances
fviz_eig(pca_result , addlabels = TRUE)


# Plot of cumulative variance explained
cumvar <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
plot(cumvar, type = "b", main = "Cumulative Variance Explained", xlab = "Number of Components", ylab = "Cumulative Proportion of Variance")
abline(h = 0.8, col = "blue", lty = 2)  # Line for 80%


#Contribution of the variables to four first PC axis which cover 80% of the variance 
#The dotted line corresponds to an “expected average” contribution threshold value. It is a kind of neutral reference that allows visually identify which variables contribute more than average to the selected axis.
fviz_contrib(pca_result, choice = "var", axes = c(1))#MADhd1, TMhd1, MADhd2,TMhd2, MAEhd1, MAEhd2, PCD, Dhd2, MSDhd1, TMG, DG, Dhd1, MAGC, MSDhd2
fviz_contrib(pca_result, choice = "var", axes = c(2))#MGA, MSGC, MG, MSGO, Mhd2, MSEhd2, MSEhd1, TMG
fviz_contrib(pca_result, choice = "var", axes = c(3))
fviz_contrib(pca_result, choice = "var", axes = c(4))


#Supplementary figure S2: Plot of the contribution of the variables to PC1 and PC2 
library(patchwork)
# Create graphs
plot1 <- fviz_contrib(pca_result, choice = "var", axes = 1, lwd = 1.5) +
  theme(axis.text.x = element_text(size = 12))
plot2 <- fviz_contrib(pca_result, choice = "var", axes = 2, lwd = 1.5) +
  theme(axis.text.x = element_text(size = 12))
# Display one below the other
plot1 / plot2


#add column to pca_data
pca_data<-as.data.frame(pca_result$x)
pca_data$tweezer <- data_mixed_prepared$tweezer
pca_data$species <- data_mixed_prepared$species
pca_data$ind <- data_mixed_prepared$ind
pca_data$medium <- data_mixed_prepared$medium



### Loading plot
fviz_pca_var(pca_result, col.var = "black")


###Figure 4: Visualisation of PCA with hull convex

palette_colors <- c( "#66FF00", "#336600", "#FFCC00","#993300", "#FF0000","#00FFFF","#006699", "#0000CC","#CC99FF","#000000" )  # Sélectionner 'n' couleurs aléatoires

### Loading plot
plot_var <- fviz_pca_var(
  pca_result,
  #col.var = "contrib",                         #Color according to contribution
  #gradient.cols = c("blue", "orange", "red"),  # color gradient
  repel = TRUE,                                 # Avoid label overlaps
  #arrowsize = 0.8,                             # Arrow thickness 
  labelsize = 6                                 # Label text size
)
#plot_var


###Visualisation of PCA with hull convex
hulls <- pca_data %>%
  group_by(medium) %>%
  slice(chull(PC1, PC2))

plot_ind <- ggplot(pca_data, aes(x = PC1, y = PC2, color = ind, shape = tweezer)) +
  geom_point(size = 5) +
  geom_polygon(data = hulls, aes(x = PC1, y = PC2, fill = medium, group = medium),
               alpha = 0.2, color = NA) +
  scale_color_manual(values = palette_colors) +
  labs(title = "PCA - Axes 1 et 2", x = "PC1", y = "PC2") +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  )

# 3. Afficher l’un au-dessus de l’autre
plot_var + plot_ind






###superposition Loading plot and PCA

#fviz_pca_biplot(
#  pca_result,
#  label = "var",              # to display only the vectors of the variables
#  habillage = pca_data$medium,  
#  col.var = "black",
#  repel = TRUE,               # Avoid label overlaps
#  addEllipses = TRUE          # add ellipse
#)




# ------------------------------------------------------------------------------
# PART II: DISPARITY
# ------------------------------------------------------------------------------


### PCA data
scores <- as.data.frame(pca_result$x)  # individuals coordinates
scores$group <- data_mixed_prepared$medium


### library and package
library(dplyr)
install.packages("dispRity")
library(dispRity)


d_disp <- data_mixed_prepared[, -c(1:4, 30)]#removing the 4 first columns
row.names(d_disp) <- data_mixed_prepared[, 1]#row name is given by the first column

group_disp <- data.frame(
  row.names = data_mixed_prepared[, 1],
  medium = data_mixed_prepared[, 4]
)


# disparity for medium
results<-dispRity.per.group(data = d_disp,
                   group = group_disp,
                   metric = c(sum, variances))
results


# post-hoc test
test_disp <- test.dispRity(results,
                           test = wilcox.test,
                           comparisons = "pairwise",
                           correction = "bonferroni")
test_disp


### Figure5
disp_obs <- get.disparity(results)#Extract the disparity results from results.
disp_obs
plot(results)



# ------------------------------------------------------------------------------
# PART III: TESTING TWEEZER AND MEDIUM IMPACT ON AXIS 1 OF THE PCA
# ------------------------------------------------------------------------------

# Simple anova model on PC1 axis
# Load required package
library(car)

# Fit the model using aov
anova_model <- aov(PC1 ~ tweezer * medium, data = pca_data)

# Perform Type II ANOVA
Anova(anova_model, type = 2)

#Anova Table (Type II tests)
#Response: PC1
#                Sum Sq  Df  F value Pr(>F) 
#tweezer           0.31   1   0.115 0.7352 
#medium         1153.35   1 429.638 <2e-16 ***
#tweezer:medium    0.27   1   0.099 0.7537
#Residuals       271.13 10 

#The effect of the clamp is clearly erased in front of the effect of the medium.


# Simple anova model on PC1 axis
# Load required package
library(car)

# Fit the model using aov
anova_model2 <- aov(PC2 ~ tweezer * medium, data = pca_data)

# Perform Type II ANOVA
Anova(anova_model2, type = 2)

#Anova Table (Type II tests)
#Response: PC2
#               Sum Sq  Df F value   Pr(>F)   
#tweezer         12.32   1  3.5823 0.061260 .    
#medium          39.79   1 11.5667 0.000963 *** 
#tweezer:medium   4.66   1  1.3556 0.247047
#Residuals      347.46 101


#Variables contributing beyond the threshold to axis 1
#MADhd1, TMhd1, MADhd2, TMhd2, PCD, MAEhd1, MAEhd2, TMG, Dhd2,TMGA, DG, MSDhd1, Dhd1, MAGC, MSDhd2

vars_PC1 <- c("MADhd1", "TMhd1", "MADhd2", "TMhd2", "PCD", "MAEhd1", 
              "MAEhd2", "TMG", "Dhd2", "DG", "MSDhd1", 
              "Dhd1", "MAGC", "MSDhd2")

results <- lapply(vars_PC1, function(var) {
  formula <- as.formula(paste(var, "~ medium"))
  result <- summary(aov(formula, data = data_mixed_prepared))
  list(variable = var, result = result)
})

# Quick display
for (res in results) {
  cat("Variable:", res$variable, "\n")
  print(res$result)
  cat("\n")
}

#test the effect of the medium on each variable contributing significantly to axis 1 separately

#Variable: MADhd1 = MADhd1
#             Df Sum Sq Mean Sq F value Pr(>F) 
#medium        1 18.657  18.657   298.3 <2e-16 ***
#Residuals   103  6.443   0.063   

#Variable: TMhd1
#             Df Sum Sq Mean Sq F value Pr(>F)  
#medium        1 15.049  15.049   296.3 <2e-16 ***
#Residuals   103  5.232   0.051  

#Variable: MADhd2
#             Df Sum Sq Mean Sq F value Pr(>F) 
#medium        1 15.977  15.977     248 <2e-16 ***
#Residuals   103  6.635   0.064 

#Variable: TMhd2 
#             Df Sum Sq Mean Sq F value Pr(>F) 
#medium        1  8.357   8.357   285.6 <2e-16 ***
#Residuals   103  3.013   0.029

#Variable: PCD
#             Df Sum Sq Mean Sq F value Pr(>F)
#medium        1  9.415   9.415   356.4 <2e-16 ***
#Residuals   103  2.721   0.026 

#Variable: MAEhd1 = MAEhd1
#             Df Sum Sq Mean Sq F value Pr(>F)
#medium        1  22.82  22.823   127.6 <2e-16 ***
#Residuals   103  18.43   0.179 

#Variable: MAEhd2
#             Df Sum Sq Mean Sq F value Pr(>F) 
#medium        1  13.94  13.935   130.5 <2e-16 ***
#Residuals   103  11.00   0.107

#Variable: TMG
#             Df Sum Sq Mean Sq F value Pr(>F)  
#medium        1 10.030   10.03   979.8 <2e-16 ***
#Residuals   103  1.054    0.01

#Variable: Dhd2 
#             Df Sum Sq Mean Sq F value Pr(>F)
#medium        1  9.051   9.051   280.4 <2e-16 ***
#Residuals   103  3.325   0.032

#Variable: DG
#             Df Sum Sq Mean Sq F value Pr(>F) 
#medium        1  6.012   6.012     402 <2e-16 ***
#Residuals   103  1.540   0.015 

#Variable: MSDhd1 = MSDhd1
#             Df Sum Sq Mean Sq F value Pr(>F) 
#medium        1  4.543   4.543   111.7 <2e-16 ***
#Residuals   103  4.188   0.041

#Variable: Dhd1
#             Df Sum Sq Mean Sq F value Pr(>F)
#medium        1  7.778   7.778   193.4 <2e-16 ***
#Residuals   103  4.143   0.040 

#Variable: MAGC
#             Df Sum Sq Mean Sq F value   Pr(>F) 
#medium        1  6.577   6.577   80.48 1.43e-14 ***
#Residuals   103  8.417   0.082    

#Variable: MSDhd2 
#             Df Sum Sq Mean Sq F value Pr(>F)
#medium        1  2.927  2.9271    61.7  4e-12 ***
#Residuals   103  4.886  0.0474




# Simple anova model on PC2 axis
anova_result_PC2 <- aov(PC2 ~ tweezer * medium, data = pca_data)
summary(anova_result_PC2)
#                Df Sum Sq Mean Sq F value  Pr(>F)  
#tweezer          1   12.4   12.41   3.391 0.06848 .
#medium           1   34.7   34.67   9.470 0.00269 **
#tweezer:medium   1    5.3    5.32   1.452 0.23103
#Residuals      101  369.7    3.66

anova_result_PC2_bis <- aov(PC2 ~ tweezer + medium, data = pca_data)
summary(anova_result_PC2_bis)
#             Df Sum Sq Mean Sq F value  Pr(>F)
#tweezer       1   12.4   12.41   3.376 0.06905 .
#medium        1   34.7   34.67   9.429 0.00274 **
#Residuals   102  375.0    3.68 

#Variables contribuant au-delà du treshold à l'axe 2
#MGA, MSGC, MG, MSGO, Mhd2, MSEhd2, MSEhd1

vars_PC2 <- c("MGA", "MSGC", "MG", "MSGO", "Mhd2",  
              "MSEhd2", "MSEhd1")

results <- lapply(vars_PC2, function(var) {
  formula <- as.formula(paste(var, "~ medium"))
  result <- summary(aov(formula, data = data_mixed_prepared))
  list(variable = var, result = result)
})

# quick display
for (res in results) {
  cat("Variable:", res$variable, "\n")
  print(res$result)
  cat("\n")
}


#Variable: MGA
#             Df Sum Sq Mean Sq F value  Pr(>F)  
#medium        1 0.9489  0.9489   78.25 2.7e-14 ***
#Residuals   103 1.2490  0.0121 

ggboxplot(data_mixed_prepared, x = "medium", y = "MGA", 
          color = "medium", palette = c("#003366", "#FF6600"),
          add = "jitter", 
          add.params = list(shape = "ind")) +
  theme(
    legend.position = "right",   
    legend.box = "vertical",     
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")   
  ) +
  guides(
    shape = guide_legend(title = "Individual"),   
    color = guide_legend(title = "Condition medium")   
  )



#Variable: MSGC
#             Df Sum Sq Mean Sq F value Pr(>F)
#medium        1 0.0154 0.01541   0.574   0.45
#Residuals   103 2.7639 0.02683


#Variable: MG
#             Df Sum Sq Mean Sq F value Pr(>F)
#medium        1  3.949   3.949   281.9 <2e-16 ***
#Residuals   103  1.443   0.014  

ggboxplot(data_mixed_prepared, x = "medium", y = "MG", 
          color = "medium", palette = c("#003366", "#FF6600"),
          add = "jitter", 
          add.params = list(shape = "ind")) +
  theme(
    legend.position = "right",   
    legend.box = "vertical",     
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")   
  ) +
  guides(
    shape = guide_legend(title = "Individual"),   
    color = guide_legend(title = "Condition medium")   
  )


#Variable: MSGO 
#             Df Sum Sq Mean Sq F value Pr(>F)
#medium        1 0.0297 0.02969   2.258  0.136
#Residuals   103 1.3541 0.01315



#Variable: Mhd2 
#             Df Sum Sq Mean Sq F value Pr(>F)
#medium        1 0.0327 0.03267   1.361  0.246
#Residuals   103 2.4720 0.02400 



#Variable: MSEhd2
#             Df Sum Sq Mean Sq F value   Pr(>F) 
#medium        1  1.260  1.2599   15.04 0.000185 ***
#Residuals   103  8.625  0.0837 

ggboxplot(data_mixed_prepared, x = "medium", y = "MSEhd2", 
          color = "medium", palette = c("#003366", "#FF6600"),
          add = "jitter", 
          add.params = list(shape = "ind")) +
  theme(
    legend.position = "right",   
    legend.box = "vertical",     
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")   
  ) +
  guides(
    shape = guide_legend(title = "Individual"),   
    color = guide_legend(title = "Condition medium")   
  )

#Variable: MSEhd1 
#             Df Sum Sq Mean Sq F value   Pr(>F) 
#medium        1  3.895   3.895   27.67 7.87e-07 ***
#Residuals   103 14.500   0.141 

ggboxplot(data_mixed_prepared, x = "medium", y = "MSEhd1", 
          color = "medium", palette = c("#003366", "#FF6600"),
          add = "jitter", 
          add.params = list(shape = "ind")) +
  theme(
    legend.position = "right",   
    legend.box = "vertical",     
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")   
  ) +
  guides(
    shape = guide_legend(title = "Individual"),   
    color = guide_legend(title = "Condition medium")   
  )

