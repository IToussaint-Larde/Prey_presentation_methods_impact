# ------------------------------------------------------------------------------
# SUCTION ANALYSIS - TWEEZER AND INDIVIDUAL IMPACT
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# PART I: DATA SUCTION PREPARATION
# ------------------------------------------------------------------------------


###OPEN FILE ("aquatic_dataset.csv")
data_Suction<-read.csv(file.choose(), header= T, sep=";", dec=".")


###SIMPLIFY DATA (getting rid of some columns)
data_Suction_reduit<-data_Suction[ , ! colnames(data_Suction) %in% c("TMGA",#similaire à TMG
                                                                     "Thd1", "Thd2",#initial movement of hyoid is difficult to capture
                                                                     "T0","Tmax_gape","T0_hd1_when_manually_extracted_from_csv_file", "Tend_hd1","T0_hd2_when_manually_extracted_from_csv_file","Tend_hd2")]#variables useless for the analysis, see SCRIPT_SUCTION_data_extraction.R for definitions


###CONVERT SPEED IN ABSOLUTE VALUE (MSGC, MSTR, MSEhd1, MSEhd2)
position_column_MSGC<- which(colnames(data_Suction_reduit) == 'MSGC')#to find the position of column 'MSGC' in the data frame data_TgP_reduit
position_column_MSEhd1<- which(colnames(data_Suction_reduit) == 'MSEhd1')
position_column_MSEhd2<- which(colnames(data_Suction_reduit) == 'MSEhd2')
columns_to_be_converted_in_absolute_value<-c(position_column_MSGC,position_column_MSEhd1,position_column_MSEhd2)
data_Suction_reduit[,columns_to_be_converted_in_absolute_value]<-abs(data_Suction_reduit[,columns_to_be_converted_in_absolute_value])


###NORMALIZE THE DATA
# Apply log10 only on numerical columns
data_Suction_prepared<-data_Suction_reduit
data_Suction_prepared[] <- lapply(data_Suction_prepared, function(x) {
  if (is.numeric(x)) {
    log10(x)  # Apply log10 only when it is a numerical column
  } else {
    x  # else, keep the column unchanged
  }
})






# ------------------------------------------------------------------------------
# PART II: TESTING INDIVIDUAL AND TWEEZER IMPACT AT GLOBAL SCALE
# ------------------------------------------------------------------------------



###NB: As the number of variable exceed the number of observations (video sequences) we use mvols from package mvMORPH which permit to avoid variable reduction

install.packages("mvMORPH")
library(mvMORPH)



### TESTING TWEEZER*IND IMPACT--------------------------------------------------


###FITTING THE DATA TO A LINEAR MODEL

fit_Suction_a <- mvols(as.matrix(data_Suction_prepared[, 3:27]) ~ tweezer*ind, 
                       data = data_Suction_prepared, 
                       method = "PL-LOOCV",
                       target="Variance")
summary(fit_Suction_a)
#GIC=-1285.076


fit_Suction_b <- mvols(as.matrix(data_Suction_prepared[, 3:27]) ~ tweezer*ind, 
                       data = data_Suction_prepared, 
                       method = "LL",
                       target="Variance")
summary(fit_Suction_b)
#GIC=-1206.334

#fit_Suction_a model having the smallest GIC, we perform the manova on fit_Suction_a.

###testing mutlinormality of the data residuals
mvqqplot(fit_Suction_a)

set.seed(123) #results will be the same when the ode is relaunched

###MANOVA
results_linear_manova_fit_Suction_a <- manova.gls(fit_Suction_a, test = c("Pillai"), type=c("II"), nperm = 5000)
results_linear_manova_fit_Suction_a
#Manova detects no interaction but there is an individual effect and an impact of the tweezer.

#Type II MANOVA Tests with 5000 permutations: Pillai test statistic 
#            Test stat Pr(>Stat)
#tweezer        0.7149  0.000200 ***
#ind            1.8902  0.003799  **
#tweezer:ind    1.4128  0.882024
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



###to visualize the distribution of the data when doing permutations if the factors had no impact
plot(results_linear_manova_fit_Suction_a)

###EFFECT SIZE
effect_size_values_Suction_a <- effectsize(results_linear_manova_fit_Suction_a )
effect_size_values_Suction_a
#             tweezer    ind tweezer:ind
#ξ^2 [Pillai]   0.543 0.1694    -0.08044


#As there is no interaction effect, we can test the impact of tweezer and the impact of individuals separately



###POST-HOC TEST
pairwise.glh(fit_Suction_a, term="ind", test=c("Pillai"),
             adjust="holm", nperm=5000)

#General Linear Hypothesis Test with 5000 permutations: Pillai test statistic
#                    Test stat Pr(>Stat) adjusted
#gold1 - gold3          0.3400   0.77924   1.0000 
#gold1 - gold4          0.4321   0.41952   1.0000
#gold1 - ind_geant_5    0.5210   0.13337   1.0000
#gold1 - pink2          0.2841   0.93781   1.0000
#gold3 - gold4          0.3619   0.72226   1.0000
#gold3 - ind_geant_5    0.5288   0.11498   1.0000 
#gold3 - pink2          0.3003   0.90622   1.0000
#gold4 - ind_geant_5    0.5464   0.07678   0.7678
#gold4 - pink2          0.3658   0.71306   1.0000
#ind_geant_5 - pink2    0.5043   0.21136   1.0000
#--- 
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1




# ------------------------------------------------------------------------------
# PART III: TESTING WHICH VARIABLE ARE IMPACTED BY THE TWEEZER
# ------------------------------------------------------------------------------

install.packages('lmerTest')

library(lmerTest)
library(emmeans)
library(ggplot2)
library(ggpubr)


####MG-------------------------------------------------------------------

#FIT THE MODEL
fit1_MG<-lmer(MG ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_MG<-lmer(MG ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_MG,fit2_MG)

#ANOVA
anova(fit1_MG)#impact of the tweezer
emmeans(fit1_MG, pairwise ~ tweezer)#impact of the tweezer

#VISUALISATION
ggboxplot(data_Suction_prepared, x = "tweezer", y = "MG", 
          color = "tweezer", palette = c("#003366", "#FF6600"),
          add = "jitter", 
          add.params = list(shape = "ind")) +
  theme(
    legend.position = "right",  
    legend.box = "vertical",     
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")  
  ) +
  guides(
    shape = guide_legend(title = "Individual"),   
    color = guide_legend(title = "Condition Tweezer")   
  )



####TMG-------------------------------------------------------------------

#FIT THE MODEL
fit1_TMG<-lmer(TMG ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_TMG<-lmer(TMG ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_TMG,fit2_TMG)

#ANOVA
anova(fit1_TMG)#impact of the tweezer
emmeans(fit1_TMG, pairwise ~ tweezer)#impact of the tweezer

#VISUALISATION
ggboxplot(data_Suction_prepared, x = "tweezer", y = "TMG", 
          color = "tweezer", palette = c("#003366", "#FF6600"),
          add = "jitter", 
          add.params = list(shape = "ind")) +
  theme(
    legend.position = "right",  
    legend.box = "vertical",     
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")  
  ) +
  guides(
    shape = guide_legend(title = "Individual"),   
    color = guide_legend(title = "Condition Tweezer")   
  )


###MGA-------------------------------------------------------------------

fit1_MGA<-lmer(MGA ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_MGA<-lmer(MGA ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_MGA,fit2_MGA)

#ANOVA
anova(fit1_MGA)#impact of the tweezer
emmeans(fit1_MGA, pairwise ~ tweezer)#impact of the tweezer

#VISUALISATION
ggboxplot(data_Suction_prepared, x = "tweezer", y = "MGA", 
          color = "tweezer", palette = c("#003366", "#FF6600"),
          add = "jitter", 
          add.params = list(shape = "ind")) +
  theme(
    legend.position = "right",  
    legend.box = "vertical",     
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")  
  ) +
  guides(
    shape = guide_legend(title = "Individual"),   
    color = guide_legend(title = "Condition Tweezer")   
  )



###DG--------------------------------------------------------------------

fit1_DG<-lmer(DG ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_DG<-lmer(DG ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_DG,fit2_DG)

#ANOVA
anova(fit1_DG)#impact of the tweezer
emmeans(fit1_DG, pairwise ~ tweezer)#impact of the tweezer

#VISUALISATION
ggboxplot(data_Suction_prepared, x = "tweezer", y = "DG", 
          color = "tweezer", palette = c("#003366", "#FF6600"),
          add = "jitter", 
          add.params = list(shape = "ind")) +
  theme(
    legend.position = "right",  
    legend.box = "vertical",     
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")  
  ) +
  guides(
    shape = guide_legend(title = "Individual"),   
    color = guide_legend(title = "Condition Tweezer")   
  )



###MSGO------------------------------------------------------------------

fit1_MSGO<-lmer(MSGO ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_MSGO<-lmer(MSGO ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_MSGO,fit2_MSGO)

#ANOVA
anova(fit1_MSGO)#impact of the tweezer
emmeans(fit1_MSGO, pairwise ~ tweezer)#impact of the tweezer

#VISUALISATION
ggboxplot(data_Suction_prepared, x = "tweezer", y = "MSGO", 
          color = "tweezer", palette = c("#003366", "#FF6600"),
          add = "jitter", 
          add.params = list(shape = "ind")) +
  theme(
    legend.position = "right",  
    legend.box = "vertical",     
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")  
  ) +
  guides(
    shape = guide_legend(title = "Individual"),   
    color = guide_legend(title = "Condition Tweezer")   
  )



###MAGO------------------------------------------------------------------

fit1_MAGO<-lmer(MAGO ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_MAGO<-lmer(MAGO ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_MAGO,fit2_MAGO)

#ANOVA
anova(fit1_MAGO)#impact of the tweezer
emmeans(fit1_MAGO, pairwise ~ tweezer)#impact of the tweezer

#VISUALISATION
ggboxplot(data_Suction_prepared, x = "tweezer", y = "MAGO", 
          color = "tweezer", palette = c("#003366", "#FF6600"),
          add = "jitter", 
          add.params = list(shape = "ind")) +
  theme(
    legend.position = "right",  
    legend.box = "vertical",     
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")  
  ) +
  guides(
    shape = guide_legend(title = "Individual"),   
    color = guide_legend(title = "Condition Tweezer")   
  )


###MSGC------------------------------------------------------------------

fit1_MSGC<-lmer(MSGC ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_MSGC<-lmer(MSGC ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_MSGC,fit2_MSGC)

#ANOVA
anova(fit1_MSGC)#tweezer does not impact
emmeans(fit1_MSGC, pairwise ~ tweezer)#tweezer does not impact



###MAGC------------------------------------------------------------------

fit1_MAGC<-lmer(MAGC ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_MAGC<-lmer(MAGC ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_MAGC,fit2_MAGC)

#ANOVA
anova(fit1_MAGC)#tweezer does not impact
emmeans(fit1_MAGC, pairwise ~ tweezer)#tweezer does not impact



###Mhd1------------------------------------------------------------------

fit1_Mhd1<-lmer(Mhd1 ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_Mhd1<-lmer(Mhd1 ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_Mhd1,fit2_Mhd1)

#ANOVA
anova(fit1_Mhd1)#tweezer does not impact
emmeans(fit1_Mhd1, pairwise ~ tweezer)#tweezer does not impact



###TMhd1-----------------------------------------------------------------

fit1_TMhd1<-lmer(TMhd1 ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_TMhd1<-lmer(TMhd1 ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_TMhd1,fit2_TMhd1)

#ANOVA
anova(fit1_TMhd1)#tweezer does not impact
emmeans(fit1_TMhd1, pairwise ~ tweezer)#tweezer does not impact



###Dhd1------------------------------------------------------------------

fit1_Dhd1<-lmer(Dhd1 ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_Dhd1<-lmer(Dhd1 ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_Dhd1,fit2_Dhd1)

#ANOVA
anova(fit1_Dhd1)#tweezer does not impact
emmeans(fit1_Dhd1, pairwise ~ tweezer)#tweezer does not impact



###MSDhd1-------------------------------------------------------------

fit1_MSDhd1<-lmer(MSDhd1 ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_MSDhd1<-lmer(MSDhd1 ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_MSDhd1,fit2_MSDhd1)

#ANOVA
anova(fit1_MSDhd1)#tweezer does not impact
emmeans(fit1_MSDhd1, pairwise ~ tweezer)#tweezer does not impact



###MADhd1-------------------------------------------------------------

fit1_MADhd1<-lmer(MADhd1 ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_MADhd1<-lmer(MADhd1 ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_MADhd1,fit2_MADhd1)

#ANOVA
anova(fit1_MADhd1)#tweezer does not impact
emmeans(fit1_MADhd1, pairwise ~ tweezer)#tweezer does not impact



###MSEhd1---------------------------------------------------------------

fit1_MSEhd1<-lmer(MSEhd1 ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_MSEhd1<-lmer(MSEhd1 ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_MSEhd1,fit2_MSEhd1)

#ANOVA
anova(fit1_MSEhd1)#tweezer does not impact
emmeans(fit1_MSEhd1, pairwise ~ tweezer)#tweezer does not impact



###MAEhd1---------------------------------------------------------------

fit1_MAEhd1<-lmer(MAEhd1 ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_MAEhd1<-lmer(MAEhd1 ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_MAEhd1,fit2_MAEhd1)

#ANOVA
anova(fit1_MAEhd1)#tweezer does not impact
emmeans(fit1_MAEhd1, pairwise ~ tweezer)#tweezer does not impact



###Mhd2------------------------------------------------------------------

fit1_Mhd2<-lmer(Mhd2 ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_Mhd2<-lmer(Mhd2 ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_Mhd2,fit2_Mhd2)

#ANOVA
anova(fit1_Mhd2)#tweezer does not impact
emmeans(fit1_Mhd2, pairwise ~ tweezer)#tweezer does not impact


###TMhd2-----------------------------------------------------------------

fit1_TMhd2<-lmer(TMhd2 ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_TMhd2<-lmer(TMhd2 ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_TMhd2,fit2_TMhd2)

#ANOVA
anova(fit1_TMhd2)#tweezer does not impact
emmeans(fit1_TMhd2, pairwise ~ tweezer)#tweezer does not impact


###Dhd2------------------------------------------------------------------

fit1_Dhd2<-lmer(Dhd2 ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_Dhd2<-lmer(Dhd2 ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_Dhd2,fit2_Dhd2)

#ANOVA
anova(fit1_Dhd2)#tweezer does not impact
emmeans(fit1_Dhd2, pairwise ~ tweezer)#tweezer does not impact


###MSDhd2-------------------------------------------------------------

fit1_MSDhd2<-lmer(MSDhd2 ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_MSDhd2<-lmer(MSDhd2 ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_MSDhd2,fit2_MSDhd2)

#ANOVA
anova(fit1_MSDhd2)#tweezer does not impact
emmeans(fit1_MSDhd2, pairwise ~ tweezer)#tweezer does not impact


###MADhd2-------------------------------------------------------------

fit1_MADhd2<-lmer(MADhd2 ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_MADhd2<-lmer(MADhd2 ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_MADhd2,fit2_MADhd2)

#ANOVA
anova(fit1_MADhd2)#tweezer does not impact
emmeans(fit1_MADhd2, pairwise ~ tweezer)#tweezer does not impact


###MSEhd2---------------------------------------------------------------

fit1_MSEhd2<-lmer(MSEhd2 ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_MSEhd2<-lmer(MSEhd2 ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_MSEhd2,fit2_MSEhd2)

#ANOVA
anova(fit1_MSEhd2)#tweezer does not impact
emmeans(fit1_MSEhd2, pairwise ~ tweezer)#tweezer does not impact


###MAEhd2---------------------------------------------------------------

fit1_MAEhd2<-lmer(MAEhd2 ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_MAEhd2<-lmer(MAEhd2 ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_MAEhd2,fit2_MAEhd2)

#ANOVA
anova(fit1_MAEhd2)#tweezer does not impact
emmeans(fit1_MAEhd2, pairwise ~ tweezer)#tweezer does not impact



###MHA-------------------------------------------------------------------

fit1_MHA<-lmer(MHA ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_MHA<-lmer(MHA ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_MHA,fit2_MHA)

#ANOVA
anova(fit1_MHA)#impact of the tweezer
emmeans(fit1_MHA, pairwise ~ tweezer)#impact of the tweezer

#VISUALISATION
ggboxplot(data_Suction_prepared, x = "tweezer", y = "MHA", 
          color = "tweezer", palette = c("#003366", "#FF6600"),
          add = "jitter", 
          add.params = list(shape = "ind")) +
  theme(
    legend.position = "right",  
    legend.box = "vertical",     
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")  
  ) +
  guides(
    shape = guide_legend(title = "Individual"),   
    color = guide_legend(title = "Condition Tweezer")   
  )


###TMHA------------------------------------------------------------------

fit1_TMHA<-lmer(TMHA ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_TMHA<-lmer(TMHA ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_TMHA,fit2_TMHA)

#ANOVA
anova(fit1_TMHA)#tweezer does not impact
emmeans(fit1_TMHA, pairwise ~ tweezer)#tweezer does not impact


###PCD-------------------------------------------------------------------

fit1_PCD<-lmer(PCD ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_PCD<-lmer(PCD ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_PCD,fit2_PCD)

#ANOVA
anova(fit1_PCD)#tweezer does not impact
emmeans(fit1_PCD, pairwise ~ tweezer)#tweezer does not impact




# ------------------------------------------------------------------------------
# PART IV: TESTING IF HEAD ANGLE AT STRIKE ONSET (HA0) IS IMPACTED THE TWEEZER
# ------------------------------------------------------------------------------


fit1_HA0<-lmer(HA0 ~ tweezer+(1|ind), data_Suction_prepared, REML=FALSE)#best model
fit2_HA0<-lmer(HA0 ~ tweezer+(1|ind), data_Suction_prepared)
AIC(fit1_HA0,fit2_HA0)

#ANOVA
anova(fit1_HA0)#tweezer does impact
emmeans(fit1_HA0, pairwise ~ tweezer)#tweezer does impact
#Type III Analysis of Variance Table with Satterthwaite's method
#           Sum Sq   Mean Sq NumDF  DenDF F value   Pr(>F)
#tweezer 0.0070858 0.0070858     1 49.489  19.186 6.15e-05 ***

#$emmeans
#tweezer emmean      SE    df lower.CL upper.CL
#no       2.104 0.00471 20.27    2.094    2.114
#yes      2.127 0.00423 13.44    2.118    2.136
#Degrees-of-freedom method: kenward-roger
#Confidence level used: 0.95 

#$contrasts
#contrast estimate      SE   df t.ratio p.value
#no - yes  -0.0232 0.00536 50.4  -4.326  0.0001
#Degrees-of-freedom method: kenward-roger 


#VISUALISATION
ggboxplot(data_Suction_prepared, x = "tweezer", y = "HA0", 
          color = "tweezer", palette = c("#003366", "#FF6600"),
          add = "jitter", 
          add.params = list(shape = "ind")) +
  theme(
    legend.position = "right",  
    legend.box = "vertical",   
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid") 
  ) +
  guides(
    shape = guide_legend(title = "Individual"),  
    color = guide_legend(title = "Condition Tweezer")  
  )

#the angle of the head is higher when using tweezer

min_HA0 <- min(data_Suction$HA0, na.rm = TRUE)
min_HA0#115.2213
videos_min_HA0 <- data_Suction$Name_of_the_video[data_Suction$HA0 == min_HA0]# Get the Name_of_the_video(s) corresponding to that minimum HA0
videos_min_HA0#gold3_sans_pince_3_vdt_cropped

max_HA0 <- max(data_Suction$HA0, na.rm = TRUE)
max_HA0#144.4218
videos_max_HA0 <- data_Suction$Name_of_the_video[data_Suction$HA0 == max_HA0]# Get the Name_of_the_video(s) corresponding to that minimum HA0
videos_max_HA0#gold3_pince_8_vdt_cropped



# ------------------------------------------------------------------------------
# PART VI: EVALUATING SNOUT-PREY-DISTACE AT STRIKE ONSET (SPD0) 
# ------------------------------------------------------------------------------


#Snout-prey-distance
mean(data_Suction$SPD0, na.rm = TRUE)#0.05425926
sd(data_Suction$SPD0, na.rm = TRUE)#0.1056088
min(data_Suction$SPD0, na.rm = TRUE)#0
max(data_Suction$SPD0, na.rm = TRUE)#0.46
