# ------------------------------------------------------------------------------
# TONGUE PREHENSION - ANALYSIS TWEEZER AND INDIVIDUAL IMPACT
# ------------------------------------------------------------------------------




# ------------------------------------------------------------------------------
# PART I: DATA PREPARATION
# ------------------------------------------------------------------------------


###OPEN FILE ("Terrestrial_dataset.csv")
data_TgP<-read.csv(file.choose(), header= T, sep=";", dec=".")


###SIMPLIFY DATA (getting rid of some columns)
data_TgP_reduit<-data_TgP[ , ! colnames(data_TgP) %in% c("prey_capture_type",#behavior
                                                         "TMGA",#similaire à TMG
                                                         "Thd1", "Thd2",#initial movement of hyoid is difficult to capture
                                                         "T0","Tmax_gape","Tend_gape","T0_tongue","P0_tongue","Tmax_tongue","Tend_tongue","T0_hd1","Tmax_hd1","Tend_hd1","T0_hd2","Tmax_hd2","Tend_hd2")]#variables useless for the analysis, (sometimes manually) extracted from the raw data. See SCRIPT_TERRESTRIAL_FEEDING_data_extraction.R for definitions


###CONVERT SPEED IN ABSOLUTE VALUE (MSGC, MSTgR, MSEhd1, MSEhd2)
position_column_MSGC<- which(colnames(data_TgP_reduit) == 'MSGC')#to find the position of column 'MSGC' in the data frame data_TgP_reduit
position_column_MSTgR<- which(colnames(data_TgP_reduit) == 'MSTgR')
position_column_MSEhd1<- which(colnames(data_TgP_reduit) == 'MSEhd1')
position_column_MSEhd2<- which(colnames(data_TgP_reduit) == 'MSEhd2')
columns_to_be_converted_in_absolute_value<-c(position_column_MSGC,position_column_MSTgR,position_column_MSEhd1,position_column_MSEhd2)
data_TgP_reduit[,columns_to_be_converted_in_absolute_value]<-abs(data_TgP_reduit[,columns_to_be_converted_in_absolute_value])


###NORMALIZE THE DATA
# Apply log10 only on numerical columns
data_prepared<-data_TgP_reduit
data_prepared[] <- lapply(data_prepared, function(x) {
  if (is.numeric(x)) {
    log10(x)  # Apply log10 only when it is a numerical column
  } else {
    x  # else, keep the column unchanged
  }
})


##Adequate sample size -> RRule of thumb: the size n in each cell is greater than the number of response variables.
##33 variables 
#data_TgP %>%
#  group_by(ind) %>%
#  summarise(N = n())
##N<33 thus 



# ------------------------------------------------------------------------------
# PART II: TESTING INDIVIDUAL AND TWEEZER IMPACT AT GLOBAL SCALE
# ------------------------------------------------------------------------------


###NB: As the number of variable exceed the number of observations (video sequences) we use mvols from package mvMORPH which permit to avoid variable reduction

install.packages("mvMORPH")
library(mvMORPH)
citation("mvMORPH")
packageVersion("mvMORPH")



### TESTING TWEEZER*IND IMPACT--------------------------------------------------


###FITTING THE DATA TO A LINEAR MODEL

fit_a <- mvols(as.matrix(data_prepared[, 3:34]) ~ tweezer*ind, 
               data = data_prepared, 
               method = "PL-LOOCV",
               target="Variance")
summary(fit_a)
#GIC=-895.9968

fit_b <- mvols(as.matrix(data_prepared[, 3:34]) ~ tweezer*ind, 
               data = data_prepared, 
               method = "LL",
               target="Variance")
summary(fit_b)
#GIC=-1148.652

#fit_b model having the smallest GIC, we perform the manova on fit_b.


###testing mutlinormality of the data residuals
mvqqplot(fit_b)


###MANOVA
results_linear_manova_fit_b <- manova.gls(fit_b, test = c("Pillai"), type=c("II"))#LL nperm is useless
results_linear_manova_fit_b

#Manova detects no interaction but there is an individual effect.
#Type II MANOVA Tests: Pillai test statistic 
#            Df test stat approx F num Df den Df  Pr(>F)  
#tweezer      1    0.8699   2.0890     32     10 0.10859 
#ind          4    3.2442   1.7437    128     52 0.01222 *
#tweezer:ind  4    2.8078   0.9568    128     52 0.58815   
#--- 
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



###EFFECT SIZE
effect_size_values_b <- effectsize(results_linear_manova_fit_b)
effect_size_values_b
#             tweezer   ind tweezer:ind
#ξ^2 [Pillai]  0.8699 0.811       0.702


#As there is no interaction effect, we can test the impact of tweezer and the impact of individuals separately


###POST-HOC TEST
pairwise.glh(fit_b, term="ind", test=c("Pillai"),
             adjust="holm")

#General Linear Hypothesis Test: Pillai test statistic 
#                    Df test stat approx F num Df den Df  Pr(>F) adjusted  
#A_m_ind2 - A_m_ind3  1    0.8848   2.4003     32     10 0.07181   0.5027  
#A_m_ind2 - A_t_ind1  1    0.7621   1.0013     32     10 0.53497   1.0000  
#A_m_ind2 - A_t_ind2  1    0.9389   4.8008     32     10 0.00609   0.0609 .
#A_m_ind2 - A_t_ind3  1    0.8405   1.6465     32     10 0.20414   1.0000  
#A_m_ind3 - A_t_ind1  1    0.8296   1.5210     32     10 0.24607   1.0000  
#A_m_ind3 - A_t_ind2  1    0.8511   1.7868     32     10 0.16626   0.9976   
#A_m_ind3 - A_t_ind3  1    0.7280   0.8366     32     10 0.66965   1.0000 
#A_t_ind1 - A_t_ind2  1    0.9245   3.8241     32     10 0.01459   0.1313  
#A_t_ind1 - A_t_ind3  1    0.7608   0.9939     32     10 0.54061   1.0000  
#A_t_ind2 - A_t_ind3  1    0.8907   2.5458     32     10 0.05971   0.4777   
#--- 
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1




# ------------------------------------------------------------------------------
# PART III: TESTING THE IMPACT OF PREY CAPTURE TYPE
# ------------------------------------------------------------------------------


#13 jaws vs 39 tweezer


data_prepared$prey_capture_type<-data_TgP$prey_capture_type



fit_c <- mvols(as.matrix(data_prepared[, 3:34]) ~ prey_capture_type*ind, 
               data = data_prepared, 
               method = "PL-LOOCV",
               target="Variance")
summary(fit_c)
#GIC=  -989.9443

fit_d <- mvols(as.matrix(data_prepared[, 3:34]) ~ prey_capture_type*ind, 
               data = data_prepared, 
               method = "LL",#maximum likelihood (method="LL")
               target="Variance")
summary(fit_d)
#GIC=  -1327.426

#the best model is fit_d


###testing mutlinormality of the data residuals
mvqqplot(fit_d)


###MANOVA
results_linear_manova_fit_d <- manova.gls(fit_d, test = c("Pillai"), type=c("II"))
results_linear_manova_fit_d
#impact of the type of prey capture and of ind. No interaction term

#Type II MANOVA Tests: Pillai test statistic 
#                      Df test stat approx F num Df den Df    Pr(>F)
#prey_capture_type      1    0.9521    6.833     32     11 0.0008775 ***
#ind                    4    3.3903    2.433    128     56 0.0001439 ***
#prey_capture_type:ind  3    2.3514    1.473     96     39 0.0874732   .
#--- 
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

###POST-HOC TEST
pairwise.glh(fit_d, term="ind", test=c("Pillai"),
             adjust="holm")

#General Linear Hypothesis Test: Pillai test statistic 
#                    Df test stat approx F num Df den Df   Pr(>F) adjusted 
#A_m_ind2 - A_m_ind3  1    0.6466   0.6290     32     11 0.850682  1.00000   
#A_m_ind2 - A_t_ind1  1    0.6038   0.5240     32     11 0.924029  1.00000   
#A_m_ind2 - A_t_ind2  1    0.9122   3.5692     32     11 0.014585  0.10480
#A_m_ind2 - A_t_ind3  1    0.8647   2.1968     32     11 0.083087  0.49852   
#A_m_ind3 - A_t_ind1  1    0.8117   1.4820     32     11 0.248747  0.99499   
#A_m_ind3 - A_t_ind2  1    0.9404   5.4250     32     11 0.002504  0.02504 *
#A_m_ind3 - A_t_ind3  1    0.8392   1.7938     32     11 0.152023  0.76011    
#A_t_ind1 - A_t_ind2  1    0.9143   3.6670     32     11 0.013100  0.10480
#A_t_ind1 - A_t_ind3  1    0.7337   0.9473     32     11 0.575201  1.00000   
#A_t_ind2 - A_t_ind3  1    0.9401   5.3997     32     11 0.002556  0.02504 *
#--- 
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 




# ------------------------------------------------------------------------------
# PART IV: TESTING WHICH KINEMATICS VALUES ARE IMPACTED BY THE PREY CAPTURE 
# TYPE AT GLOBAL SCALE
# ------------------------------------------------------------------------------

install.packages('lmerTest')

library(lmerTest)
library(emmeans)
library(ggplot2)



###MG--------------------------------------------------------------------

#FIT THE MODEL
fit1_MG<-lmer(MG ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MG<-lmer(MG ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MG,fit2_MG)

#ANOVA
anova(fit1_MG)#prey_capture_type does not impact
emmeans(fit1_MG, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###TMG--------------------------------------------------------------------------

#FIT THE MODEL
fit1_TMG<-lmer(TMG ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_TMG<-lmer(TMG ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_TMG,fit2_TMG)

#ANOVA
anova(fit1_TMG)#prey_capture_type does not impact
emmeans(fit1_TMG, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###MGA--------------------------------------------------------------------

#FIT THE MODEL
fit1_MGA<-lmer(MGA ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MGA<-lmer(MGA ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MGA,fit2_MGA)

#ANOVA
anova(fit1_MGA)#prey_capture_type does not impact
emmeans(fit1_MGA, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###DG--------------------------------------------------------------------

#FIT THE MODEL
fit1_DG<-lmer(DG ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_DG<-lmer(DG ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_DG,fit2_DG)

#ANOVA
anova(fit1_DG)#prey_capture_type does not impact
emmeans(fit1_DG, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###MSGO--------------------------------------------------------------------

#FIT THE MODEL
fit1_MSGO<-lmer(MSGO ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MSGO<-lmer(MSGO ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MSGO,fit2_MSGO)

#ANOVA
anova(fit1_MSGO)#prey_capture_type does not impact
emmeans(fit1_MSGO, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###MAGO--------------------------------------------------------------------

#FIT THE MODEL
fit1_MAGO<-lmer(MAGO ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MAGO<-lmer(MAGO ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MAGO,fit2_MAGO)

#ANOVA
anova(fit1_MAGO)#prey_capture_type impacts
emmeans(fit1_MAGO, pairwise ~ prey_capture_type)#prey_capture_type impacts


install.packages("ggpubr")
library(ggpubr)

#VISUALISATION
ggboxplot(data_prepared, x = "prey_capture_type", y = "MAGO", 
          color = "prey_capture_type", palette = c("#333300", "#999900"),
          add = "jitter", 
          add.params = list(shape = "ind")) +
  theme(
    legend.position = "right",  # Position the legend on the right side
    legend.box = "vertical",    # The legend will be vertical.
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")  # Add a frame around the caption
  ) +
  guides(
    shape = guide_legend(title = "Individual"),  # Legend title for shapes
    color = guide_legend(title = "Prey capture type")  # Legend title for colors
  )+
  ylab("MAGO log10 values")+
  xlab("Prey capture type")



###T-TEST MSGC--------------------------------------------------------------------

#FIT THE MODEL
fit1_MSGC<-lmer(MSGC ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MSGC<-lmer(MSGC ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MSGC,fit2_MSGC)

#ANOVA
anova(fit1_MSGC)#prey_capture_type does not impact
emmeans(fit1_MSGC, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST MAGC--------------------------------------------------------------------

#FIT THE MODEL
fit1_MAGC<-lmer(MAGC ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MAGC<-lmer(MAGC ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MAGC,fit2_MAGC)

#ANOVA
anova(fit1_MAGC)#prey_capture_type does not impact
emmeans(fit1_MAGC, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST MTgP--------------------------------------------------------------------

#FIT THE MODEL
fit1_MTgP<-lmer(MTgP ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MTgP<-lmer(MTgP ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MTgP,fit2_MTgP)

#ANOVA
anova(fit1_MTgP)#prey_capture_type impacts
emmeans(fit1_MTgP, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST TMTgP--------------------------------------------------------------------

#FIT THE MODEL
fit1_TMTgP<-lmer(TMTgP ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_TMTgP<-lmer(TMTgP ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_TMTgP,fit2_TMTgP)

#ANOVA
anova(fit1_TMTgP)#prey_capture_type does not impact
emmeans(fit1_TMTgP, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST TgD--------------------------------------------------------------------

#FIT THE MODEL
fit1_TgD<-lmer(TgD ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_TgD<-lmer(TgD ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_TgD,fit2_TgD)

#ANOVA
anova(fit1_TgD)#prey_capture_type does not impact
emmeans(fit1_TgD, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST MSTgP--------------------------------------------------------------------

#FIT THE MODEL
fit1_MSTgP<-lmer(MSTgP ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MSTgP<-lmer(MSTgP ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MSTgP,fit2_MSTgP)

#ANOVA
anova(fit1_MSTgP)#prey_capture_type does not impact
emmeans(fit1_MSTgP, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST MATgP--------------------------------------------------------------------

#FIT THE MODEL
fit1_MATgP<-lmer(MATgP ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MATgP<-lmer(MATgP ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MATgP,fit2_MATgP)

#ANOVA
anova(fit1_MATgP)#prey_capture_type impacts
emmeans(fit1_MATgP, pairwise ~ prey_capture_type)#prey_capture_type impacts


#VISUALISATION
ggboxplot(data_prepared, x = "prey_capture_type", y = "MATgP", 
          color = "prey_capture_type", palette = c("#333300", "#999900"),
          add = "jitter", 
          add.params = list(shape = "ind")) +
  theme(
    legend.position = "right",  
    legend.box = "vertical",    
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")  
  ) +
  guides(
    shape = guide_legend(title = "Individual"), 
    color = guide_legend(title = "Prey capture type")  
  )+
  ylab("MATgP log10 values")+
  xlab("Prey capture type")


###T-TEST MSTgR--------------------------------------------------------------------

#FIT THE MODEL
fit1_MSTgR<-lmer(MSTgR ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MSTgR<-lmer(MSTgR ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MSTgR,fit2_MSTgR)

#ANOVA
anova(fit1_MSTgR)#prey_capture_type does not impact
emmeans(fit1_MSTgR, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST MATgR--------------------------------------------------------------------

#FIT THE MODEL
fit1_MATgR<-lmer(MATgR ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MATgR<-lmer(MATgR ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MATgR,fit2_MATgR)

#ANOVA
anova(fit1_MATgR)#prey_capture_type does not impact
emmeans(fit1_MATgR, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST Mhd1--------------------------------------------------------------------

#FIT THE MODEL
fit1_Mhd1<-lmer(Mhd1 ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_Mhd1<-lmer(Mhd1 ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_Mhd1,fit2_Mhd1)

#ANOVA
anova(fit1_Mhd1)#prey_capture_type does not impact
emmeans(fit1_Mhd1, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST TMhd1--------------------------------------------------------------------

#FIT THE MODEL
fit1_TMhd1<-lmer(TMhd1 ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_TMhd1<-lmer(TMhd1 ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_TMhd1,fit2_TMhd1)

#ANOVA
anova(fit1_TMhd1)#prey_capture_type does not impact
emmeans(fit1_TMhd1, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST Dhd1--------------------------------------------------------------------

#FIT THE MODEL
fit1_Dhd1<-lmer(Dhd1 ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_Dhd1<-lmer(Dhd1 ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_Dhd1,fit2_Dhd1)

#ANOVA
anova(fit1_Dhd1)#prey_capture_type does not impact
emmeans(fit1_Dhd1, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST MSDhd1--------------------------------------------------------------------

#FIT THE MODEL
fit1_MSDhd1<-lmer(MSDhd1 ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MSDhd1<-lmer(MSDhd1 ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MSDhd1,fit2_MSDhd1)

#ANOVA
anova(fit1_MSDhd1)#prey_capture_type does not impact
emmeans(fit1_MSDhd1, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST MADhd1--------------------------------------------------------------------

#FIT THE MODEL
fit1_MADhd1<-lmer(MADhd1 ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MADhd1<-lmer(MADhd1 ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MADhd1,fit2_MADhd1)

#ANOVA
anova(fit1_MADhd1)#prey_capture_type does not impact
emmeans(fit1_MADhd1, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST MSEhd1--------------------------------------------------------------------

#FIT THE MODEL
fit1_MSEhd1<-lmer(MSEhd1 ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MSEhd1<-lmer(MSEhd1 ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MSEhd1,fit2_MSEhd1)

#ANOVA
anova(fit1_MSEhd1)#prey_capture_type does not impact
emmeans(fit1_MSEhd1, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST MAEhd1--------------------------------------------------------------------

#FIT THE MODEL
fit1_MAEhd1<-lmer(MAEhd1 ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MAEhd1<-lmer(MAEhd1 ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MAEhd1,fit2_MAEhd1)

#ANOVA
anova(fit1_MAEhd1)#prey_capture_type does not impact
emmeans(fit1_MAEhd1, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST Mhd2--------------------------------------------------------------------

#FIT THE MODEL
fit1_Mhd2<-lmer(Mhd2 ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_Mhd2<-lmer(Mhd2 ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_Mhd2,fit2_Mhd2)

#ANOVA
anova(fit1_Mhd2)#prey_capture_type does not impact
emmeans(fit1_Mhd2, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST TMhd2--------------------------------------------------------------------

#FIT THE MODEL
fit1_TMhd2<-lmer(TMhd2 ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_TMhd2<-lmer(TMhd2 ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_TMhd2,fit2_TMhd2)

#ANOVA
anova(fit1_TMhd2)#prey_capture_type does not impact
emmeans(fit1_TMhd2, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST Dhd2--------------------------------------------------------------------

#FIT THE MODEL
fit1_Dhd2<-lmer(Dhd2 ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_Dhd2<-lmer(Dhd2 ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_Dhd2,fit2_Dhd2)

#ANOVA
anova(fit1_Dhd2)#prey_capture_type does not impact
emmeans(fit1_Dhd2, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST MSDhd2--------------------------------------------------------------------

#FIT THE MODEL
fit1_MSDhd2<-lmer(MSDhd2 ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MSDhd2<-lmer(MSDhd2 ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MSDhd2,fit2_MSDhd2)

#ANOVA
anova(fit1_MSDhd2)#prey_capture_type does not impact
emmeans(fit1_MSDhd2, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST MADhd2--------------------------------------------------------------------

#FIT THE MODEL
fit1_MADhd2<-lmer(MADhd2 ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MADhd2<-lmer(MADhd2 ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MADhd2,fit2_MADhd2)

#ANOVA
anova(fit1_MADhd2)#prey_capture_type does not impact
emmeans(fit1_MADhd2, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST MSEhd2--------------------------------------------------------------------

#FIT THE MODEL
fit1_MSEhd2<-lmer(MSEhd2 ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MSEhd2<-lmer(MSEhd2 ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MSEhd2,fit2_MSEhd2)

#ANOVA
anova(fit1_MSEhd2)#prey_capture_type does not impact
emmeans(fit1_MSEhd2, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST MAEhd2--------------------------------------------------------------------

#FIT THE MODEL
fit1_MAEhd2<-lmer(MAEhd2 ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MAEhd2<-lmer(MAEhd2 ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MAEhd2,fit2_MAEhd2)

#ANOVA
anova(fit1_MAEhd2)#prey_capture_type does not impact
emmeans(fit1_MAEhd2, pairwise ~ prey_capture_type)#prey_capture_type does not impact



##T-TEST MHA--------------------------------------------------------------------

#FIT THE MODEL
fit1_MHA<-lmer(MHA ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_MHA<-lmer(MHA ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_MHA,fit2_MHA)

#ANOVA
anova(fit1_MHA)#prey_capture_type does not impact
emmeans(fit1_MHA, pairwise ~ prey_capture_type)#prey_capture_type does not impact



###T-TEST TMHA--------------------------------------------------------------------

#FIT THE MODEL
fit1_TMHA<-lmer(TMHA ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_TMHA<-lmer(TMHA ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_TMHA,fit2_TMHA)

#ANOVA
anova(fit1_TMHA)##prey_capture_type does not impact
emmeans(fit1_TMHA, pairwise ~ prey_capture_type)##prey_capture_type does not impact



###T-TEST PCD-------------------------------------------------------------------

#FIT THE MODEL
fit1_PCD<-lmer(PCD ~ prey_capture_type+(1|ind), data_prepared, REML=FALSE)#best model
fit2_PCD<-lmer(PCD ~ prey_capture_type+(1|ind), data_prepared)
AIC(fit1_PCD,fit2_PCD)

#ANOVA
anova(fit1_PCD)##prey_capture_type does not impact
emmeans(fit1_PCD, pairwise ~ prey_capture_type)##prey_capture_type does not impact



###Script for figure 3----------------------------------------------------------

# First graph : MAGO
plot1 <- ggboxplot(data_prepared, x = "prey_capture_type", y = "MAGO", 
                   color = "prey_capture_type", palette = c("#333300", "#999900"),
                   add = "jitter", 
                   add.params = list(shape = "ind")) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")
  ) +
  guides(
    shape = guide_legend(title = "Individuals"),
    color = guide_legend(title = "Prey capture type")
  ) +
  ylab("MAGO log10 values") +
  xlab("Prey capture type")

# Second graph : MATgP
plot2 <- ggboxplot(data_prepared, x = "prey_capture_type", y = "MATgP", 
                   color = "prey_capture_type", palette = c("#333300", "#999900"),
                   add = "jitter", 
                   add.params = list(shape = "ind")) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")
  ) +
  guides(
    shape = guide_legend(title = "Individuals"),
    color = guide_legend(title = "Prey capture type")
  ) +
  ylab("MATgP log10 values") +
  xlab("Prey capture type")

# Display both graphs side by side
ggarrange(plot1, plot2, ncol = 2, common.legend = TRUE, legend = "right")



# ------------------------------------------------------------------------------
# PART V: TESTING IF PREY PRESENTATION METHODS IMPACT HEAD ANGLE AT STRIKE ONSET 
# (HA0)
# ------------------------------------------------------------------------------

#FIT THE MODEL
fit1_HA0<-lmer(HA0 ~ tweezer+(1|ind), data_prepared, REML=FALSE)#best model
fit2_HA0<-lmer(HA0 ~ tweezer+(1|ind), data_prepared)
AIC(fit1_HA0,fit2_HA0)

#ANOVA
anova(fit1_HA0)
emmeans(fit1_HA0, pairwise ~ tweezer)
#tweezer does not impact HA0

#Type III Analysis of Variance Table with Satterthwaite's method
#            Sum Sq    Mean Sq NumDF  DenDF F value Pr(>F)
#tweezer 0.00016324 0.00016324     1 46.278  0.2603 0.6123

#$emmeans
# tweezer emmean     SE   df lower.CL upper.CL
# no        2.09 0.0119 8.76     2.06     2.11
# yes       2.08 0.0114 7.30     2.06     2.11
#Degrees-of-freedom method: kenward-roger 
#Confidence level used: 0.95

#$contrasts
# contrast estimate      SE   df t.ratio p.value
# no - yes  0.00369 0.00731 47.2   0.504  0.6166
#Degrees-of-freedom method: kenward-roger  



# ------------------------------------------------------------------------------
# PART VI: EVALUATING SNOUT-PREY-DISTACE AT STRIKE ONSET (SPD0) 
# ------------------------------------------------------------------------------


#Snout-prey-distance
mean(data_TgP$SPD0, na.rm = TRUE)#0.737451
sd(data_TgP$SPD0, na.rm = TRUE)#0.6857634
min(data_TgP$SPD0, na.rm = TRUE)#0
max(data_TgP$SPD0, na.rm = TRUE)#3.17

