#################################################################################
#                                                                               #
# EXTRACTION of suction feeding kinematics data from a DeepLabCut .CSV file      #
#                                                                               #
#################################################################################



########################################
#                                      #
# PARTI : PAKAGES AND LIBRARIES NEEDED #
#                                      #
########################################

install.packages('openxlsx')
install.packages("ggplot2")

library(openxlsx) #to export data in excel form
library(dplyr)
library(ggplot2)



########################################
#                                      #
# PARTII : CONVERTING VARIABLES UNIT   #
# AND CALCULATING PRIMARY VARIABLES    #                                  
#                                      #
########################################



########## STEP 1 --> CHOOSE AND OPEN ONE OF THE CSV FILE OBTAINED WITH DEEPLABCUT

#First we have to choose the csv file to be transformed into data frame
initial_data_frame<-read.csv(file.choose(), header= TRUE, sep=",", dec=".")
#initial_data_frame



########## STEP 2 --> CONVERT THE DATA IN THE RIGHT UNITS

scale=112.1116#WRITE THE PIXEL VALUE corresponding to 2cm
nbfps= 1000 #WRITE THE NUMBER OF FRAME PER SECOND

#nr is the total number of row in the initial_data_frame
nr<-as.numeric(nrow(initial_data_frame))

#The first 4 rows of initial_data_frame only contain title, 
#that's why we convert the numerical value from row "4" to row "nr" [4:nr, ].
#All conversion are made using the rule of three.

time<-(as.numeric(c(initial_data_frame[4:nr, 1]))/nbfps)#converting from frame per second in second unit

x_eye<-(as.numeric(c(initial_data_frame[4:nr, 2]))*2)/scale #converting from pixel to cm thanks to the rule of three
y_eye<-(as.numeric(c(initial_data_frame[4:nr, 3]))*2)/scale

x_upjaw<-(as.numeric(c(initial_data_frame[4:nr, 5]))*2)/scale
y_upjaw<-(as.numeric(c(initial_data_frame[4:nr, 6]))*2)/scale

x_lowjaw<-(as.numeric(c(initial_data_frame[4:nr, 8]))*2)/scale
y_lowjaw<-(as.numeric(c(initial_data_frame[4:nr, 9]))*2)/scale

x_comissure<-(as.numeric(c(initial_data_frame[4:nr, 11]))*2)/scale
y_comissure<-(as.numeric(c(initial_data_frame[4:nr, 12]))*2)/scale

x_hyoid1<-(as.numeric(c(initial_data_frame[4:nr, 14]))*2)/scale
y_hyoid1<-(as.numeric(c(initial_data_frame[4:nr, 15]))*2)/scale
#we keep the likelihood value for the hyoid as x and y coords are ok if likelihood > 0.4
likelihood_hyoid1<-(as.numeric(c(initial_data_frame[4:nr, 16])))

x_hyoid2<-(as.numeric(c(initial_data_frame[4:nr, 17]))*2)/scale
y_hyoid2<-(as.numeric(c(initial_data_frame[4:nr, 18]))*2)/scale
#we keep the likelihood value for the hyoid as x and y coords are ok if likelihood > 0.4
likelihood_hyoid2<-(as.numeric(c(initial_data_frame[4:nr, 19])))

x_trunk<-(as.numeric(c(initial_data_frame[4:nr, 20]))*2)/scale
y_trunk<-(as.numeric(c(initial_data_frame[4:nr, 21]))*2)/scale



##########STEP 3 --> CREATE A NEW_DATA_FRAME WITH THE VARIABLES OF INTEREST

#the new data_frame contain the variables of interest converted in the right units.
new_data_frame<-data.frame(time,x_eye,y_eye,x_upjaw,y_upjaw,x_lowjaw,y_lowjaw,x_comissure,y_comissure,x_hyoid1,y_hyoid1,likelihood_hyoid1,x_hyoid2,y_hyoid2,likelihood_hyoid2,x_trunk,y_trunk)
new_data_frame



##########STEP 4 --> SMOOTHING CURVES


#SMOOTHING EYE curve coordinates through time

plot(time,x_eye)
spline_x_eye<-smooth.spline(time,x_eye,df=80) #smooth curve with 30 degrees of freedom
lines(spline_x_eye, col="red")
new_data_frame$x_eye_smoothed <- spline_x_eye$y #add x_eye_lisse in new_data_frame

plot(time,y_eye)
spline_y_eye<-smooth.spline(time,y_eye,df=80)
lines(spline_y_eye, col="red")
new_data_frame$y_eye_smoothed <- spline_y_eye$y #add y_eye_lisse in new_data_frame


#SMOOTHING UPJAW curve coordinates through time

plot(time,x_upjaw)
spline_x_upjaw<-smooth.spline(time,x_upjaw,df=80) #lisse la courbe avec 30 degrés de liberté
lines(spline_x_upjaw, col="red")
new_data_frame$x_upjaw_smoothed <- spline_x_upjaw$y #ajoute la colonne x_upjaw_lisse dans new_data_frame

plot(time,y_upjaw)
spline_y_upjaw<-smooth.spline(time,y_upjaw,df=80)
lines(spline_y_upjaw, col="red")
new_data_frame$y_upjaw_smoothed <- spline_y_upjaw$y #ajoute la colonne y_upjaw_lisse dans new_data_frame


#SMOOTHING LOWJAW curve coordinates through time

plot(time,x_lowjaw)
spline_x_lowjaw<-smooth.spline(time,x_lowjaw,df=80) #lisse la courbe avec 30 degrés de liberté
lines(spline_x_lowjaw, col="red")
new_data_frame$x_lowjaw_smoothed <- spline_x_lowjaw$y #ajoute la colonne x_upjaw_lisse dans new_data_frame

plot(time,y_lowjaw)
spline_y_lowjaw<-smooth.spline(time,y_lowjaw,df=80)
lines(spline_y_lowjaw, col="red")
new_data_frame$y_lowjaw_smoothed <- spline_y_lowjaw$y #ajoute la colonne y_lowjaw_lisse dans new_data_frame


#SMOOTHING COMISSURE curve coordinates through time

plot(time,x_comissure)
spline_x_comissure<-smooth.spline(time,x_comissure,df=80) #lisse la courbe avec 30 degrés de liberté
lines(spline_x_comissure, col="red")
new_data_frame$x_comissure_smoothed <- spline_x_comissure$y #ajoute la colonne x_comissure_lisse dans new_data_frame

plot(time,y_comissure)
spline_y_comissure<-smooth.spline(time,y_comissure,df=80) #lisse la courbe avec 30 degrés de liberté
lines(spline_y_comissure, col="red")
new_data_frame$y_comissure_smoothed <- spline_y_comissure$y #ajoute la colonne y_comissure_lisse dans new_data_frame


#SMOOTHING HYOID curve coordinates through time

plot(time,x_hyoid1)
spline_x_hyoid1<-smooth.spline(time,x_hyoid1,df=80) #lisse la courbe avec 30 degrés de liberté
lines(spline_x_hyoid1, col="red")
new_data_frame$x_hyoid1_smoothed <- spline_x_hyoid1$y #ajoute la colonne x_hyoid_lisse dans new_data_frame

plot(time,y_hyoid1)
spline_y_hyoid1<-smooth.spline(time,y_hyoid1,df=80) #lisse la courbe avec 30 degrés de liberté
lines(spline_y_hyoid1, col="red")
new_data_frame$y_hyoid1_smoothed <- spline_y_hyoid1$y #ajoute la colonne y_hyoid_lisse dans new_data_frame


plot(time,x_hyoid2)
spline_x_hyoid2<-smooth.spline(time,x_hyoid2,df=80) #lisse la courbe avec 30 degrés de liberté
lines(spline_x_hyoid2, col="red")
new_data_frame$x_hyoid2_smoothed <- spline_x_hyoid2$y #ajoute la colonne x_hyoid_lisse dans new_data_frame

plot(time,y_hyoid2)
spline_y_hyoid2<-smooth.spline(time,y_hyoid2,df=80) #lisse la courbe avec 30 degrés de liberté
lines(spline_y_hyoid2, col="red")
new_data_frame$y_hyoid2_smoothed <- spline_y_hyoid2$y #ajoute la colonne y_hyoid_lisse dans new_data_frame


#SMOOTHING TRUNK curve coordinates through time

plot(time,x_trunk)
spline_x_trunk<-smooth.spline(time,x_trunk,df=80) #lisse la courbe avec 30 degrés de liberté
lines(spline_x_trunk, col="red")
new_data_frame$x_trunk_smoothed <- spline_x_trunk$y #ajoute la colonne x_trunk_lisse dans new_data_frame

plot(time,y_trunk)
spline_y_trunk<-smooth.spline(time,y_trunk,df=80) #lisse la courbe avec 30 degrés de liberté
lines(spline_y_trunk, col="red")
new_data_frame$y_trunk_smoothed <- spline_y_trunk$y #ajoute la colonne y_trunk_lisse dans new_data_frame



##########STEP 5 --> CALCULATE MOVEMENTS THROUGH TIME

#Reminder:
#dAB=sqrt(((xa-xb)**2) + ((Ya-yb)**2))

ntot<-as.numeric(nrow(new_data_frame))#number of rows in new_data_frame


#GAPE OF THE MOUTH

gape<-c()
for (n in 1:ntot) {
  new_value=sqrt(((new_data_frame$x_upjaw_smoothed[n]-new_data_frame$x_lowjaw_smoothed[n])**2)+((new_data_frame$y_upjaw_smoothed[n]-new_data_frame$y_lowjaw_smoothed[n])**2))
  gape<-append(gape,new_value)
  print(gape)
}
new_data_frame$gape<-gape


#HYOID MOVEMENT calculated as hyoid-comissure distance

mvt_hyoid1<-c()
for (n in 1:ntot) {
  new_value=sqrt(((new_data_frame$x_comissure_smoothed[n]-new_data_frame$x_hyoid1_smoothed[n])**2)+((new_data_frame$y_comissure_smoothed[n]-new_data_frame$y_hyoid1_smoothed[n])**2))
  mvt_hyoid1<-append(mvt_hyoid1,new_value)
  print(mvt_hyoid1)
}
new_data_frame$mvt_hyoid1<-mvt_hyoid1


mvt_hyoid2<-c()
for (n in 1:ntot) {
  new_value=sqrt(((new_data_frame$x_comissure_smoothed[n]-new_data_frame$x_hyoid2_smoothed[n])**2)+((new_data_frame$y_comissure_smoothed[n]-new_data_frame$y_hyoid2_smoothed[n])**2))
  mvt_hyoid2<-append(mvt_hyoid2,new_value)
  print(mvt_hyoid2)
}
new_data_frame$mvt_hyoid2<-mvt_hyoid2



########## STEP 6 --> CALCULATE SPEED AND ACCELERATION


#HYOID speed and acceleration

new_data_frame$hyoid1_speed <- c(0, diff(new_data_frame$mvt_hyoid1) / diff(new_data_frame$time))
new_data_frame$hyoid1_acceleration <- c(0, diff(new_data_frame$hyoid1_speed) / diff(new_data_frame$time))

new_data_frame$hyoid2_speed <- c(0, diff(new_data_frame$mvt_hyoid2) / diff(new_data_frame$time))
new_data_frame$hyoid2_acceleration <- c(0, diff(new_data_frame$hyoid2_speed) / diff(new_data_frame$time))


#GAPE speed and acceleration

new_data_frame$gape_speed <- c(0, diff(new_data_frame$gape) / diff(new_data_frame$time))
new_data_frame$gape_acceleration <- c(0, diff(new_data_frame$gape_speed) / diff(new_data_frame$time))



########## STEP 7 --> CALCULATE ANGLES


### GAPE ANGLE = maximum mouth opening angle

#angle ABC, A=upjaw, B=comissure, C=lowjaw

dfGA <- data.frame(
  xA = c(new_data_frame$x_upjaw_smoothed),
  yA = c(new_data_frame$y_upjaw_smoothed),
  xB = c(new_data_frame$x_comissure_smoothed),
  yB = c(new_data_frame$y_comissure_smoothed),
  xC = c(new_data_frame$x_lowjaw_smoothed),
  yC = c(new_data_frame$y_lowjaw_smoothed)
)


# Fonction pour calculer l'angle en degrés entre trois points
calculate_angle <- function(xA, yA, xB, yB, xC, yC) {
  AB <- sqrt((xB - xA)^2 + (yB - yA)^2)
  BC <- sqrt((xC - xB)^2 + (yC - yB)^2)
  AC <- sqrt((xC - xA)^2 + (yC - yA)^2)
  angle_rad <- acos((AB^2 + BC^2 - AC^2) / (2 * AB * BC))
  angle_deg <- angle_rad * 180 / pi
  return(angle_deg)
}

# Calculer les angles pour chaque ligne du dataframe
dfGA$angle <- apply(dfGA, 1, function(row) {
  calculate_angle(row["xA"], row["yA"], row["xB"], row["yB"], row["xC"], row["yC"])
})

# Afficher le résultat
print(dfGA)



### HEAD ANGLE = maximum mouth opening angle

#angle ABC, A=upjaw, B=eye, C=trunk

dfHA <- data.frame(
  xA = c(new_data_frame$x_upjaw_smoothed),
  yA = c(new_data_frame$y_upjaw_smoothed),
  xB = c(new_data_frame$x_eye_smoothed),
  yB = c(new_data_frame$y_eye_smoothed),
  xC = c(new_data_frame$x_trunk_smoothed),
  yC = c(new_data_frame$y_trunk_smoothed)
)


# Fonction pour calculer l'angle en degrés entre trois points
calculate_angle <- function(xA, yA, xB, yB, xC, yC) {
  AB <- sqrt((xB - xA)^2 + (yB - yA)^2)
  BC <- sqrt((xC - xB)^2 + (yC - yB)^2)
  AC <- sqrt((xC - xA)^2 + (yC - yA)^2)
  angle_rad <- acos((AB^2 + BC^2 - AC^2) / (2 * AB * BC))
  angle_deg <- angle_rad * 180 / pi
  return(angle_deg)
}

# Calculer les angles pour chaque ligne du dataframe
dfHA$angle <- apply(dfHA, 1, function(row) {
  calculate_angle(row["xA"], row["yA"], row["xB"], row["yB"], row["xC"], row["yC"])
})

# Afficher le résultat
print(dfHA)


########## STEP 8 --> CLEAN_DATA_FRAME

#gathering the kinematic variables into a new data frame
clean_data_frame<-data.frame(time=new_data_frame$time, 
                             gape=new_data_frame$gape, gape_speed=new_data_frame$gape_speed, gape_acceleration=new_data_frame$gape_acceleration, gape_angle=dfGA$angle,
                             mvt_hyoid1=new_data_frame$mvt_hyoid1, hyoid1_speed=new_data_frame$hyoid1_speed, hyoid1_acceleration=new_data_frame$hyoid1_acceleration,
                             mvt_hyoid2=new_data_frame$mvt_hyoid2, hyoid2_speed=new_data_frame$hyoid2_speed, hyoid2_acceleration=new_data_frame$hyoid2_acceleration, 
                             head_angle=dfHA$angle)
clean_data_frame
#export new_data_frame in excel form in the folder documents of the computer
#write.xlsx(clean_data_frame,file = "clean_data_frame.xlsx", rowNames = FALSE)



########################################
#                                      #
# PARTIII : EXTRACTION OF KINEMATIC    #
# VARIABLES RELATED TO MOUTH MOVEMENTS #
# MG, TMG, MGA, TMGA, DG, MSGO, MSGC,  #
# MAGO, MAGC                           #
#                                      #
########################################


####VISUALIZATION_OF_GAPE_EVOLUTION###################################################################
######################################################################################################

time <- clean_data_frame$time
movement <- clean_data_frame$gape
df <- data.frame(time, movement)

# Trouver les indices où le mouvement change de direction
change_direction <- diff(sign(diff(movement)))
# Indice des sommets
sommet_indices <- which(change_direction < 0) + 1
# Indice des creux
creux_indices <- which(change_direction > 0) + 1

# Coordonnées des sommets et creux
sommet_coords <- df[sommet_indices, ]
creux_coords <- df[creux_indices, ]

# Ajouter les indices des sommets et creux
sommet_coords$index <- seq_along(sommet_indices)
creux_coords$index <- seq_along(creux_indices)

# Tracer la courbe du mouvement en fonction du temps avec sommets et creux numérotés
p <- ggplot(df, aes(x = time, y = movement)) +
  geom_line() +
  geom_point(data = sommet_coords, aes(color = "Sommets"), size = 3) +
  geom_point(data = creux_coords, aes(color = "Creux"), size = 3) +
  # Ajouter le numéro du sommet au-dessus du point
  geom_text(data = sommet_coords, aes(label = index),
            nudge_y = 0.2, color = "red") +
  # Ajouter le numéro du creux au-dessus du point
  geom_text(data = creux_coords, aes(label = index),
            nudge_y = 0.2, color = "blue") +
  labs(x = "Temps", y = "Mouvement", title = "Courbe de mouvement avec sommets et creux") +
  scale_color_manual(values = c("Sommets" = "red", "Creux" = "blue")) +
  theme_minimal()

print(p)
print(sommet_coords) #gives time;movement coordinates for each red peak
print(creux_coords) #gives the time;movement coordinates for each blue trough


####MANUAL_SETTINGS###################################################################################
######################################################################################################

#ENTER BEGINING TIME and DISTANCE GAPE (T0 and G0) and ENDING TIME (Tend_gape) of gape movement

T0 <- creux_coords$time[2] #PUT THE CORRECT NUMBER
G0 <- creux_coords$movement[2]#PUT THE CORRECT NUMBER
Tend_gape<- creux_coords$time[3]#PUT THE CORRECT NUMBER

#if needed, enter manual value
T0<-0.006
G0<-0.168892713170934
Tend_gape<- creux_coords$time[2] #PUT THE CORRECT NUMBER

Tend_gape<-0.079


####CALCUL_KINEMATICS_VARIABLES#######################################################################
######################################################################################################

# MG # MAXIMUM GAPE
MG <- max(clean_data_frame$gape) - G0
# TMG # TIME TO MAXIMUM GAPE
Tmax_gape<- clean_data_frame$time[which.max(clean_data_frame$gape)]
TMG <- Tmax_gape - T0
# MGA # MAXIMUM GAPE ANGLE
MGA <- max(clean_data_frame$gape_angle) - clean_data_frame$gape_angle[clean_data_frame$time == T0]
# TMGA # TIME TO MAXIMUM GAPE ANGLE (should be the same than TMG)
TMGA <- clean_data_frame$time[which.max(clean_data_frame$gape_angle)] - T0
# DG # DURATION GAPE
DG <- Tend_gape - T0 #PUT THE CORRECT NUMBER
# Filter intervals
interval_T0_Tmax <- new_data_frame[new_data_frame$time >= T0 & new_data_frame$time <= Tmax_gape, ]
interval_Tmax_Tend <- new_data_frame[new_data_frame$time >= Tmax_gape & new_data_frame$time <= Tend_gape, ]
# MSGO # MAXIMUM SPEED OF GAPE OPENING
MSGO <- max(interval_T0_Tmax$gape_speed, na.rm = TRUE)         # Valeur max de gape_speed entre T0 et Tmax_gape
# MAGO # MAXIMUM ACCELERATION DURING MOUTH OPENING
MAGO <- max(interval_T0_Tmax$gape_acceleration, na.rm = TRUE)  # Valeur max de gape_acceleration entre T0 et Tmax_gape
# MSGC # MAXIMUM SPEED OF GAPE CLOSING
MSGC <- min(interval_Tmax_Tend$gape_speed, na.rm = TRUE)       # Valeur min de gape_speed entre Tmax_gape et Tend_gape #taking the minimal because as the mouth is closing it gives negative values
# MAGC # MAXIMUM ACCELERATION DURING MOUTH CLOSING
MAGC <- max(interval_Tmax_Tend$gape_acceleration, na.rm = TRUE)# Valeur max de gape_acceleration entre Tmax_gape et Tend_gape


####RESULTS###########################################################################################
######################################################################################################

T0
MG # MAXIMUM GAPE
TMG # TIME TO MAXIMUM GAPE
MGA # MAXIMUM GAPE ANGLE
TMGA # TIME TO MAXIMUM GAPE ANGLE
DG # DURATION GAPE
MSGO # MAXIMUM SPEED OF GAPE OPENING
MAGO # MAXIMUM ACCELERATION DURING MOUTH OPENING
MSGC # MAXIMUM SPEED OF GAPE CLOSING
MAGC # MAXIMUM ACCELERATION DURING MOUTH CLOSING



########################################
#                                      #
# PARTIV : EXTRACTION OF KINEMATIC     #
# VARIABLES RELATED TO HD1 MOVEMENTS   #
# Thd1, Mhd1, TMhd1, Dhd1, MSDhd1,     #
# MADhd1, MSEhd1, MAEhd1               #
#                                      #
########################################


####VISUALIZATION_OF_HD1_EVOLUTION###################################################################
######################################################################################################

time <- clean_data_frame$time
movement <- clean_data_frame$mvt_hyoid1
df <- data.frame(time, movement)

# Trouver les indices où le mouvement change de direction
change_direction <- diff(sign(diff(movement)))
# Indice des sommets
sommet_indices <- which(change_direction < 0) + 1
# Indice des creux
creux_indices <- which(change_direction > 0) + 1

# Coordonnées des sommets et creux
sommet_coords <- df[sommet_indices, ]
creux_coords <- df[creux_indices, ]

# Ajouter les indices des sommets et creux
sommet_coords$index <- seq_along(sommet_indices)
creux_coords$index <- seq_along(creux_indices)

# Tracer la courbe du mouvement en fonction du temps avec sommets et creux numérotés
p <- ggplot(df, aes(x = time, y = movement)) +
  geom_line() +
  geom_point(data = sommet_coords, aes(color = "Sommets"), size = 3) +
  geom_point(data = creux_coords, aes(color = "Creux"), size = 3) +
  # Ajouter le numéro du sommet au-dessus du point
  geom_text(data = sommet_coords, aes(label = index),
            nudge_y = 0.2, color = "red") +
  # Ajouter le numéro du creux au-dessus du point
  geom_text(data = creux_coords, aes(label = index),
            nudge_y = 0.2, color = "blue") +
  labs(x = "Temps", y = "Mouvement", title = "Courbe de mouvement avec sommets et creux") +
  scale_color_manual(values = c("Sommets" = "red", "Creux" = "blue")) +
  theme_minimal()

print(p)
print(sommet_coords) #gives time;movement coordinates for each red peak
print(creux_coords) #gives the time;movement coordinates for each blue trough



####MANUAL_SETTINGS###################################################################################
######################################################################################################

#To interpret the curve accurately, watching the video may be necessary.

#ENTER BEGINING TIME and DISTANCE GAPE (T0_hd1 and P0_hd1) and ENDING TIME (Tend_hd1) of hd1 movement

T0_hd1 <-creux_coords$time[3]#PUT THE CORRECT NUMBER
P0_hd1 <-creux_coords$movement[3]#PUT THE CORRECT NUMBER
Tend_hd1<-creux_coords$time[4]#PUT THE CORRECT NUMBER

T0_hd1 <- 0.007
P0_hd1 <- 0.392719463810072

Tend_hd1<- 0.325

####CALCUL_KINEMATICS_VARIABLES#######################################################################
######################################################################################################

# Thd1 # time when hyoid1 begin to depress compared to T0
Thd1<-T0_hd1-T0
# Mhd1 # maximal hyoid1 depression
Mhd1 <- max(clean_data_frame$mvt_hyoid1) - P0_hd1
# TMhd1 # Time to maximal hyoid1 depression
Tmax_hd1<-clean_data_frame$time[which.max(clean_data_frame$mvt_hyoid1)]
TMhd1 <-Tmax_hd1 - T0 
#Dhd1 #duration of hyoid1 cycle
Dhd1 <-Tend_hd1 - T0_hd1 


# Filter intervals
interval_T0_hd1_Tmax_hd1 <- new_data_frame[new_data_frame$time >= T0_hd1 & new_data_frame$time <= Tmax_hd1, ]
interval_Tmax_hd1_Tend_hd1 <- new_data_frame[new_data_frame$time >= Tmax_hd1 & new_data_frame$time <= Tend_hd1, ]
# MSDhd1 # max hyoid1 speed when depressing
MSDhd1 <- max(interval_T0_hd1_Tmax_hd1$hyoid1_speed, na.rm = TRUE)  
# MADhd1 # max hyoid1 accelertion when depressing
MADhd1 <- max(interval_T0_hd1_Tmax_hd1$hyoid1_acceleration, na.rm = TRUE) 
# MSEhd1 # max hyoid1 speed when going back to initial position
MSEhd1 <- min(interval_Tmax_hd1_Tend_hd1$hyoid1_speed, na.rm = TRUE)  
#MAEhd1 # max hyoid1 acceleration when going back to initial position
MAEhd1 <- max(interval_Tmax_hd1_Tend_hd1$hyoid1_acceleration, na.rm = TRUE) 


####RESULTS###########################################################################################
######################################################################################################

T0
T0_hd1
Tmax_hd1
Tend_hd1

Thd1#time when hyoid1 begin to depress compared to T0
Mhd1 #maximal hyoid1 depression
TMhd1 #Time to maximal hyoid1 depression
Dhd1 #duration of hyoid1 cycle
MSDhd1 # max hyoid1 speed when depressing
MADhd1 # max hyoid1 accelertion when depressing
MSEhd1 # max hyoid1 speed when going back to initial position
MAEhd1 # max hyoid1 acceleration when going back to initial position



########################################
#                                      #
# PARTV : EXTRACTION OF KINEMATIC      #
# VARIABLES RELATED TO HD2 MOVEMENTS   #
# Thd2, Mhd2, TMhd2, Dhd2, MSDhd2,     #
# MADhd2, MSEhd2, MAEhd2               #
#                                      #
########################################


####VISUALIZATION_OF_HD2_EVOLUTION###################################################################
######################################################################################################

time <- clean_data_frame$time
movement <- clean_data_frame$mvt_hyoid2
df <- data.frame(time, movement)

# Trouver les indices où le mouvement change de direction
change_direction <- diff(sign(diff(movement)))
# Indice des sommets
sommet_indices <- which(change_direction < 0) + 1
# Indice des creux
creux_indices <- which(change_direction > 0) + 1

# Coordonnées des sommets et creux
sommet_coords <- df[sommet_indices, ]
creux_coords <- df[creux_indices, ]

# Ajouter les indices des sommets et creux
sommet_coords$index <- seq_along(sommet_indices)
creux_coords$index <- seq_along(creux_indices)

# Tracer la courbe du mouvement en fonction du temps avec sommets et creux numérotés
p <- ggplot(df, aes(x = time, y = movement)) +
  geom_line() +
  geom_point(data = sommet_coords, aes(color = "Sommets"), size = 3) +
  geom_point(data = creux_coords, aes(color = "Creux"), size = 3) +
  # Ajouter le numéro du sommet au-dessus du point
  geom_text(data = sommet_coords, aes(label = index),
            nudge_y = 0.2, color = "red") +
  # Ajouter le numéro du creux au-dessus du point
  geom_text(data = creux_coords, aes(label = index),
            nudge_y = 0.2, color = "blue") +
  labs(x = "Temps", y = "Mouvement", title = "Courbe de mouvement avec sommets et creux") +
  scale_color_manual(values = c("Sommets" = "red", "Creux" = "blue")) +
  theme_minimal()

print(p)
print(sommet_coords) #gives time;movement coordinates for each red peak
print(creux_coords) #gives the time;movement coordinates for each blue trough



####MANUAL_SETTINGS###################################################################################
######################################################################################################

#To interpret the curve accurately, watching the video may be necessary.

#ENTER BEGINING TIME and DISTANCE GAPE (T0_hd2 and P0_hd2) and ENDING TIME (Tend_hd2) of hd2 movement

T0_hd2 <-creux_coords$time[3]#PUT THE CORRECT NUMBER
P0_hd2 <-creux_coords$movement[3]#PUT THE CORRECT NUMBER
Tend_hd2<-creux_coords$time[21]#PUT THE CORRECT NUMBER

T0_hd2 <-0.014
P0_hd2 <-0.688436813099591
Tend_hd2<-creux_coords$time[14]#PUT THE CORRECT NUMBER

Tend_hd2<-0.325

####CALCUL_KINEMATICS_VARIABLES#######################################################################
######################################################################################################

# Thd2 # time when hyoid2 begin to depress compared to T0
Thd2<-T0_hd2-T0
# Mhd2 # maximal hyoid2 depression
Mhd2 <- max(clean_data_frame$mvt_hyoid2) - P0_hd2
# TMhd2 # Time to maximal hyoid2 depression
Tmax_hd2<-clean_data_frame$time[which.max(clean_data_frame$mvt_hyoid2)]
TMhd2 <-Tmax_hd2 - T0 
#Dhd2 #duration of hyoid2 cycle
Dhd2 <-Tend_hd2 - T0_hd2 


# Filter intervals
interval_T0_hd2_Tmax_hd2 <- new_data_frame[new_data_frame$time >= T0_hd2 & new_data_frame$time <= Tmax_hd2, ]
interval_Tmax_hd2_Tend_hd2 <- new_data_frame[new_data_frame$time >= Tmax_hd2 & new_data_frame$time <= Tend_hd2, ]
# MSDhd2 # max hyoid2 speed when depressing
MSDhd2 <- max(interval_T0_hd2_Tmax_hd2$hyoid2_speed, na.rm = TRUE)  
# MADhd2 # max hyoid2 accelertion when depressing
MADhd2 <- max(interval_T0_hd2_Tmax_hd2$hyoid2_acceleration, na.rm = TRUE) 
# MSEhd2 # max hyoid2 speed when going back to initial position
MSEhd2 <- min(interval_Tmax_hd2_Tend_hd2$hyoid2_speed, na.rm = TRUE)  
#MAEhd2 # max hyoid2 acceleration when going back to initial position
MAEhd2 <- max(interval_Tmax_hd2_Tend_hd2$hyoid2_acceleration, na.rm = TRUE) 


####RESULTS###########################################################################################
######################################################################################################

T0
T0_hd2
Tmax_hd2
Tend_hd2

Thd2#time when hyoid2 begin to depress compared to T0
Mhd2 #maximal hyoid2 depression
TMhd2 #Time to maximal hyoid2 depression
Dhd2 #duration of hyoid2 cycle
MSDhd2 # max hyoid2 speed when depressing
MADhd2 # max hyoid2 accelertion when depressing
MSEhd2 # max hyoid2 speed when going back to initial position
MAEhd2 # max hyoid2 acceleration when going back to initial position



########################################
#                                      #
# PARTVI : EXTRACTION OF KINEMATIC     #
# VARIABLES RELATED TO HEAD MOVEMENTS  #
# AND TOTAL CYCLE DURATION             #
# PCD, TMHA, MHA                       #                         
#                                      #
########################################

# Comparaison des valeurs de temps et calcul de PCD
if (Tend_hd1 > Tend_hd2) {
  PCD <- Tend_hd1 - T0
} else {
  PCD <- Tend_hd2 - T0
}


# Filtrage du dataframe entre T0 et la valeur la plus tardive entre TFhd1 et TFhd2
subset_data <- subset(clean_data_frame, time >= T0 & time <= max(Tend_hd1, Tend_hd2))

# Trouver le maximum de la colonne head_angle dans le sous-ensemble de données filtré
MHA <- max(subset_data$head_angle)
TMHA <- clean_data_frame$time[ which(clean_data_frame$head_angle == MHA)]-T0


# Afficher le résultat

MHA
TMHA
PCD
