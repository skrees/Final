##Magnetics data collected on March 11th, 2017, at Merriam Crater
install.packages("ggmap")
install.packages("sp")
install.packages("maptools")
install.packages("plotly")
install.packages("stats")
library(plotly)
library(ggmap)
library(ggplot2)
require(akima)
library(signal)
??filter

magdata=read.csv("MagProject/Magdata2.csv") ## full dataset
#magdata=read.csv("MagProject/short_data.csv") ## shorter set of data for testing
head(magdata2)
### Making data into dataframes for ggplot###
lat = data.frame(magdata$POS_1_Y) 
long = data.frame(magdata$POS_1_X)
mag = data.frame(magdata$READING_1)
time = data.frame(magdata$TIME)

magdata_mat = as.matrix(magdata)
magdata = magdata[]
lat_mat = as.matrix(magdata$POS_1_Y) 
long_mat = as.matrix(magdata$POS_1_X)
mag_mat = as.matrix(magdata$READING_1)
time_mat = as.matrix(magdata$TIME)
magdata$TIMEVec = as.numeric(magdata$TIME)
################ DIURNAL CORRECTIONS ##############

## BASE STATIONS  == rows == 1,62,106
bs1 = magdata[1,]
bs2 = magdata[62,]
bs3 = magdata[106,]
base_stations = rbind(bs1,bs2, bs3)

ggplot() + geom_line(data=magdata, aes(x =TIMEVec, y =READING_1))+
  geom_point(data=base_stations, aes(x =TIMEVec, y =READING_1),colour = "blue")+
  geom_line(data=base_stations, aes(x =TIMEVec, y =READING_1),colour = "blue")+
  geom_hline(yintercept = 49832.9)+
  scale_x_continuous("Time") +
  scale_y_continuous("Magnetic value (nT)")


################################################################################################
### Creating Numeric matrix for longitude  (plotly required)###
y = as.matrix(sapply(lat, as.numeric))
x = as.matrix(sapply(long, as.numeric))
z = as.matrix(sapply(mag, as.numeric))

### Creates 2D gridded data ###
zsurf = interp(x,y,z)
##########################################################################

#################CREATING ORIGINAL MAP##############################
area = make_bbox(lat = lat, lon = long, f = .2)
map = get_map(location = area, zoom = 19, maptype = "satellite")

base = ggmap(map)
loc = base + geom_point(data = magdata2, aes(x = magdata2$POS_1_X, y = magdata2$POS_1_Y))


m = loc + geom_tile(data = mag, aes(x = long, y = lat, fill = mag),width = 0.00005,height = 0.00005) + scale_fill_gradient(high = "Blue", low = "Red", breaks = seq(49000, 51000, by = 250))


################# PLOTLY mapping ###############################
??add_area
?plot_ly
py = plot_ly(username = 'skrees', key = 'kathleen15')
######## 3-D map ################
xaxis <- list(
  title = "Longitude",
  showgrid = T
)
yaxis <- list(
  title = "Latitude",
  showgrid = T
)
zaxis <- list(
  title = "Raw Magnetic Reading",
  showgrid = T
)
scene = list(
  xaxis = xaxis,
  yaxis = yaxis,
  zaxis = zaxis)

p = plot_ly(magdata) %>%
    add_trace(magdata, type = "surface",
                x = zsurf$x,               
                y = zsurf$y,
                z = zsurf$z,
              colors = "RdYlBu") %>%
    layout(
      title = "Raw Magnetic Reading for Merriam Crater North Rim",
      scene = scene)
p

####### Heatmap #########
pHM = plot_ly(x = magdata$POSX, y = magdata$POSY, z = magdata$READING,type = "contour", colors = "RdYlBu", connectgaps = "T", line = "2")%>%
  layout(title = "Raw Magnetic Reading for Merriam Crater North Rim",xaxis = list(title = "Longitude", showgrid = T), yaxis = list(title = "Latitude"))
pHM

#################### spatial Objects ################
############## CORRECTING FOR EARTH MAG FIELD ######################

Earth_magF = 48758.8 ### at 1900m elevation on 3/11/2017 for area of max/min lay/long
magdata$anom = Earth_magF
for ( i in 1:107){
  magdata$anom[] = magdata$READING[] - magdata$anom[]
}

### Creates 2D gridded data for anomaly ###
za = as.matrix(sapply(magdata$anom, as.numeric))
zanom = interp(x,y,za)

### 3D mapping for anomaly
za_axis <- list(
  title = "Magnetic Anomaly",
  showgrid = T
)
scene = list(
  xaxis = xaxis,
  yaxis = yaxis,
  zaxis = za_axis)

p_anom = plot_ly(magdata) %>%
  add_trace(magdata, type = "surface",
            x = zanom$x,               
            y = zanom$y,
            z = zanom$z,
            colors = "RdYlBu") %>%
  layout(
    title = "Magnetic Anomaly for Merriam Crater North Rim",
    scene = scene)
p_anom

####### Heatmap #########
pHM_anom = plot_ly(x = magdata$POS_1_X, y = magdata$POS_1_Y, z = magdata$anom,type = "contour", colors = "RdYlBu", connectgaps = "T", line = "2")%>%
  layout(title = "Magnetic Anomaly for Merriam Crater North Rim",xaxis = list(title = "Longitude", showgrid = T), yaxis = list(title = "Latitude"))
pHM_anom

#####################DESPIKING DATA ##########################
spike = 850

for (i in 2:106){
  if ((magdata$anom[i] - magdata$anom[i-1]) >= 850){
    magdata$anom[i-1] = NA
  }else(NULL)
}
####### Heatmap -- Despiked #########
pHM_ds = plot_ly(x = magdata$POS_1_X, y = magdata$POS_1_Y, z = magdata$anom,type = "contour", colors = "RdYlBu", connectgaps = "T", line = "2")%>%
  layout(title = "Magnetic Anomaly for Merriam Crater North Rim",xaxis = list(title = "Longitude", showgrid = T), yaxis = list(title = "Latitude"))
pHM_ds

### Creates 2D gridded data for despike ###
isna = which(is.na(magdata$anom))
magmat = matrix(data = NA, ncol = 8, nrow = 106)
magmat = magdata[-isna,]
x_ds = as.matrix(sapply(magmat$POS_1_X, as.numeric))
y_ds = as.matrix(sapply(magmat$POS_1_Y, as.numeric))
z_ds = as.matrix(sapply(magmat$anom, as.numeric))
zs_ds = interp(x_ds,y_ds,z_ds)

?which
### 3D mapping for despike
zds_axis <- list(
  title = "Magnetic Anomaly",
  showgrid = T
)
scene = list(
  xaxis = xaxis,
  yaxis = yaxis,
  zaxis = zds_axis)

p_ds = plot_ly(magmat) %>%
  add_trace(magmat, type = "surface",
            x = zs_ds$x,               
            y = zs_ds$y,
            z = zs_ds$z,
            colors = "RdYlBu") %>%
 
  layout(
    title = "Magnetic Anomaly for Merriam Crater North Rim",
    scene = scene)
p_ds

####################################################################
################ New working dataset - magmat ######################
####################################################################

###### LOW-PASS FILTERING ##########
magmat
lowpass.spline = smooth.spline(magmat$TIMEVec, magmat$anom, spar = 0.2) ## Control spar for amount of smoothing - 0.1 produces best fit 
lowpass.loess = loess(magmat$anom ~ magmat$TIMEVec, data = data.frame(x = magmat$TIMEVec, y = magmat$anom), span = 0.1) ## control span to define the amount of smoothing
lowpass_anom = predict(lowpass.spline, magmat$TIMEVec)
lowpass_leossanom = predict(lowpass.loess, magmat$TIMEVec) ### DIDNT USE BECAUSE WASNT AS SMOOTH
highpass = magmat$anom - lowpass_anom$y ## subtract lowpass data from original to get highpass data

####### LINE PLOTS FOR FILTERED DATA
ggplot() + geom_line(data=magmat, aes(x =TIMEVec, y =anom, colour = "Original Anomaly"))+
  geom_line(aes(x =lowpass_anom$x, y =lowpass_anom$y,colour = "Lowpass Filtered"))+
  geom_line(aes(x =magmat$TIMEVec, y =highpass, colour = "Highpass Filtered"))+
  labs(title = "Filtered Magnetic Anomaly")+
  scale_x_continuous("Time") +
  scale_y_continuous("Magnetic anomaly (nT)")

?legend
?ggplot
pLP = plot_ly(x = magdata$POS_1_X, y = magdata$POS_1_Y, z = lowpass_anom$y,type = "contour", colors = "RdYlBu", connectgaps = "T", line = "2")%>%
  layout(title = "Low Pass Filtered Magnetic Anomaly",xaxis = list(title = "Longitude", showgrid = T), yaxis = list(title = "Latitude"))
pLP

pHP = plot_ly(x = magdata$POS_1_X, y = magdata$POS_1_Y, z = highpass,type = "contour", colors = "RdYlBu", connectgaps = "T", line = "2")%>%
  layout(title = "High Pass Filtered Magnetic Anomaly",xaxis = list(title = "Longitude", showgrid = T), yaxis = list(title = "Latitude"))
pHP

### 3D mapping for despike
zLP = as.matrix(sapply(lowpass_anom$y, as.numeric))
zHP = as.matrix(sapply(highpass, as.numeric))
zs_HP = interp(x_ds,y_ds,zHP)
zs_LP = interp(x_ds,y_ds,zLP)

zds_axis <- list(
  title = "Magnetic Anomaly",
  showgrid = T
)
scene = list(
  xaxis = xaxis,
  yaxis = yaxis,
  zaxis = zds_axis)

pFilt3D = plot_ly(magmat) %>%
  add_trace(magmat, type = "surface",
            x = zs_ds$x,               
            y = zs_ds$y,
            z = zs_ds$z,
            colors = "RdYlBu") %>%  
  add_trace(magmat, type = "surface",
            x = zs_LP$x,              
            y = zs_LP$y,
            z = zs_LP$z,
            colors = "RdYlBu") %>%
  add_trace(magmat, type = "surface",
            x = zs_HP$x,              
            y = zs_HP$y,
            z = zs_HP$z,
            colors = "RdYlBu") %>%
  
  layout(
    title = "Magnetic Anomaly for Merriam Crater North Rim",
    scene = scene)
pFilt3D
