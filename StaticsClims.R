library(sf)
library(raster)
library(rasterVis)
# library(xlsx)
library(dplyr)
library(ggplot2)
library(viridis)
library(latticeExtra)
library(RColorBrewer)
library(sp)
# Ruta --------------------------------------------------------------------
getwd()
# Datos --------------------------------------------------------------------
ruta <- paste0("wc2.1_2.5m_prec_2010-2018/",
       "wc2.1_2.5m_prec_", rep(seq(2014,2018), each = 12),
       "-", rep(c(paste0("0",seq(1,9)),c(10,11,12)),5), ".tif")
shp <- read_sf("prov_cldisol.shp") %>% st_transform(crs = 4326)
dem <- raster("dem/altitud_crop_lima.tif")
alt <- raster::extract(dem, shp, df = TRUE)
h <- mean(alt$altitud_crop_lima) / 1000
lat <- abs((st_coordinates(st_transform(st_centroid(st_transform(shp, epsg = 32718)), epsg = 4326)))[2])
df <- data.frame(FECHA = 1:60, min = 1:60, mean = 1:60, max = 1:60)
dfm <- data.frame(`Mes` = seq(as.Date("2014-01-01"), by = "month", length = 60))
dfa <- data.frame(`Año` = seq(as.Date("2014-01-01"), by = "year", length = 5))
ptr <- c("prec", "tmax", "tmin")

# Proceso: Medias --------------------------------------------------------------
for (j in 1:length(ptr)) {
  substr(ruta, 12, 15) <- ptr[j] # Cambio de rutas
  substr(ruta, 38, 41) <- ptr[j]
  
  # Proceso: Medias --------------------------------------------------------------
  for(i in 1:length(ruta)) {
    pp <- raster(ruta[i])
    values = raster::extract(x = pp, y = shp, df = TRUE)
    colnames(values)[2] <- c("1")
    values <- group_by(values, ID) %>% 
      summarize_at(vars(1), list(~min(.), ~mean(.), ~max(.))) #%>% 
      # mutate(semisuma = (max + min) / 2) # otro criterio
    df[i, 1:4] <- c(paste0(substr(ruta[i], 43, 49), "-01"), c(values[1, 2:4]))
    df[, 1] <- as.Date(df[, 1])
    df["year"] <- format(df["FECHA"], "%Y")
    # df[i, 1:5] <- c(paste0(substr(ruta[i], 43, 49), "-01"), c(values[1, 2:5]))
    # df[, 1] <- as.Date(df[, 1])
    # df["year"] <- format(df["FECHA"], "%Y")
  }
  

  # Guardado de medias  ---------------------------------------------------------------
  if (j == 1) {
    write.xlsx(df, file= paste0("ParametrosMetoro.xlsx"), sheetName = paste0(ptr[j], "2014-2018"), append = F, row.names = F)
  } else {
    write.xlsx(df, file= paste0("ParametrosMetoro.xlsx"), sheetName = paste0(ptr[j], "2014-2018"), append = T, row.names = F)
  }
  

  # Temperaturas cálidas y frías (tabla) --------------------------------------------
  if (ptr[j] == "tmax") {
    dfm["meantmax"] <- df["mean"]
  }
  
  if (ptr[j] == "tmin") {
    dfm["meantmin"] <- df["mean"]
  }

  # Medias anuales ----------------------------------------------
  if (ptr[j] == "prec") {
    n <- group_by(df, year) %>% summarise(a = sum(mean)) # suma de promedios en un año
    # n <- group_by(df, year) %>% summarise(a = mean(semisuma)) # otro criterio
    colnames(n)[2] <- paste0("P", ptr[j])
    dfa[j + 1] <- n[,2]
  } else {
    n <- group_by(df, year) %>% summarise(a = mean(mean))
    # n <- group_by(df, year) %>% summarise(a = mean(semisuma)) # promedio de los semisumas mensuales en un año
    colnames(n)[2] <- paste0("P", ptr[j])
    dfa[j + 1] <- n[,2]
  }
}

# Temperaturas medias del mes más cálido - frío -------------------------

# dfm <- dfm %>% mutate(`TemMed` = (Semitmax + Semitmin)/2) # Para las semisumas
# dfm["year"] <- substring(dfm$Mes, 1, 4)
# Dfm <- dfm %>% group_by(year) %>% summarize(`TMMC` = max(TemMed), `TMMF` = min(TemMed))

dfm["year"] <- substring(dfm$Mes, 1, 4)
Dfm <- dfm %>% group_by(year) %>% summarize(`TMMC` = max(meantmax), `TMMF` = min(meantmin)) %>% mutate(`Am` = TMMC - TMMF)

# Indices -----------------------------------------------------------------
attach(Dfm)
dfa <- dfa %>% mutate(`I. Aridez Martonne` = Pprec / (((Ptmax + Ptmin) / 2) + 10),
               `I. Continentalidad Currey` = (Ptmax - Ptmin) / (1 + (1/3) * lat),
               `I. Continentalidad Daget` = ((1.7 * (TMMC - TMMF)) / sin((lat + 10 + (9 * h)) * (pi / 180))) - 14,
               `I. Termopluviométrico Dantin-Revenga` = 100 * ((Ptmax + Ptmin) / 2)/ Pprec)
write.xlsx(dfa, file = paste0("ParametrosMetoro.xlsx"), sheetName = "Índices", append = T, row.names = F)


# Resample del dem a wordclim ------------------------------------------------------

shp <- read_sf("prov_cldisol.shp") %>% st_transform(shp1, crs = 4326)
dem <- raster("dem/altitud_crop_lima.tif")
wd <- crop(raster(ruta[1]), shp, snap = 'out') # raster_molde
dem_rwd <- resample(dem, wd)
writeRaster(dem_rwd, filename = "dem/dem_rwd.tif")


# Raster de latitudes del raster resampleado a dem_rwd ------------------------------

coor <- raster::coordinates(dem_rwd) # extrae coordenadas x,y del raster
long <- abs(coor[,1]) # extrae coordenada x
lat <- abs(coor[,2]) # extrae coordenada y
tiz_long<-tiz # raster de longitud
tiz_long$band1<-long # sustitucion de values por longitud
Rlat <- dem_rwd # raster de latitud
Rlat$band1 <- lat # sustitucion de values por latitud
latitud <- Rlat$band1
writeRaster(latitud, filename = "dem/Rlat.tif")


# Indice mensual de Martonne ----------------------------------------------

Mprec <- brick(stack(list.files('/home/jnck/Documents/biogeografia/practica/material/wc2.1/wc2.1_2.5m_prec_2010-2018/NormCrop/', full.names = T)))
Mtmax <- brick(stack(list.files('/home/jnck/Documents/biogeografia/practica/material/wc2.1/wc2.1_2.5m_tmax_2010-2018/NormCrop/', full.names = T)))
Mtmin <- brick(stack(list.files('/home/jnck/Documents/biogeografia/practica/material/wc2.1/wc2.1_2.5m_tmin_2010-2018/NormCrop/', full.names = T)))

Martonne <- (12 * Mprec) / ((Mtmax + Mtmin)/2 + 10)
Mart <- mask(Martonne, shp)
Martonne <- resample(Martonne, dem) 

Martonne2014 <- mask(Martonne[[1:12,]], shp)
Martonne2015 <- mask(Martonne[[13:24,]], shp)
Martonne2016 <- mask(Martonne[[25:36,]], shp)
Martonne2017 <- mask(Martonne[[37:48,]], shp)
Martonne2018 <- mask(Martonne[[49:60,]], shp)

prov <- st_read('Shp/prov_limac.shp') %>% st_transform(crs = 4326)
meses <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')
colr <- colorRampPalette(brewer.pal(11, 'RdYlBu'))

levelplot(Marton[[1:12,]], 
          margin=TRUE,                       
          colorkey=list(
            space='bottom',                   
            labels=list(at=seq(0, 100, 10), font=4),
            axis.line=list(col='black'),
            width=0.75
          ),    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ), names.attr= meses, 
          col.regions=colr,
          scales=list(draw=FALSE),
          xlab=NULL,
          ylab=NULL,
          main='Índice de Aridez de Martonne en el 2015') +
  latticeExtra::layer(sp.polygons(as_Spatial(prov), lwd=1))
  



# Generación de promedios mensuales y anuales de raster -------------------

A2014P <- 0; A2015P <- 0; A2016P <- 0; A2017P <- 0; A2018P <- 0
A2014TX <- 0; A2015TX <- 0; A2016TX <- 0; A2017TX <- 0; A2018TX <- 0
A2014TN <- 0; A2015TN <- 0; A2016TN <- 0; A2017TN <- 0; A2018TN <- 0

for (j in 1:length(ptr)) {
  substr(ruta, 12, 15) <- ptr[j]
  substr(ruta, 38, 41) <- ptr[j]
  for(i in 1:length(ruta)) {
    # general <- raster("wc2.1_2.5m_tmin_2010-2018/wc2.1_2.5m_tmin_2018-12.tif") # raster meteorologico
    meteoro <- raster(ruta[i])
    # crope = resample(crop(meteoro, shp, snap = 'out'), dem)
    crope = crop(meteoro, shp, snap = 'out')
    # maskk = mask(cropp, shp)
    writeRaster(crope, filename = paste0(getwd(),"/wc2.1_2.5m_", ptr[j], "_2010-2018/NormCrop/", "NormCroP",substr(ruta[i], 38, 53)))
    
    # algebra de mapas --------------------------------------------------------
    
    if (ptr[j] == "prec") {
      if (i <= 12) {
        A2014P <- A2014P + crope
      }
      if (i > 12 & i <= 24) {
        A2015P <- A2015P + crope
      }
      if (i > 24 & i <= 36) {
        A2016P <- A2016P + crope
      }
      if (i > 36 & i <= 48) {
        A2017P <- A2017P + crope
      }
      if (i > 48 & i <= 60) {
        A2018P <- A2018P + crope
      }
      # listPrec <- append(listPrec, crope)
    }
    
    if (ptr[j] == "tmax") {
      if (i <= 12) {
        A2014TX <- A2014TX + crope / 12
      }
      if (i > 12 & i <= 24) {
        A2015TX <- A2015TX + crope / 12
      }
      if (i > 24 & i <= 36) {
        A2016TX <- A2016TX + crope / 12
      }
      if (i > 36 & i <= 48) {
        A2017TX <- A2017TX + crope / 12
      }
      if (i > 48 & i <= 60) {
        A2018TX <- A2018TX + crope / 12
      }
      # listTmx <- append(listTmx, crope)
    }
    
    if (ptr[j] == "tmin") {
      if (i <= 12) {
        A2014TN <- A2014TN + crope / 12
      }
      if (i > 12 & i <= 24) {
        A2015TN <- A2015TN + crope / 12
      }
      if (i > 24 & i <= 36) {
        A2016TN <- A2016TN + crope / 12
      }
      if (i > 36 & i <= 48) {
        A2017TN <- A2017TN + crope / 12
      }
      if (i > 48 & i <= 60) {
        A2018TN <- A2018TN + crope / 12
      }
      # listTmn <- append(listTmn, crope)
    }
  }
}


# TMMC y TMMF en algebra de raster ----------------------------------------

rutas <- paste0("wc2.1_2.5m_prec_2010-2018/NormCrop/",
               "NormCroPprec_", rep(seq(2014,2018), each = 12),
               "-", rep(c(paste0("0",seq(1,9)),c(10,11,12)),5), ".tif")

listTmx2014 <- list(); listTmx2015 <- list(); listTmx2016 <- list(); listTmx2017 <- list(); listTmx2018 <- list()
listTmn2014 <- list(); listTmn2015 <- list(); listTmn2016 <- list(); listTmn2017 <- list(); listTmn2018 <- list()


for (j in 1:length(ptr)) {
  substr(rutas, 12, 15) <- ptr[j]
  substr(rutas, 44, 47) <- ptr[j]
  for(i in 1:length(rutas)) {
    clim <- raster(rutas[i])
    if (i <= 12) {
      if (ptr[j] == "tmax") {
        listTmx2014 <- append(listTmx2014, clim)
      }
      if (ptr[j] == "tmin") {
        listTmn2014 <- append(listTmn2014, clim)
      }
    }
    if (i > 12 & i <= 24) {
      if (ptr[j] == "tmax") {
        listTmx2015 <- append(listTmx2015, clim)
      }
      if (ptr[j] == "tmin") {
        listTmn2015 <- append(listTmn2015, clim)
      }
    }
    if (i > 24 & i <= 36) {
      if (ptr[j] == "tmax") {
        listTmx2016 <- append(listTmx2016, clim)
      }
      if (ptr[j] == "tmin") {
        listTmn2016 <- append(listTmn2016, clim)
      }
    }
    if (i > 36 & i <= 48) {
      if (ptr[j] == "tmax") {
        listTmx2017 <- append(listTmx2017, clim)
      }
      if (ptr[j] == "tmin") {
        listTmn2017 <- append(listTmn2017, clim)
      }
    }
    if (i > 48 & i <= 60) {
      if (ptr[j] == "tmax") {
        listTmx2018 <- append(listTmx2018, clim)
      }
      if (ptr[j] == "tmin") {
        listTmn2018 <- append(listTmn2018, clim)
      }
    }
  }
}

TMMC2014 <- max(brick(listTmx2014)); TMMC2015 <- max(brick(listTmx2015)); TMMC2016 <- max(brick(listTmx2016)); TMMC2017 <- max(brick(listTmx2017)); TMMC2018 <- max(brick(listTmx2018))
TMMF2014 <- min(brick(listTmn2014)); TMMF2015 <- min(brick(listTmn2015)); TMMF2016 <- min(brick(listTmn2016)); TMMF2017 <- min(brick(listTmn2017)); TMMF2018 <- min(brick(listTmn2018))
TMMCR <- list(TMMC2014, TMMC2015, TMMC2016, TMMC2017, TMMC2018)
TMMFR <- list(TMMF2014, TMMF2015, TMMF2016, TMMF2017, TMMF2018)

# Indices en raster -------------------------------------------------------------------------


RTemx <- list(A2014TX, A2015TX, A2016TX, A2017TX, A2018TX)
RTemn <- list(A2014TN, A2015TN, A2016TN, A2017TN, A2018TN)
RPrec <- list(A2014P, A2015P, A2016P, A2017P, A2018P)
años <- 2014:2018
indice <- c("AMartonne", "ContCurrey", "ContDaget", "TermoDatin")


for(j in 1:length(indice)){
  for (i in 1:length(años)) {
    if (indice[j] == "AMartonne") {
      AMartonne <- RPrec[[i]] / (((RTemx[[i]] + RTemn[[i]]) / 2) + 10)
      AMartonne <- resample(AMartonne, dem) # adicinal para resamplear a escala dem (30m) <-
      writeRaster(AMartonne, filename = paste0('Indices/AMartonne/FPrmRAMartonne', años[i], '.tif'))
      AMartonne[AMartonne[] < 5] <- 0.5
      AMartonne[AMartonne[] >= 5 & AMartonne[] < 10] <- 1
      AMartonne[AMartonne[] >= 10 & AMartonne[] < 20] <- 1.5
      AMartonne[AMartonne[] >= 20 & AMartonne[] < 30] <- 2
      AMartonne[AMartonne[] >= 30 & AMartonne[] < 40] <- 2.5
      AMartonne[AMartonne[] >= 40] <- 3
      writeRaster(AMartonne, filename = paste0('Indices/AMartonne/FPrmRClasAMartonne', años[i], '.tif'))
      # AMartonne <- mask(AMartonne, shp) # corte a usa resolución pequeña
      # writeRaster(AMartonne, filename = paste0('Indices/AMartonne/FextClipClasAMartonne', años[i], '.tif'))
      AMartonne <- mask(AMartonne, shp) # corte a usa resolución pequeña
      writeRaster(AMartonne, filename = paste0('Indices/AMartonne/FPrmRClipClasAMartonne', años[i], '.tif'))
    }
    if (indice[j] == "ContCurrey") {
      ContCurrey <- (RTemx[[i]] - RTemn[[i]]) / (1 + (1/3) * latitud) # Se quedo en promedios, pero puede cambiar
      ContCurrey <- resample(ContCurrey, dem)
      writeRaster(ContCurrey, filename = paste0('Indices/ContCurrey/FPrmRContCurrey', años[i], '.tif'))
      ContCurrey[ContCurrey[] < 0.6] <- 0.1
      ContCurrey[ContCurrey[] >= 0.6 & ContCurrey[] < 1.1] <- 0.15
      ContCurrey[ContCurrey[] >= 1.1 & ContCurrey[] < 1.7] <- 0.2
      ContCurrey[ContCurrey[] >= 1.7 & ContCurrey[] < 2.3] <- 0.25
      ContCurrey[ContCurrey[] >= 2.3 & ContCurrey[] < 5] <- 0.3
      ContCurrey[ContCurrey[] >= 5] <- 0.35
      writeRaster(ContCurrey, filename = paste0('Indices/ContCurrey/FPrmRClasContCurrey', años[i], '.tif'))
      ContCurrey <- mask(ContCurrey, shp)
      writeRaster(ContCurrey, filename = paste0('Indices/ContCurrey/FPrmRClipClasContCurrey', años[i], '.tif'))
    }
    if (indice[j] == "ContDaget") {
      ContDaget <- ((1.7 * (RTemx[[i]] - RTemn[[i]])) / sin((latitud + 10 + (9 * dem_rwd / 1000)) * (pi / 180))) - 14
      ContDaget <- resample(ContDaget, dem)
      writeRaster(ContDaget, filename = paste0('Indices/ContDaget/FPrmRContDaget', años[i], '.tif'))
      ContDaget[ContDaget[] < 25] <- 1
      ContDaget[ContCurrey[] >= 25] <- 2
      writeRaster(ContDaget, filename = paste0('Indices/ContDaget/FPrmRClasContDaget', años[i], '.tif'))
      ContDaget <- mask(ContDaget, shp)
      writeRaster(ContDaget, filename = paste0('Indices/ContDaget/FPrmRClipClasContDaget', años[i], '.tif'))
    }
    if (indice[j] == "TermoDatin") {
      TermoDantin <- 100 * ((RTemx[[i]] + RTemn[[i]]) / 2)/ RPrec[[i]]
      TermoDantin <- resample(TermoDantin, dem)
      writeRaster(TermoDantin, filename = paste0('Indices/TermoDatin/FPrmRTermoDatin', años[i], '.tif'))
      TermoDantin[TermoDantin[] < 2] <- 0.1
      TermoDantin[ContCurrey[] >= 2 & TermoDantin[] < 3] <- 0.2
      TermoDantin[TermoDantin[] >= 3 & TermoDantin[] < 5] <- 0.3
      TermoDantin[TermoDantin[] >= 5] <- 0.4
      writeRaster(TermoDantin, filename = paste0('Indices/TermoDatin/FPrmRClasTermoDatin', años[i], '.tif'))
      TermoDantin <- mask(TermoDantin, shp)
      writeRaster(TermoDantin, filename = paste0('Indices/TermoDatin/FPrmRClipClasTermoDantin', años[i], '.tif'))
    }
  }
}



# CHM_ov <- overlay(RTemx[[1]], RTemn[[1]], fun = function(r1, r2) { return( r1 / r2) })


# AMartonne = A2018P / (((A2018TX + A2018TN) / 2) + 10)
# ContCurrey2018 = (A2018TX - A2018TN) / (1 + (1/3) * latitud)
# ContDaget2018 = ((1.7 * (A2018TX - A2018TN)) / sin(lat + 10 + (9 * dem))) - 14
# TermoDantin2018 = 100 * ((A2018TX + A2018TN) / 2)/ A2018P
# 
# writeRaster(AMartonne2018, filename = 'Indices/AMartonne2018.tif')


# I. Aridez Martonne ------------------------------------------------------


# Corte crop y mask --------------------------------------------------------

dem_lima <- raster("dem/Lima_Callao_4326.tif") # dem lima
croplima <- crop(dem_lima, shp, snap = 'out') #crop con la extencion del dem
masklima <- mask(croplima, shp)
writeRaster(masklima, filename = paste0(getwd(),"/dem/altitud_mask_lima.tif"))



mkk = mask(c, shp)
writeRaster(mkk, filename = paste0(getwd(),"/wc2.1_2.5m_tmin_2010-2018/mask/dem.tif"))

tdem = resample(c, dem_lima)
writeRaster(tdem, filename = paste0(getwd(),"/wc2.1_2.5m_tmin_2010-2018/mask/temcrop.tif"))
tdemask <- mask(tdem, shp) # variable meteorologica enmascarada por lima
writeRaster(tdemask, filename = paste0(getwd(),"/wc2.1_2.5m_tmin_2010-2018/mask/temask.tif"))

dem_lima + tdemask


# altitud -----------------------------------------------------------------

altitud <- raster('dem/Lima_Callao_4326.tif')
crop_altitud <- crop(altitud, shp, snap = 'out')
writeRaster(crop_altitud, filename = paste0(getwd(),"/dem/altitud_crop_lima.tif"))


# latitud -----------------------------------------------------------------

latit <- raster('dem/latitudes.tif')
crop_latitud <- crop(latit, shp, snap = 'out') 
writeRaster(crop_latitud, filename = paste0(getwd(),"/dem/latitud_crop_lima.tif"))

# generar coordenadas de cada pixel

# elev_point_lima = rasterToPoints(dem_lima, spatial = TRUE) %>% 
#   st_as_sf()
# st_write(elev_)

# Corte de los datos del geoservidor --------------------------------------

rt <- list.files("geoservidor", pattern = ".shp$", full.names = T)
shlist <- sapply(rt, FUN = st_read)
shp1 <- st_transform(shp, crs = 32718)
geoservidor <- list()
for (i in 1:length(shlist)) {
  geoservidor[[i]] <- st_crop(st_transform(shlist[[i]], crs = 32718), shp1)
}

nm <- substring(rt, 13, nchar(rt) - 4)
for (i in 1:length(geoservidor)) {
  st_write(geoservidor[[i]], file.path(paste0(getwd(), "/geoservidor"), 'geoservidor.gpkg'), layer = nm[i], quiet = TRUE)
}

# 
# setwd('proyectosR') #establece directorio de trabajo
# library(rgdal)
# tiz<-readGDAL('tiznados_canoa.tif') #carga raster de interes
# coor<-coordinates(tiz) #extrae coordenadas x,y del raster
# long<-coor[,1] #extrae coordenada x
# lat<-coor[,2] #extrae coordenada y
# tiz_long<-tiz #raster de longitud
# tiz_long$band1<-long #sustitucion de values por longitud
# tiz_lat<-tiz #raster de latitud
# tiz_lat$band1<-lat #sustitucion de values por latitud
# writeGDAL(tiz_long,'tiz_long.tif',drivername='GTiff') #raster de longitud
# writeGDAL(tiz_lat,'tiz_lat.tif',drivername='GTiff') #raster de latitud