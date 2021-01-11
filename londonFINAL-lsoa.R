install.packages("tmap")
install.packages(c("geojsonio","plotly","rgdal","broom","mapview","crosstalk","sf","sp","spdep","car","fs","janitor","corrr"))
library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
#read data
shapefile<- st_read(here::here("lcalldata2.shp"))
qtm(shapefile)

#Clean data
CrimeProfiles <- read_csv(here::here("2019-07-crime.csv"), 
                          na = c("", "NA", "n/a"), 
                               col_names = TRUE, 
                               locale = locale(encoding = 'Latin1'))
Datatypelist <- CrimeProfiles %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

#plot
q <- qplot(x = `Avg_Educat`, 
           y = `Count_1`, 
           data=shapefile)

#EASY PLOT plot with a regression line - note, I've added some jitter here as the x-scale is rounded
q + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()


# linear regression
Regressiondata<- shapefile%>%
  dplyr::select(Count_1,
                Avg_Educat)

model1 <- Regressiondata %>%
  lm(Count_1 ~
       Avg_Educat,
     data=.)

summary(model1)
#delete

#real start
myvars <- shapefile %>%
dplyr::select(Avg_Educat,
              Avg_Health,
              Avg_Employ,
              Avg_Occupa,
              Count_1)
library(corrr)
Correlation_myvars <- myvars %>%
  st_drop_geometry()%>%
  dplyr::select(-Count_1)%>%
  correlate()

Correlation_myvars <- myvars %>%
  st_drop_geometry()%>%
  dplyr::select(-Count_1)%>%
  correlate()

model_final <- lm(Count_1 ~ Avg_Educat + Avg_Health + Avg_Employ + Avg_Occupa,
                  data = myvars)

tidy(model_final)


shapefile <- shapefile %>%
  mutate(model_final_res = residuals(model_final))

par(mfrow=c(2,2))
plot(model_final)

#plot 4 pic
qtm(shapefile, fill = "model_final_res")

#knn
coordsW <- shapefile%>%
  st_centroid()%>%
  st_geometry()
plot(coordsW)
knn_lsoa <-coordsW %>%
  knearneigh(., k=4)
lc_knn <- knn_lsoa %>%
  knn2nb()
plot(lc_knn, st_geometry(coordsW), col="green")
lc.knn_4_weight <- lc_knn %>%
  nb2listw(., style="C")


final_model_Moran <- shapefile %>%
  st_drop_geometry()%>%
  dplyr::select(model_final_res)%>%
  pull()%>%
  moran.test(., lc.knn_4_weight)%>%
  tidy()

final_model_Moran

#GWR
install.packages("spgwr")
library(spgwr)

st_crs(shapefile) = 27700
shapefileSP <- shapefile %>%
  as(., "Spatial")
st_crs(coordsW) = 27700
coordsWSP <- coordsW %>%
  as(., "Spatial")

GWRbandwidth <- gwr.sel(Count_1 ~ Avg_Educat + Avg_Health + Avg_Employ + Avg_Occupa, 
                        data = shapefileSP, 
                        coords=coordsWSP,
                        adapt=T)

gwr.model = gwr(Count_1 ~ Avg_Educat + Avg_Health + Avg_Employ + Avg_Occupa, 
                data = shapefileSP, 
                coords=coordsWSP, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)
gwr.model

#clear

results <- as.data.frame(gwr.model$SDF)
names(results)

shapefile2 <- shapefile %>%
  mutate(coefedu = results$Avg_Educat,
         coefhealth = results$Avg_Health,
         coefemploy = results$Avg_Employ,
         coefoccu = results$Avg_Occupa)
#plot GWR
tm_shape(shapefile2) +
  tm_polygons(col = "coefedu", 
              palette = "RdBu", 
              alpha = 0.5)

tm_shape(shapefile2) +
  tm_polygons(col = "coefhealth", 
              palette = "RdBu", 
              alpha = 0.5)

tm_shape(shapefile2) +
  tm_polygons(col = "coefemploy", 
              palette = "RdBu", 
              alpha = 0.5)

tm_shape(shapefile2) +
  tm_polygons(col = "coefoccu", 
              palette = "RdBu", 
              alpha = 0.5)

sigTest1 = abs(gwr.model$SDF$"Avg_Educat")-2 * gwr.model$SDF$"Avg_Educat"
shapefile3 <- shapefile2 %>%
  mutate(GWREduSig = sigTest)

#sig test
tm_shape(shapefile2) +
  tm_polygons(col = "GWREduSig", 
              palette = "RdYlBu")




