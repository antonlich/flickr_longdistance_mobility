# installation and loading of the packages available via cran ----
packagename<-c("RPostgreSQL","mapview","dbscan","ggplot2","ggmap","giscoR","sf","RJSONIO","foreign","remotes","geosphere","rworldmap","rworldxtra","questionr","countrycode")
if (length(setdiff(packagename, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packagename, rownames(installed.packages())),repos = "https://cran.uni-muenster.de/", Ncpus = 16)  
}

lapply(packagename, require, character.only = T)
rm(packagename)


# defining the country of interest in the analysis (that is to say the country in which the users should be residing) ----
cc<-"DE" # defining the countrycode (DE for Germany in this case)
cn<-"countryname" # providing the countryname in English 


# setting up the connection to the data  base (in this case a postgres data base but this can be changed if needed) ----
dbn<-"name of the data base"
hst<-"name or address of the hosting server"
pt<-"port number"
u<-"name of the user with access to the data base"
pw<-"password of the user"
con<-dbConnect(PostgreSQL(),dbname=dbn ,host=hst, port=pt,user=u, password=pw)

# loading the data set from the data base and ordering it by owner and date when the photo was taken ----
flickrall<-dbReadTable(con, "flickrall")
flickrall<-flickrall[with(flickrall, order(owner, datetaken)),]

# printing the number of users in the data set
length(unique(flickrall$owner))

# deleting those rows from the data set which do not contain the coordinates of a consecutive photo (the coordinates of the preceding photo will still be kept in the 
# preceding row as the coordinates of the consecitive photo in that row)
fral<-flickrall[is.na(flickrall$latnext)==F,]

# adding some temporal variables to the data set ----
# calculating the time difference between the dates taken of the two consecutive photos in each row
fral$duration_sec<-fral$datenext-fral$datetaken

# extracting the year and month of the dates on which the photos were taken into separate variables
fral$year<-as.numeric(format(fral$datetaken, format="%Y"))
fral$month<-as.numeric(format(fral$datetaken, format="%m"))

# identifying the coordinates of the place of residence of the flickr users ----
# correcting the special characters of the names of the cities in which the users are residing and their hometown which might be shown incorrectly in R (in this case
# the correction is focused on special characters occurring in the German language but this can be changed)
fral$city<-gsub("Ã¼","ue",fral$city)
fral$city<-gsub("Ã¶","oe",fral$city)
fral$city<-gsub("ÃŸ","ss",fral$city)
fral$hometown<-gsub("Ã¼","ue",fral$hometown)
fral$hometown<-gsub("Ã¶","oe",fral$hometown)
fral$hometown<-gsub("ÃŸ","ss",fral$hometown)

# retrieving the coordinates of the centroid of each city and hometown via the nominatim search engine in a while loop (a working internet connection is needed)
# first it is tried to get the coordinates of the city in which the users are residing, only if no city name was provided it is tried to get the coordinates of the hometown
test<-fral[duplicated(fral$owner)==F,]
test$city==""
nrow <- nrow(test)
counter <- 1
test$home_lon[counter] <- NA
test$home_lat[counter] <- NA
while (counter <= nrow){
  if(test$city[counter]!=""){
    CityName <- gsub(' ','%20',test$city[counter]) # remove space for URLs
    CountryCode <- cc
    url <- paste(
      "http://nominatim.openstreetmap.org/search?city="
      , CityName
      , "&countrycodes="
      , CountryCode
      , "&limit=9&format=json"
      , sep="")
    x <- fromJSON(url)
    if(is.vector(x)){
      test$home_lon[counter] <- x[[1]]$lon
      test$home_lat[counter] <- x[[1]]$lat    
    }
    counter <- counter + 1
  } else{if(test$hometown[counter]!=""){
    CityName <- gsub(' ','%20',test$hometown[counter]) # remove space for URLs
    CountryCode <- cc
    url <- paste(
      "http://nominatim.openstreetmap.org/search?city="
      , CityName
      , "&countrycodes="
      , CountryCode
      , "&limit=9&format=json"
      , sep="")
    x <- fromJSON(url)
    if(is.vector(x)){
      test$home_lon[counter] <- x[[1]]$lon
      test$home_lat[counter] <- x[[1]]$lat    
    }}
    counter <- counter + 1
  }
}

# checking for how many users no information on the city and hometown were provided 
table(test$city=="") 
table(test$hometown=="") 

# merging the coordinates of the centroid of the city of residence or the hometown to the general data set
fral<-merge(fral,test[,c("owner","home_lat","home_lon")],by="owner",all.x = T)
rm(test,x,CityName,counter,CountryCode,nrow,url)

# checking for how many rows coordinates of the centroid of the city of residence or the hometown could be identified
table(is.na(fral$home_lat)) 

# creation of a new data set only containing those users whose place of residence could be located within the borders of the country of interest ----
# retrieving the borders of the country of interest in 2021 (the year can be changed)
cn_bor<-gisco_get_nuts(year = "2021",epsg = "4326",cache = TRUE,update_cache = FALSE,cache_dir = NULL,verbose = FALSE,resolution = "20",spatialtype = "RG",country = cn,nuts_id = NULL,nuts_level = "0")

# transforming the coordinates of the photos in the flickr data set into the projection of the country borders (in this case epsg 4326 but this can be changed)
df = st_as_sf(fral, coords = c("longitude","latitude"), remove = FALSE, crs = 4326)

# checking which of the coordinates are within the borders of the country and which are not and merging this information back into the main data set
l<-st_intersects(df,cn_bor,sparse = F)
dfm<-data.frame(l)
fral<-cbind(fral,dfm)
fral$ind<-ifelse(fral$l==F,0,1)
fral$l<-NULL
rm(df,l,dfm)

# transforming the coordinates of the places of residence in the flickr data set into the projection of the country borders (in this case epsg 4326 but this can be changed)
df<-fral[is.na(fral$home_lat)==F,]
df = st_as_sf(df[duplicated(df$owner)==F,], coords = c("home_lon","home_lat"), remove = FALSE, crs = 4326)

# checking which of the coordinates are within the borders of the country and which are not and merging this information back into the main data set
l<-st_intersects(df,cn_bor,sparse = F)
dfm<-data.frame(l)
df<-cbind(df,dfm)
df$home_latlon_ind<-ifelse(df$l==F,0,1)
fral<-merge(fral,df[,c("owner","home_latlon_ind")], by="owner", all.x = T)

# checking how many of the places of residence fall within the borders of the country of interest
table(fral$home_latlon_ind, useNA = "always")
table(df$home_latlon_ind, useNA = "always") 
table(dfm$l==F, useNA = "always")
rm(df,l,dfm,cn_bor)

# creating a new data set with only those users whose place of residence could be located within the borders of the country of interest
fhom<-fral[fral$home_latlon_ind==1&is.na(fral$home_latlon_ind)==F,]

# spatial clustering of the coordinates of the photos uploaded per user ----
# spatial clustering of the coordinates of the photos uploaded per user with the parameters eps = 0.3 and minPts = 1 which gave good results for users residing in Germany but which can be changed if needed
# first a list is created in which each element contains the coordinates of the photos uploaded by a specific user 
all_us<-list()
for (i in 1:length(unique(fhom$owner))){
  all_us[[i]]<-fhom[fhom$owner==unique(fhom$owner)[i],]
}

# the coordinates of the photos uploaded are spatially clustered per user and the number of photos per cluster (nopc) is calculated
for (i in 1:length(all_us)){
  locs<-all_us[[i]][c("latitude","longitude")]
  db<-dbscan(locs, eps = 0.3, minPts = 1)
  all_us[[i]]["cluster"]<-db$cluster
  nc<-aggregate(duration_sec~cluster,all_us[[i]],NROW)
  nc$nopc<-nc$duration_sec
  all_us[[i]]<-merge(all_us[[i]],nc[,c("cluster","nopc")],by="cluster",all.x=T)
}

# all clusters found are merged back into the main data set
fhom<-do.call(rbind,all_us)
rm(locs,nc,db,all_us,i)

# visual inspection of the clusters found for different users
n<-1 # defining the row number by which different users can be selected
mapview(fhom[fhom$owner==fhom$owner[n],],xcol = "longitude", ycol = "latitude", crs = 4326, grid = FALSE,  zcol = "cluster")


# creation of a new data set with only those clusters that do not overlap with the 100km radius around the coordinates of the place of residence ----
# checking which of the clusters overlap with a 100km radius around the coordinates of the place of residence
fhom$ownclust<-paste0(fhom$owner,"_",fhom$cluster)
fhom$home_lat<-as.numeric(fhom$home_lat)
fhom$home_lon<-as.numeric(fhom$home_lon)

# a 100km radius (buffer) around the coordinates of the place of residence is drawn for each user
fhomrad<-fhom[duplicated(fhom$ownclust)==F,]
l<-list()
for (i in 1:nrow(fhomrad)){
  l[[i]]<-st_as_sf(fhomrad[i,], coords=c("home_lon","home_lat"))
  st_crs(l[[i]]) = 4326
  l[[i]]<-st_transform(l[[i]], "+proj=utm +zone=18 +units=m")
  l[[i]]<-st_buffer(l[[i]], 100000)
}

# in the next step the coordinates of each cluster are transformed into a spatial object depending on the number of photos in the cluster
# extracting the clusters containing four or more coordinates as these will be transformed into polygons
clustd_pol<-fhom[fhom$nopc>=4,]
# extracting the clusters containing two or three coordinates as these will be transformed into bounding boxes
clustd_bb<-fhom[fhom$nopc==2|fhom$nopc==3,] 
# extracting the clusters containing only one pair of coordinates as these will be transformed into points 
clustd_rad<-fhom[fhom$nopc==1,] 

# transforming the selected clusters into polygons 
df = st_as_sf(clustd_pol, coords=c("longitude","latitude"))
polys = st_transform(st_sf(aggregate(df$geometry,list(df$ownclust),function(g){st_cast(st_combine(g),"POLYGON")}),crs = 4326), "+proj=utm +zone=18 +units=m")
st_geometry(polys) <- st_convex_hull(polys$geometry)

# checking which of the polygons overlaps with the 100km radius around the place of residence and transferring this information back into the polygon data set
d<-list()
for (h in 1:nrow(polys)){
  for (i in 1:length(l)){
    if(l[[i]]$ownclust==polys$Group.1[h]){
      d[[h]]<-st_intersects(l[[i]],polys[polys$Group.1==polys$Group.1[h],],sparse = T)
    }
  }  
}

for (i in 1:length(d)) {
  if(is.null(d[[i]])){
    d[[i]]<-d[[1]]
  }
}

dfm<-do.call(rbind,d)
dfm<-as.data.frame(dfm)
dfm$V1<-ifelse(dfm$V1==1,1,0)
dfm$V1<-ifelse(is.na(dfm$V1),0,dfm$V1)

polys<-cbind(polys,dfm)
rm(d,dfm,h,i,df)
polys$ownclust<-polys$Group.1
polys<-st_drop_geometry(polys)
clustd_pol<-merge(clustd_pol,polys[,c("ownclust","V1")], by="ownclust", all.x = T)

# transforming the selected clusters into bounding boxes 
df = st_as_sf(clustd_bb, coords=c("longitude","latitude"), crs = 4326)
polys = st_transform(st_sf(aggregate(df$geometry,list(df$ownclust),function(g){st_as_sfc(st_bbox(g))}),crs = 4326), "+proj=utm +zone=18 +units=m")

# checking which of the bounding boxes overlaps with the 100km radius around the place of residence and transferring this information back into the bounding box data set
d<-list()
for (h in 1:nrow(polys)){
  for (i in 1:length(l)){
    if(l[[i]]$ownclust==polys$Group.1[h]){
      d[[h]]<-st_intersects(l[[i]],polys[polys$Group.1==polys$Group.1[h],],sparse = T)
    }
  }  
}
rm(i)
for (i in 1:length(d)) {
  if(is.null(d[[i]])){
    d[[i]]<-d[[2]]
  }
}

dfm<-do.call(rbind,d)
dfm<-as.data.frame(dfm)
dfm$V1<-ifelse(dfm$V1==1,1,0)
dfm$V1<-ifelse(is.na(dfm$V1),0,dfm$V1)

polys<-cbind(polys,dfm)

rm(d,dfm,h,i,df)
polys$ownclust<-polys$Group.1
polys<-st_drop_geometry(polys)
clustd_bb<-merge(clustd_bb,polys[,c("ownclust","V1")], by="ownclust", all.x = T)

# transforming the selected clusters with only one pair of coordinates into points 
clustd_rad$ownclust<-paste0(clustd_rad$owner,"_",clustd_rad$cluster)
length(unique(clustd_rad$ownclust))

# drawing a radius (buffer) of 100km round the points and checking if it overlaps with the 100km radius around the place of residence and transferring this information back into the points data set
g<-list()
for (i in 1:nrow(clustd_rad)){
  g[[i]]<-st_as_sf(clustd_rad[i,], coords=c("longitude","latitude"))
  st_crs(g[[i]]) = 4326
  g[[i]]<-st_transform(g[[i]], "+proj=utm +zone=18 +units=m")
  g[[i]]<-st_buffer(g[[i]], 100000)
}


d<-list()
for (h in 1:length(g)){
  for (i in 1:length(l)){
    if(l[[i]]$ownclust==g[[h]]$ownclust){
      d[[h]]<-st_intersects(l[[i]],g[[h]],sparse = T)
    }
  }  
}

dfm<-do.call(rbind,d)
dfm<-as.data.frame(dfm)
dfm$V1<-ifelse(dfm$V1==1,1,0)
dfm$V1<-ifelse(is.na(dfm$V1),0,dfm$V1)

m<-do.call(rbind,g)
m<-cbind(m,dfm)
m<-st_drop_geometry(m)
clustd_rad<-merge(clustd_rad,m[,c("ownclust","V1")], by="ownclust", all.x = T)
rm(g,l,m,polys,h,i,d,dfm)

# creating a new data set with only those clusters that do not overlap with the 100km radius around the coordinates of the place of residence
fnohom<-rbind(clustd_pol[clustd_pol$V1==0,],clustd_bb[clustd_bb$V1==0,],clustd_rad[clustd_rad$V1==0,])

# calculation of the distance between the clusters and the places of residence ----
# calculating the distance as the crow flies between the coordinates of the photos uploaded to the coordinates of the place of residence for each user
for (i in 1:nrow(fnohom)){
  fnohom$di_home[i]<-distm(c(fnohom$longitude[i],fnohom$latitude[i]),c(fnohom$home_lon[i],fnohom$home_lat[i]), fun = distHaversine) 
}

# calculating the mean distance to the place of residence among all distances per cluster and merging the information back into the main data set
df<-aggregate(di_home ~ ownclust,fnohom,mean)
df$di_home_clusmean<-df$di_home
fnohom<-merge(fnohom,df[,c("ownclust","di_home_clusmean")], by ="ownclust", all.x = T)
rm(df)

# temporal separation of the clusters ----
# temporal separation of the clusters according to the following two rules:
# 1) if the mean distance of a cluster to the place of residence is 750km or less and the time passed between two consecutive photos in the cluster is longer than 30 days, then the photos before and after that time period are separated into different clusters
# 2) if the mean distance of a cluster to the place of residence is larger than 750km and the time passed between two consecutive photos in the cluster is longer than 90 days, then the photos before and after that time period are separated into different clusters
df<-data.frame()
df1<-data.frame()

for (i in 1:length(unique(fnohom$ownclust))){
  df<-fnohom[fnohom$ownclust==unique(fnohom$ownclust)[i],]
  df<-df[order(df$datetaken),]
  df$clustnew<-0
  df$clustnew2<-0
  h<-nrow(df)
  if(h>1){
    for (j in 2:h){
      df$clustnew[j]<-ifelse(difftime(df$datetaken[j],df$datetaken[j-1], units = "days")>30&df$di_home_clusmean[j]<750000,j-1,                              
                             ifelse(difftime(df$datetaken[j],df$datetaken[j-1], units = "days")>90&df$di_home_clusmean[j]>=750000,j-1,df$clustnew[j])) 
    }
    if(length(unique(df$clustnew))>1){
      for(m in 2:h){
        df$clustnew2[m]<-ifelse(df$clustnew[m]!=0&df$clustnew2[m]==0,df$clustnew[m],
                                ifelse(df$clustnew[m-1]!=0&df$clustnew2[m]==0,df$clustnew[m-1],
                                       ifelse(df$clustnew2[m-1]!=0&df$clustnew2[m]==0,df$clustnew2[m-1],df$clustnew2)))
      }
    }
  }
  df1<-rbind(df1,df)
}

# if everything went as it should, then the main data set is replaced by the new data set containing the information on temporal cluster separation
if(nrow(fnohom)==nrow(df1)){
  fnohom<-df1
  fnohom$ownclust_new<-paste0(fnohom$ownclust,"_",fnohom$clustnew2)
  fnohom$clustnew2<-fnohom$clustnew<-NULL
  rm(df,df1,h,i,j,m)
}

# calculation the number of clusters per user and merging the information back to the main data set
df<-aggregate(ownclust_new~owner,fnohom,NROW)
df$noc<-df$ownclust_new
fnohom<-merge(fnohom,df[c("owner","noc")],by="owner",all.x = T)
rm(df)

# calculating the mean distance of the coordinates of the photos of the new clusters to the coordinates of the place of residence and merging this information back to the main data set
df<-aggregate(di_home ~ ownclust_new,fnohom,mean)
df$di_home_clusmean_new<-df$di_home
fnohom<-merge(fnohom,df[,c("ownclust_new","di_home_clusmean_new")], by ="ownclust_new", all.x = T)
rm(df)

# locating the coordinates of the photos uploaded in the respective country ----
# retrieving the borders of all countries (in this case in the year 2020 but this can be changed)
worldstates<-gisco_get_countries(year = "2020",epsg = "4326",cache = TRUE,update_cache = FALSE,cache_dir = NULL,verbose = FALSE,resolution = "20",spatialtype = "RG",country = NULL)

# locating the coordinates of each photo in the respective country and merging this information back to the main data set
l<-as.character()
for (i in 1:nrow(fnohom)){
  l[i]<-st_join(st_as_sf(fnohom[i,c("longitude","latitude")], coords=c("longitude","latitude"), crs = 4326),worldstates, join = st_nearest_feature)$NAME_ENGL
}
fnohom<-cbind(fnohom,l)
fnohom$dest_country<-fnohom$l
fnohom$l<-NULL

# locating the coordinates of each photo on the respective continent 
fnohom$continent<-countrycode(sourcevar = fnohom[,"dest_country"], origin = "country.name", destination = "continent")

# locating the coordinates of each photo in the respective region of the world (two different world region categorizations are used)
fnohom$region<-countrycode(sourcevar = fnohom[,"dest_country"], origin = "country.name", destination = "region")
fnohom$region23<-countrycode(sourcevar = fnohom[,"dest_country"], origin = "country.name", destination = "region23")

# calculating the time spend at each cluster of locations ----
# calculation of the time spend (in days) at each cluster of locations on the basis of the date of the first and the last photo of each cluster 
df<-data.frame()
df1<-data.frame()

for (i in 1:length(unique(fnohom$ownclust_new))){
  df<-fnohom[fnohom$ownclust_new==unique(fnohom$ownclust_new)[i],]
  df<-df[order(df$datetaken),]
  df$hol_dur<-0
  h<-nrow(df)
  if(h>1){
    df$hol_dur<-difftime(df$datetaken[h],df$datetaken[1], units = "days")
  }
  df1<-rbind(df1,df)
}

# if everything went as it should, then the main data set is replaced by the new data set containing the information on the time spend at each cluster of locations
if(nrow(fnohom)==nrow(df1)){
  fnohom<-df1
  rm(df,df1,h,i,l)
}

# exporting the final data set into the data base ----
con<-dbConnect(PostgreSQL(),dbname=dbn ,host=hst, port=pt,user=u, password=pw)
dbWriteTable(con,"fnohom",fnohom, row.names=FALSE, overwrite=T)
