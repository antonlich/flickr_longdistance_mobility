# installation and loading of the packages available via cran ----
packagename<-c("RPostgreSQL","mapview","dbscan","ggplot2","ggmap","giscoR","sf","RJSONIO","foreign","remotes","geosphere","rworldmap","rworldxtra","questionr","countrycode")
if (length(setdiff(packagename, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packagename, rownames(installed.packages())),repos = "https://cran.uni-muenster.de/", Ncpus = 16)  
}

lapply(packagename, require, character.only = T)
rm(packagename)

# setting up the connection to the data  base (in this case a postgres data base but this can be changed if needed) ----
dbn<-"name of the data base"
hst<-"name or address of the hosting server"
pt<-"port number"
u<-"name of the user with access to the data base"
pw<-"password of the user"
con<-dbConnect(PostgreSQL(),dbname=dbn ,host=hst, port=pt,user=u, password=pw)

# loading the data set from the data base
fnohom<-dbReadTable(con, "fnohom")

# loading the long-distance data set of the Mobility in Germany 2017 survey ----
pt<-"local path to the long-distance data set"
r<-as.data.frame(read.spss(paste0(pt,"MiD2017_Reisen.sav"),use.value.labels=FALSE))

# descriptive analyses of the long-distance data set to get a better understanding of it ----
# tabulating the number of long-distance trips per person by trip id (there is maximum of three trips per person)
table(r$R_ID)

# calculating the number of individuals in the long-distance data set
length(unique(r$HP_ID))

# tabulating the distances traveled in groups
table(r$entf_gr)
prop.table(table(r$entf_gr))
# the classification is as follows: 1 - less than 100km, 2 - 100 to less than 200km, 3 - 200 to less than 300km, 4 - 300 to less than 400km, 5 - 400 to less than 500km, 6 - 500 to less than 750km, 7 - 750 to less than 1000km, 8 - 1000 to less than 2000km, 9 - 2000km and longer

# tabulating the number of overnight stays in groups
prop.table(table(r$anzueb_gr)) 
# the classification is as follows: 1 - 1, 2 - 2, 3 - 3, 4 - 4 to 5, 5 - 6 to 7, 6 - 8 to 14, 7 - 15 and more

# tabulating the non-categorized number of overnights stays (showing a maximum of 54)
table(r$R_ANZUEB) 

# crosstabulating the distances traveled and the number of overnight stays (showing a strong relative increase in the share of overnight stays on trips longer than 750km)
table(r$entf_gr, r$anzueb_gr)
prop.table(table(r$entf_gr, r$anzueb_gr),1)

# tabulating the trip purposes
table(r$R_ZWECK)
prop.table(table(r$R_ZWECK))
# the coding is as follows: 1 - holiday, 2 - visiting friends or relatives, 3 - other private travel, 4 - business trip, 5 - other, 9 - no information provided

# tabulating the travel destinations
table(r$R_ZIEL)
prop.table(table(r$R_ZIEL))
# the coding is as follows: 1 - within Germany, 2 - Withe the rest of Europe, 3 - Outside of Europe, 9 - no information provided

# tabulating the share of the travel destinations by trip purpose
prop.table(table(r$R_ZIEL[r$R_ZWECK==1]))
prop.table(table(r$R_ZIEL[r$R_ZWECK==2]))
prop.table(table(r$R_ZIEL[r$R_ZWECK==4]))
prop.table(table(r$R_ZIEL[r$R_ZWECK==3]))
prop.table(table(r$R_ZIEL[r$R_ZWECK==5]))

# preparing the flickr data set for comparisons with the long-distance data set of the Mobility in Germany 2017 survey ----
# creating a variable for the clusters of locations in the flickr data set with the same categories as the travel destination in the long-distance data set of the Mobility in Germany 2017 survey
fnohom$region_ger<-ifelse(fnohom$dest_country=="Germany","Germany",fnohom$region)
fnohom$region_mid<-ifelse(fnohom$dest_country=="Germany","Germany",
                             ifelse(fnohom$region23%in%c("Northern Europe","Eastern Europe","Western Europe","Southern Europe"),"Europe","Outside of Europe"))

# creating a variable for the time spend at each cluster of locations in the flickr data set with the same categories as the number of overnight stays in the long-distance data set of the Mobility in Germany 2017 survey
fnohom$hol_dur_mid<-ifelse(fnohom$hol_dur==0,0,
                              ifelse(fnohom$hol_dur<=1,1,
                                     ifelse(fnohom$hol_dur<=2,2,
                                            ifelse(fnohom$hol_dur<=3,3,
                                                   ifelse(fnohom$hol_dur<=5,4,
                                                          ifelse(fnohom$hol_dur<=7,5,
                                                                 ifelse(fnohom$hol_dur<=14,6,7)))))))

# creating a variable for the distances traveled to each cluster of locations in the flickr data set with the same categories as the distances traveled in the long-distance data set of the Mobility in Germany 2017 survey
fnohom$di_home_mid<-ifelse(fnohom$di_home_clusmean_new<100000,1,
                              ifelse(fnohom$di_home_clusmean_new<200000,2,
                                     ifelse(fnohom$di_home_clusmean_new<300000,3,
                                            ifelse(fnohom$di_home_clusmean_new<400000,4,
                                                   ifelse(fnohom$di_home_clusmean_new<500000,5,
                                                          ifelse(fnohom$di_home_clusmean_new<750000,6,
                                                                 ifelse(fnohom$di_home_clusmean_new<1000000,7,
                                                                        ifelse(fnohom$di_home_clusmean_new<2000000,8,9))))))))

# creating a data set with one row for each cluster in the flickr data set for an easier comparison of the distances traveled etc. with the long-distance data set of the Mobility in Germany 2017 survey
fclusun<-fnohom[duplicated(paste0(fnohom$ownclust_new,"_",fnohom$dest_country))==F,]

# comparing the flickr data set with the long-distance data set of the Mobility in Germany 2017 survey on the basis of various indicators ----
# creating a variable for the clusters of locations in the flickr data set with the same categories as the travel destination in the long-distance data set of the Mobility in Germany 2017 survey
fnohom$region_ger<-ifelse(fnohom$dest_country=="Germany","Germany",fnohom$region)
fnohom$region_mid<-ifelse(fnohom$dest_country=="Germany","Germany",
                             ifelse(fnohom$region23%in%c("Northern Europe","Eastern Europe","Western Europe","Southern Europe"),"Europe","Outside of Europe"))

# creating a variable for the time spend at each cluster of locations in the flickr data set with the same categories as the number of overnight stays in the long-distance data set of the Mobility in Germany 2017 survey
fnohom$hol_dur_mid<-ifelse(fnohom$hol_dur==0,0,
                              ifelse(fnohom$hol_dur<=1,1,
                                     ifelse(fnohom$hol_dur<=2,2,
                                            ifelse(fnohom$hol_dur<=3,3,
                                                   ifelse(fnohom$hol_dur<=5,4,
                                                          ifelse(fnohom$hol_dur<=7,5,
                                                                 ifelse(fnohom$hol_dur<=14,6,7)))))))

# creating a variable for the distances traveled to each cluster of locations in the flickr data set with the same categories as the distances traveled in the long-distance data set of the Mobility in Germany 2017 survey
fnohom$di_home_mid<-ifelse(fnohom$di_home_clusmean_new<100000,1,
                              ifelse(fnohom$di_home_clusmean_new<200000,2,
                                     ifelse(fnohom$di_home_clusmean_new<300000,3,
                                            ifelse(fnohom$di_home_clusmean_new<400000,4,
                                                   ifelse(fnohom$di_home_clusmean_new<500000,5,
                                                          ifelse(fnohom$di_home_clusmean_new<750000,6,
                                                                 ifelse(fnohom$di_home_clusmean_new<1000000,7,
                                                                        ifelse(fnohom$di_home_clusmean_new<2000000,8,9))))))))

# creating a data set with one row for each cluster in the flickr data set for an easier comparison of the distances traveled etc. with the long-distance data set of the Mobility in Germany 2017 survey
fclusun<-fnohom[duplicated(paste0(fnohom$ownclust_new,"_",fnohom$dest_country))==F,]

# comparison of the distances traveled in the flickr and the long-distance data set
round(prop.table(table(fclusun$di_home_mid[fclusun$year==2017])),2)
round(prop.table(wtd.table(r$entf_gr[r$entf_gr<90&r$R_ZWECK==1], weights = r$R_GEW[r$entf_gr<90&r$R_ZWECK==1])),2) 

# plotting the distance comparison in a grouped bar chart
df<-data.frame(round(prop.table(wtd.table(r$entf_gr[r$entf_gr<90&r$R_ZWECK==1], weights = r$R_GEW[r$entf_gr<90&r$R_ZWECK==1])),2))
df$source<-"MiD 2017"
df1<-data.frame(round(prop.table(table(fclusun$di_home_mid[fclusun$year==2017])),2))
df1$source<-"Flickr"
df<-rbind(df,df1)
df$dist<-as.numeric(df$Var1)
df$dist_fac<-factor(df$dist, levels = c(1:9), labels = c("<100","100 - <200","200 - <300", "300 - <400","400 - <500","500 - <750","750 - <1,000", "1,000 - <2,000", ">=2,000"))
rm(df1)

ggplot(df, aes(x=dist_fac,y=Freq,fill=source)) + geom_bar(position = position_dodge(),stat = "identity") + labs(x="\nDistance (in km)",y="Share of all trips\n",fill="Source") + 
  scale_y_continuous(labels = scales::percent)+theme(axis.title = element_text(size=14), axis.text = element_text(size = 14))

# comparison of the destinations of the clusters in the flickr data set in 2017 and the Mobility in Germany 2017 survey by region of the world 
round(prop.table(table(fclusun$region_mid[fclusun$year==2017])),2)
round(prop.table(wtd.table(r$R_ZIEL[r$R_ZWECK==1&r$R_ZIEL<9], weights = r$R_GEW[r$R_ZWECK==1&r$R_ZIEL<9])),2)

# comparison of the time spend in the clusters in the flickr data set and the number of overnight stays on the ling-distance trips of the Mobility in Germany 2017 survey
prop.table(table(r$anzueb_gr)) 
prop.table(table(fclusun$hol_dur_mid))
prop.table(table(fclusun$hol_dur_mid[fclusun$hol_dur_mid>0]))
round(prop.table(wtd.table(r$anzueb_gr[r$R_ZWECK==1&r$anzueb_gr<90], weights = r$R_GEW[r$R_ZWECK==1&r$anzueb_gr<90])),2)
round(prop.table(table(fclusun$hol_dur_mid[fclusun$hol_dur_mid>0&fclusun$year==2017])),2)
round(prop.table(table(fclusun$hol_dur_mid[fclusun$hol_dur_mid>0&fclusun$year==2018])),2)
round(prop.table(table(fclusun$hol_dur_mid[fclusun$hol_dur_mid>0&fclusun$year==2019])),2)
round(prop.table(table(fclusun$hol_dur_mid[fclusun$hol_dur_mid>0&fclusun$year==2020])),2)
round(prop.table(table(fclusun$hol_dur_mid[fclusun$hol_dur_mid>=2&fclusun$year==2017])),2)

# analyses of the changes in the kilometres traveled per year ----
round(prop.table(table(fclusun$di_home_mid[fclusun$year==2017])),2)
round(prop.table(table(fclusun$di_home_mid[fclusun$year==2018])),2)
round(prop.table(table(fclusun$di_home_mid[fclusun$year==2019])),2)
round(prop.table(table(fclusun$di_home_mid[fclusun$year==2020])),2)
round(prop.table(table(fclusun$di_home_mid[fclusun$year==2020&fclusun$month>=3])),2)
round(prop.table(table(fclusun$di_home_mid[fclusun$year==2021])),2)

# comparison of the distances traveled in 2019 and 2020 after march onwards in the flickr data set
sum(fclusun$di_home_clusmean_new[fclusun$year==2019&fclusun$month>=3])
sum(fclusun$di_home_clusmean_new[fclusun$year==2020&fclusun$month>=3])
(sum(fclusun$di_home_clusmean_new[fclusun$year==2020&fclusun$month>=3])-sum(fclusun$di_home_clusmean_new[fclusun$year==2019&fclusun$month>=3]))/sum(fclusun$di_home_clusmean_new[fclusun$year==2019&fclusun$month>=3]) # 59 prozent geringere verkehrsleistung

# plotting the relative changes in the distances traveled from 2019 to 2020 in the flickr data set and in the transport in figures statistics (the value of the later is inserted manually)
df<-data.frame(Source=c("Flickr","Transport\nin\nFigures"),Freq=c(round((sum(fclusun$di_home_clusmean_new[fclusun$year==2020&fclusun$month>=3])-sum(fclusun$di_home_clusmean_new[fclusun$year==2019&fclusun$month>=3]))/sum(fclusun$di_home_clusmean_new[fclusun$year==2019&fclusun$month>=3]),2),-0.47))
ggplot(df, aes(x=Source,y=Freq))+ geom_bar(stat="identity") + 
  scale_y_continuous(labels = scales::percent)+theme(axis.title = element_text(size=18), axis.text = element_text(size = 18))+ labs(y="Relative change in the distances travelled\n")

# plotting the different locations of the photos in the flickr data set in 2019 and 2020
mapview(fnohom[fnohom$year==2019&fnohom$month>3,], xcol = "longitude", ycol = "latitude", crs = 4326, grid = FALSE)
mapview(fnohom[fnohom$year==2020&fnohom$month>3,], xcol = "longitude", ycol = "latitude", crs = 4326, grid = FALSE)

# comparison of the flickr data set with the official air travel statistics of the Statistical Office of Germany ----
# loading the prepared data set with the officual air travel statistics of the Statistical Office of Germany
pt<-"local path to the prepared data set of the official air travel statistics"
fligths<-read.csv(paste0(pt,"2017_2021_prepared.csv"),sep=";")
fligths$counts<-as.numeric(fligths$counts)

# locating the countries in the flights data set on the respective continent
fligths$continent<-countrycode(sourcevar = fligths[,"country"], origin = "country.name", destination = "continent")

# locating the countries in the flights data set in the respective region of the world
fligths$region<-countrycode(sourcevar = fligths[,"country"], origin = "country.name", destination = "region")
fligths$region23<-countrycode(sourcevar = fligths[,"country"], origin = "country.name", destination = "region23")

# creating a variable for the clusters of locations in the flickr data set with the same categories as the travel destination in the long-distance data set of the Mobility in Germany 2017 survey
fligths$region_ger<-ifelse(fligths$country=="Germany","Germany",fligths$region)
fligths$region_mid<-ifelse(fligths$country=="Germany","Germany",
                           ifelse(fligths$region23%in%c("Northern Europe","Eastern Europe","Western Europe","Southern Europe"),"Europe","Outside of Europe"))

# calculating the shares of each destination country per year in the flights data set containing the official air traffic statistics for Germany
df<-aggregate(counts~region_mid+year,fligths,sum)
df<-merge(df,aggregate(counts~year,df,sum),by="year",all.x=T)
df$counts_reg_year<-df$counts.x
df$counts_year<-df$counts.y
df$counts.y<-df$counts.x<-NULL
df$counts_share_year<-df$counts_reg_year/df$counts_year

# figure for the comparison of air traffic statistics and flickr data
df1<-df[df$year<2020,c("year","region_mid","counts_share_year")]
df2<-rbind(data.frame(round(prop.table(table(fclusun$region_mid[fclusun$year==2017])),2),year=2017),
           data.frame(round(prop.table(table(fclusun$region_mid[fclusun$year==2018])),2),year=2018),
           data.frame(round(prop.table(table(fclusun$region_mid[fclusun$year==2019])),2),year=2019))
df1$Freq<-round(df1$counts_share_year,2)
df1$counts_share_year<-NULL
df2$region_mid<-df2$Var1
df2$Var1<-NULL
df1$Source<-"Official\nair traffic\nstatistics"
df2$Source<-"Flickr"
df1<-rbind(df1,df2)

df1$region<-ifelse(df1$region_mid=="Germany",1,
                   ifelse(df1$region_mid=="Europe",2,3))
df1$region_fac<-factor(df1$region, levels = c(1:3), labels = c("Germany","Europe","Outside\nof\nEurope"))

ggplot(df1, aes(x=region_fac,y=Freq,fill=Source)) + geom_bar(position = position_dodge(),stat = "identity") + labs(x="\nRegion of destination",y="Share of all trips\n",fill="Source") + 
  scale_y_continuous(labels = scales::percent)+theme(axis.title = element_text(size=18), axis.text = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_text(size = 18),
                                                     strip.text.x = element_text(size = 18)) + facet_grid(~year)

# calculating the radius of movement in the clusters of locations in the flickr data set ----
# first the number of locations per cluster is calculated and merged back into the main data set
df<-aggregate(owner~ownclust_new,fnohom,NROW)
df$noppc_new<-df$owner
fnohom<-merge(fnohom,df[,c("ownclust_new","noppc_new")],by="ownclust_new",all.x=T)

# only those clusters with at least four locations are selected and saved in a new data set
fpol<-fnohom[fnohom$noppc_new>=4,]

# transforming the clusters into polygons
df = st_as_sf(fpol, coords=c("longitude","latitude"))
polys = st_transform(st_sf(aggregate(df$geometry,list(df$ownclust_new),function(g){st_cast(st_combine(g),"POLYGON")}),crs = 4326), "+proj=utm +zone=18 +units=m")
st_geometry(polys) <- st_convex_hull(polys$geometry)

# calculating the centroid of each polygon
polys$cent<-st_centroid(polys)
polys$di_cent<-st_distance(polys$geometry,polys$cent)
df<-st_centroid(polys)

# extracting the latitude and longitude coordinates of the centroids and merging them back into the main data set
df$cent<-st_transform(df$geometry, "+proj=longlat +zone=18 +units=m")
for(i in 1:nrow(df)){
  df$long_cent[[i]]<-df$cent[[i]][1]
  df$lat_cent[[i]]<-df$cent[[i]][2]
}
df$lat_cent<-as.numeric(df$lat_cent)
df$long_cent<-as.numeric(df$long_cent)
df$ownclust_new<-df$Group.1
df$cent<-df$lat<-df$long<-NULL
df<-st_drop_geometry(df)
fpol<-merge(fpol,df[,c("ownclust_new","lat_cent","long_cent")],by="ownclust_new", all.x=T)

# calculating the distances as the crow flies between the coordinates of the photos of each cluster and the cluster centroid
for (i in 1:nrow(fpol)){
  fpol$dist_cent[i]<-distm(c(fpol[i,"longitude"],fpol[i,"latitude"]),c(fpol[i,"long_cent"],fpol[i,"lat_cent"]), fun = distHaversine)
}

# calculating the maximum distance as the crow flies between the coordinates of the photos of each cluster and the cluster centroid and merging this information back into the main data set
df<-aggregate(dist_cent~ownclust_new,fpol,max)
df$dist_cent_max<-df$dist_cent
fpol<-merge(fpol,df[c("ownclust_new","dist_cent_max")],by="ownclust_new",all.x=T)

# creating a new data set with only one cluster per row
fpolmax<-fpol[duplicated(fpol$ownclust_new)==F,]

# plotting the radius of movement in the clusters of the flickr data set ----
df<-data.frame(dist=c(1:6),share=c(round(prop.table(table(fpolmax$dist_cent_max<=1000))[2],2),round(prop.table(table(fpolmax$dist_cent_max<=2000))[2],2),round(prop.table(table(fpolmax$dist_cent_max<=5000))[2],2),
                                   round(prop.table(table(fpolmax$dist_cent_max<=10000))[2],2),round(prop.table(table(fpolmax$dist_cent_max<=15000))[2],2),round(prop.table(table(fpolmax$dist_cent_max<=20000))[2],2)))
df$dist_fac<-factor(df$dist, levels = c(1:6), labels = c("<=1","<=2","<=5","<=10","<=15","<=20"))

ggplot(df, aes(x=dist_fac,y=share)) + geom_bar(position = position_dodge(),stat = "identity") + labs(x="\nMaximum distance travelled at the destination (in km)",y="Share of all trips at the destination\n") + 
  scale_y_continuous(labels = scales::percent)+theme(axis.title = element_text(size=18), axis.text = element_text(size = 16), legend.text = element_text(size = 16), legend.title = element_text(size = 18)) 

# calculating pearsons correlation coefficient between the distances traveled to each cluster and the radius of movement within the cluster ----
cor(fpolmax$dist_cent_max,fpolmax$di_home_clusmean_new)

# analyses of the share of the destination countries of trips within europe per year in the official flight statistics and the flickr data set ----
df<-aggregate(counts~country+year,fligths[fligths$region_mid=="Europe",],sum)
df<-merge(df,aggregate(counts~year,df,sum),by="year",all.x=T)
df$counts_reg_year<-df$counts.x
df$counts_year<-df$counts.y
df$counts.y<-df$counts.x<-NULL
df$counts_share_year<-round(df$counts_reg_year/df$counts_year,2)
df[df$year==2017,]
df[df$year==2018,]
df[df$year==2019,]

with(df, subset(df[order(counts_share_year, decreasing = T),], subset = year == 2017)) 
with(df, subset(df[order(counts_share_year, decreasing = T),], subset = year == 2018)) 
with(df, subset(df[order(counts_share_year, decreasing = T),], subset = year == 2019)) 
with(df, subset(df[order(counts_share_year, decreasing = T),], subset = year == 2020)) 
with(df, subset(df[order(counts_share_year, decreasing = T),], subset = year == 2021)) 

sort(round(prop.table(table(fclusun$dest_country[fclusun$region_mid=="Europe"&fclusun$year==2017])),2), decreasing = T) 
sort(round(prop.table(table(fclusun$dest_country[fclusun$region_mid=="Europe"&fclusun$year==2018])),2), decreasing = T)    
sort(round(prop.table(table(fclusun$dest_country[fclusun$region_mid=="Europe"&fclusun$year==2019])),2), decreasing = T) 
sort(round(prop.table(table(fclusun$dest_country[fclusun$region_mid=="Europe"&fclusun$year==2020])),2), decreasing = T) 
sort(round(prop.table(table(fclusun$dest_country[fclusun$region_mid=="Europe"&fclusun$year==2021])),2), decreasing = T) 

# analyses of the share of the destination countries of trips outside of europe per year in the official flight statistics and the flickr data set ----
df<-aggregate(counts~country+year,fligths[fligths$region_mid=="Outside of Europe",],sum)
df<-merge(df,aggregate(counts~year,df,sum),by="year",all.x=T)
df$counts_reg_year<-df$counts.x
df$counts_year<-df$counts.y
df$counts.y<-df$counts.x<-NULL
df$counts_share_year<-round(df$counts_reg_year/df$counts_year,2)
df[df$year==2017,]
df[df$year==2018,]
df[df$year==2019,]

with(df, subset(df[order(counts_share_year, decreasing = T),], subset = year == 2017)) 
with(df, subset(df[order(counts_share_year, decreasing = T),], subset = year == 2018)) 
with(df, subset(df[order(counts_share_year, decreasing = T),], subset = year == 2019)) 
with(df, subset(df[order(counts_share_year, decreasing = T),], subset = year == 2020)) 
with(df, subset(df[order(counts_share_year, decreasing = T),], subset = year == 2021)) 

sort(round(prop.table(table(fclusun$dest_country[fclusun$region_mid=="Outside of Europe"&fclusun$year==2017])),2), decreasing = T) 
sort(round(prop.table(table(fclusun$dest_country[fclusun$region_mid=="Outside of Europe"&fclusun$year==2018])),2), decreasing = T)
sort(round(prop.table(table(fclusun$dest_country[fclusun$region_mid=="Outside of Europe"&fclusun$year==2019])),2), decreasing = T)
sort(round(prop.table(table(fclusun$dest_country[fclusun$region_mid=="Outside of Europe"&fclusun$year==2020])),2), decreasing = T)
sort(round(prop.table(table(fclusun$dest_country[fclusun$region_mid=="Outside of Europe"&fclusun$year==2021])),2), decreasing = T)