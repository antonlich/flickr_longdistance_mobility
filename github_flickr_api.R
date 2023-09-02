# installation and loading of the packages available via cran
packagename<-c("remotes","geosphere","RPostgreSQL")
if (length(setdiff(packagename, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packagename, rownames(installed.packages())),repos = "https://cran.uni-muenster.de/", Ncpus = 16)  
}

lapply(packagename, require, character.only = T)
rm(packagename)

# installation and loading of the remote package photosearcher from github
remotes::install_github("nfox29/photosearcher")
library("photosearcher")

# specification of the time period and the country of interest
mindt<-"year-month-day" # definition of the minimum date on which a photo was taken in the provided format
maxdt<-"year-month-day" # definition of the maximum date on which a photo was taken in the provided format
maxup<-"year-month-day" # definition of the maximum date on which a photo was uploaded in the provided format
countryname_english<-"name of the country of interest in english" # country name in english has to be provided
countryname_native<-"name of the country of interest in the country's native language" # country name in he country's native language has to be provided

# download of the information accessible for all photos in the specified time period
area_photos<-photo_search(mindate_taken  = mindt, maxdate_taken = maxdt, maxdate_uploaded= maxup) 

# extraction of the available information on the users that uploaded the photos 
social_data<-user_info(user_id=area_photos$owner)

# selection of only those users that reside in the country of interest
social_data_countryname<-social_data[(social_data$country%in%countryname_english|social_data$country%in%countryname_english),]
rm(social_data)

# dropping the duplicates from the data set 
social_data_countryname$owner<-social_data_countryname$id
social_data_countryname<-social_data_countryname[duplicated(social_data_countryname$owner)==F,]

# merging the data sets with information on the users and the photos
arph<-merge(social_data_countryname[,c("owner","hometown","city","occupation","country")],area_photos[,c("owner","latitude","longitude","datetaken","dateupload","title","tags","description","woeid")],by="owner",all.x = T)
rm(area_photos,social_data_countryname)

# calculating the number of photos uploaded per user
df<-aggregate(latitude~owner,arph,NROW)
df$nop<-df$latitude
arph<-merge(arph,df[,c("owner","nop")],by="owner",all.x = T)
rm(df)


# export of the new data set into the data base (in this case a postgres data base but this can be changed)
dbn<-"name of the data base"
hst<-"name or address of the hosting server"
pt<-"port number"
u<-"name of the user with access to the data base"
pw<-"password of the user"
con<-dbConnect(PostgreSQL(),dbname=dbn ,host=hst, port=pt,user=u, password=pw)
dbWriteTable(con,"arph",arph, row.names=FALSE, overwrite=T)


# defining the variables for the latitude and longitude values of the consecutive photo uploaded and the distance between the coordinates of two consecutive photos
arph$latnext<-NA
arph$lonnext<-NA
arph$di<-NA

# creating an empty list 
lor<-list()

# creating a loop which fills each element of the list with the information available on all photos uploaded by one user and calculates the distance as the crow flies between the coordinates of two consecutive photos
for (i in 1:length(unique(arph$owner))){
  print(i)
  lor[[i]]<-arph[arph$owner==unique(arph$owner)[i],]
  if(nrow(lor[[i]])>1){
    lor[[i]]<-lor[[i]][order(lor[[i]]["datetaken"]),]
    for (h in 1:(nrow(lor[[i]])-1)){
      lor[[i]][h,"latnext"]<-lor[[i]][h+1,"latitude"]
      lor[[i]][h,"lonnext"]<-lor[[i]][h+1,"longitude"]
      lor[[i]][h,"di"]<-distm(c(lor[[i]][h,"longitude"],lor[[i]][h,"latitude"]),c(lor[[i]][h,"lonnext"],lor[[i]][h,"latnext"]), fun = distHaversine)
      lor[[i]][h,"datenext"]<-lor[[i]][h+1,"datetaken"]
      lor[[i]][h,"titlenext"]<-lor[[i]][h+1,"title"]
      lor[[i]][h,"tagsnext"]<-lor[[i]][h+1,"tags"]
      lor[[i]][h,"descriptionnext"]<-lor[[i]][h+1,"description"]
    }
    lor[[i]]<-lor[[i]][lor[[i]]["di"]>0&is.na(lor[[i]]["di"])==F,]
  }
}

# (row-)binding all elements of the list into a data frame
flickrall<-do.call(rbind,lor)


# export of the data frame into the data base
con<-dbConnect(PostgreSQL(),dbname=dbn ,host=hst, port=pt,user=u, password=pw)
dbWriteTable(con,"flickrall",flickrall, row.names=FALSE, overwrite=T)

# printing "doneall" (this can be useful if the script is run on a remote server in the background)
print("doneall")
rm(flickrall,lor,arph)
