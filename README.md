# flickr_longdistance_mobility
This repository makes use of the public Flickr API in order to analyse long-distance trips on the basis of the georeferenced locations of the photos uploaded to Flickr. More details about the Flickr API such as the data available and the terms of use can be found here: https://www.flickr.com/services/api/

More specifically, this repo allows to analyse the long-distance mobility of the Flickr users residing in a selected country. For this purpose, various scripts are provided that allow identifying the user's city of residence and destinations, spatially and temporally clustering the different locations at the destinations into separate holiday trips, as well as calculating the distances travelled between the places of residence and the regions of destination and the radius of movenemnt at the destination. 

The prepared data set with the information retrieved from th Flickr API is then compared to other data sources on the basis of key indicators such as the distances traveled and the share of the destination countries. The other data sources are the Mobility in Germany 2017 survey, official statistics on air travel of the Statistical Office of Germany, and further transportation statistics provided by the Federal Ministry for Digital and Transport of Germany. 

More information about the Mobility in Germany 2017 survey can be found on the following website: https://www.mobilitaet-in-deutschland.de/archive/index.html
The actual data collected in the Mobility in Germany 2017 survey can be requested here: https://daten.clearingstelle-verkehr.de/279/
The official statistics on air traffic used in this repository and provided in a separate folder can also be retrieved here: https://www-genesis.destatis.de/genesis//online?operation=table&code=46421-0007&bypass=true&levelindex=0&levelid=1692946459814#abreadcrumb 
More information about the Transport in Figures statistics as well as the actual annual reports can be accessed on the webpage of the Federal Ministry for Digital and Transport of Germany: https://bmdv.bund.de/SharedDocs/DE/Artikel/G/verkehr-in-zahlen.html

A paper based on this work will be presented at the European Transport Conference in September 2023 in Milan (Italy). 
