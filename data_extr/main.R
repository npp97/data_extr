library(ncdf4);
library(raster);
library(chron);
library(readxl);

path0 <- "d://cru.ts//nc"
setwd(path0);
flst <- dir()

path1 <- "d://cru.ts//expdata"
fn_to_read <- read_excel(paste(path1, "exp"), sheet = 1);
lo_to_ex<-data.frame(lat=,lon=,year_search=)

ncdata <- nc_open(flst[1]);
data <- ncvar_get(ncdata);

year_search<-c(1989,2001)
years <-as.numeric(as.character(years(as.Date(ncdata$dim[[3]]$vals, origin = '1900-01-01'))))
ii <- which(years %in% year_search)
data_brick <- data[,,ii]

#firstly,rotate layers and build a brick then extract
#extent(data_brick)<-c(-90,90,180,-180)
bba <- raster(rotate(rotate(rotate(data[,, ii[1]]))))
extent(bba) <- c(-180, 180, -90, 90)
for (i in 2:length(ii)) {
    ccc <- raster(rotate(rotate(rotate(data[,, ii[i]]))));
    extent(ccc) <- c(-180, 180, -90, 90)
    bba <- stack(bba, ccc);
}

data_cal <- as.numeric(extract(bba, data.frame(x = 11, y = 12)))

