library(ncdf4);
library(raster);
library(chron);
rotate <- function(x) t(apply(x, 2, rev));

path0 <- "d://cru.ts//nc";
setwd(path0);
flst <- dir();

path1 <- "d://cru.ts//";
lo_to_ex <- read.csv(paste(path1, "meta_info.csv", sep = ''),colClasses = c("character","numeric","numeric","numeric"));

i3 <- which((!is.na(lo_to_ex$lat))& (lo_to_ex$year<=2014));
lo_to_ex2 <- lo_to_ex[i3,];

outp <- as.data.frame(matrix(0, ncol = 2 * length(flst) + 1, nrow = length(lo_to_ex2$id)));
names(outp) <- c("id", paste(rep(substr(flst, 22, 24),each=2), rep(c('_mean', "_sum"),length(flst)), sep = ''));
row.names(outp) <- lo_to_ex2$id;
outp$id <- lo_to_ex2$id;

for (ijk in 1:length(flst)) {

    ncdata <- nc_open(flst[ijk]);
    data <- ncvar_get(ncdata);
    nc_close(ncdata);

    years <- as.numeric(as.character(years(as.Date(ncdata$dim[[3]]$vals, origin = '1900-01-01'))))
    
    for (ij in 1:length(lo_to_ex2$id)) {

        year_search <- lo_to_ex2$year[ij]
        ii <- which(years %in% year_search)
        if(length(ii)<1) break;
        bba <- raster(rotate(rotate(rotate(data[,, ii[1]]))))
        extent(bba) <- c(-180, 180, -90, 90)
        for (i in 2:length(ii)) {
            ccc <- raster(rotate(rotate(rotate(data[,,ii[i]]))));
            extent(ccc) <- c(-180, 180, -90, 90)
            bba <- stack(bba, ccc);
        }

        data_cal <- as.numeric(extract(bba, data.frame(x = lo_to_ex2$lon[ij], y = lo_to_ex2$lat[ij])), method = 'bilinear')
        if(is.na(mean(data_cal, na.rm = T))) break;
        outp[ij, c(2 * ijk, 2 * ijk + 1)] = c(mean(data_cal, na.rm = T), sum(data_cal, na.rm = T))
    }

}

write.csv(outp,'outp.csv')