library(ncdf4);
library(raster);
library(chron);
library(doBy);
rotate <- function(x) t(apply(x, 2, rev));

path0 <- "d://cru.ts//nc";
setwd(path0);
flst <- dir();

path1 <- "d://cru.ts//";
lo_to_ex <- read.csv(paste(path1, "id_station.csv", sep = ''), colClasses = c("character", "character", "numeric", "numeric"));
    l.id <- length(lo_to_ex$id);
    years <- 1951:2014
    l.yr <- length(years)
    l.m <- 12

outp <- as.data.frame(matrix(0, ncol = length(flst) + 3, nrow = l.id*l.yr*l.m));
names(outp) <- c("id", "year", "month", paste(substr(flst, 22, 24), rep('_mean', length(flst)), sep = ''));

for (ijk in 1:length(flst)) {

    ncdata <- nc_open(flst[ijk]);
    data <- ncvar_get(ncdata);
    nc_close(ncdata);

    years <- as.numeric(as.character(years(as.Date(ncdata$dim[[3]]$vals, origin = '1900-01-01'))))
    for (ik in years) {
        ii <- which(years %in% ik)
        bba <- raster(rotate(rotate(rotate(data[,, ii[1]]))))
        extent(bba) <- c(-180, 180, -90, 90)
        for (i in 2:length(ii)) {
            ccc <- raster(rotate(rotate(rotate(data[,, ii[i]]))));
            extent(ccc) <- c(-180, 180, -90, 90)
            bba <- stack(bba, ccc);
        }
        for (ij in 1:length(lo_to_ex$id)) {
            data_cal <- as.numeric(extract(bba, data.frame(x = lo_to_ex$lon[ij], y = lo_to_ex$lat[ij])), method = 'bilinear')
            ij_b <- (ik - 1) * l.id * 12 + (ij - 1) * 12

            outp[(ij_b+1):(ij_b+12), (ijk+3)] = data_cal
            outp[(ij_b + 1):(ij_b + 12), 1] <- lo_to_ex$id[ij];
            outp[(ij_b + 1):(ij_b + 12), 2] <- years[ik];
            outp[(ij_b + 1):(ij_b + 12), 3] <- 1:12
        }
    }

}

write.csv(outp, 'outp_jiang.csv')

