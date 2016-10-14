library(ncdf4);
library(raster);
library(chron);
library(doBy);

path0 <- "d://cru.ts//nc";
setwd(path0);
flst <- dir();

path1 <- "d://cru.ts//";
#lo_to_ex <- read.csv(paste(path1, "id_station.csv", sep = ''), colClasses = c("character", "character", "numeric", "numeric"));
lo_to_ex <- read.csv(paste(path1, "id_station_ne.csv", sep = ''), colClasses = c("character", "character", "numeric", "numeric"));

ijk = 1
ncdata <- nc_open(flst[ijk]);
data <- ncvar_get(ncdata);
nc_close(ncdata);

mdy<-month.day.year(ncdata$dim[[3]]$vals, origin = c(month = 1, day = 1, year = 1900))
years <- mdy$year
months <- mdy$month

ss <- stack(flst[ijk])
ex_ss <- extract(ss, lo_to_ex[, c('lon', 'lat')], method = 'bilinear')

ex_ss <- as.data.frame(t(ex_ss))
nss<-paste('S', lo_to_ex$id, sep = '')
names(ex_ss) <- nss
ex_ss <- data.frame(years, months, ex_ss)

reshape(ex_ss, v.names = substr(flst[ijk], 22, 24), varying = names(ex_ss)[3:ncol(ex_ss)], timevar = "id1", time = names(ex_ss)[3:ncol(ex_ss)], direction = 'long') -> aa
aa$id<-paste(aa$id1,aa$years,aa$months,sep='.')

ijk = 2
ncdata <- nc_open(flst[ijk]);
data <- ncvar_get(ncdata);
nc_close(ncdata);

mdy <- month.day.year(ncdata$dim[[3]]$vals, origin = c(month = 1, day = 1, year = 1900))
years <- mdy$year
months <- mdy$month

ss <- stack(flst[ijk])
ex_ss <- extract(ss, lo_to_ex[, c('lon', 'lat')], method = 'bilinear')

ex_ss <- as.data.frame(t(ex_ss))
nss <- paste('S', lo_to_ex$id, sep = '')
names(ex_ss) <- nss
ex_ss <- data.frame(years, months, ex_ss)

reshape(ex_ss, v.names = substr(flst[ijk], 22, 24), varying = names(ex_ss)[3:ncol(ex_ss)], timevar = "id1", time = names(ex_ss)[3:ncol(ex_ss)], direction = 'long') -> bb
bb$id <- paste(bb$id1, bb$years, bb$months, sep = '.')

merge(aa, bb, by = 'id') -> cc
cc <- cc[, c(1:5, 9)];
names(cc) <- c("id", "years", "months", "id1", "pre", "tem")

#write.csv(cc, 'tem_pre.csv', row.names = F)
write.csv(cc, 'tem_pre_ne.csv', row.names = F)

####################
path0 <- "d://cru.ts//";
setwd(path0);

read.csv('s2016obs_ne.csv') -> obs
read.csv('tem_pre_cru_ne.csv') -> cru
merge(obs, cru, by = 'id') -> all
all <- all[, c(1:6, 10:11)]
names(all) <- c("id", "month", "id1", "years", "tem_obs", "pre_obs", "pre_cru", "tem_cru")


read.csv('id_station.csv') -> st_intl
read.csv('id_station_ne.csv') -> st_ne

ii <- which(!(st_ne$id %in% st_intl$id))
id_stc<-st_ne$id[ii]

ii_nintl <- which(all$id1 %in% id_stc)
ii_intl <- which(all$id1 %in% st_intl$id)

plot(all$tem_cru[ii_nintl], all$tem_obs[ii_nintl])
plot(all$pre_cru[ii_nintl], all$pre_obs[ii_nintl])

plot(all$tem_cru[ii_intl], all$tem_obs[ii_intl])
plot(all$pre_cru[ii_intl], all$pre_obs[ii_intl])

summary(lm(tem_obs ~ tem_cru, data = all[ii_nintl,]))
summary(lm(tem_obs ~ tem_cru, data = all[ii_intl,]))

summary(lm(pre_cru ~ pre_obs, data = all[ii_nintl,]))
summary(lm(pre_cru ~ pre_obs, data = all[ii_intl,]))

library(doBy)
s_intl <- summaryBy(tem_obs + tem_cru + pre_cru + pre_obs ~ years + months, FUN = mean, na.rm = T, data = all[ii_intl,])
s_nintl <- summaryBy(tem_obs + tem_cru + pre_cru + pre_obs ~ years + months, FUN = mean, na.rm = T, data = all[ii_nintl,])

ts.plot(s_intl[,2:3])
