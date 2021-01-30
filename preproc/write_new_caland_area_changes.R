
# read Annual_USGS_Cultivated_Areas_Per_RO.csv & Annual_USGS_Developed_Areas_Per_RO.csv
setwd("/Users/msimmonds/Desktop/LBL/caland/data for updates to land use & cover")

last.year <- 2051
years <- 2010:last.year
# read in initial 2010 caland areas. note: missing fresh march (landtype = 120)
caland_area_2010 <- read.csv("area_code_sp9_own9_2010lt15_sqm_stats.csv", stringsAsFactors=FALSE, header=FALSE)
names(caland_area_2010) <- c("Landcat","Area.2010")
# logic index Landcat == "*" to delete 
caland_area_2010 <- caland_area_2010[caland_area_2010[,1]!="*",]
# assign numeric class to landcat column
caland_area_2010[,1] = as.numeric(caland_area_2010[,1])
# create ro column for referencing in loop
caland_area_2010$ro <- paste0((caland_area_2010[,1] %/% 100) %/% 1000, caland_area_2010[,1] %% 100)
# save separate region ownership columns
caland_area_2010 <- cbind(read.fwf(file = textConnection(as.character(caland_area_2010[,3])), 
                    widths = c(1, 1), colClasses = "character", 
                    col.names = c("Region", "Ownership")), caland_area_2010)
# create landtype column
caland_area_2010$Landtype <- (caland_area_2010$Landcat %/% 100)%% 1000
# logic index to delete water and ice landtypes. These landtpyes will be assumed to remain constant
# and not be affected by urban and cultivated area changes.
caland_area_2010 <- caland_area_2010[caland_area_2010[,6]!=0 & caland_area_2010[,6]!=10,]
# create df of total areas for each ro which must stay constant
ro.area.totals <- aggregate(Area.2010~ro,caland_area_2010,sum)
# assign numeric classes 
make.numeric<-c("Region","Ownership","ro")
caland_area_2010[,make.numeric]<-lapply(make.numeric, function(x) as.numeric(caland_area_2010[,x]))
# reorder columns
caland_area_2010 <- caland_area_2010[,c("Landcat","Region","Ownership","Landtype","ro", "Area.2010")]
# total area = 409554351900 
sum(caland_area_2010$Area.2010) == sum(ro.area.totals$Area.2010)
# total cult and dev area 69914038500 
sum(caland_area_2010$Area.2010[caland_area_2010$Landtype == 130 | caland_area_2010$Landtype == 160])

caland_area_calcs <- caland_area_2010
# flag all 2010 cult/dev areas with 1, all others are NA
caland_area_calcs$cult.dev[caland_area_calcs$Landtype == 130 | caland_area_calcs$Landtype == 160] <- 1
# calc intial aggregate sum of cult/dev areas per ro (recalc each time in loop and cbind them to ro.area.totals)
area.ro.cult.dev <- aggregate(Area.2010~ro+cult.dev,caland_area_calcs,sum)
# check that aggregation total is correct by comparing sum to above value of 69914038500 == 69914038500
sum(area.ro.cult.dev$Area.2010)

# combine 2010 cult/dev aggregates sum column with total ro aggregate sum df
ro.area.totals <- cbind(ro.area.totals,area.ro.cult.dev[,3])
# rename 2010 cult/dev aggregates sum column
colnames(ro.area.totals)[3] <- "Area.cult.dev.2010"
# match ro totals with the usgs.area.changes df by landcat
ro.totals <- ro.area.totals[match(caland_area_calcs$ro, ro.area.totals$ro), "Area.2010"]
# add ro.totals to caland.area.calcs
caland_area_calcs$ro.total.area <- ro.totals
# reorder columns
caland_area_calcs <- caland_area_calcs[,c("Landcat","Region","Ownership","Landtype","ro", "cult.dev","ro.total.area","Area.2010")]
################### (1) compile cult and dev counts for each year to get df of areas from 2010-2051 ###################

# save all landcats for cult and dev only (152 total)
usgs.areas <- caland_area_2010[caland_area_2010[,"Landtype"]==130|caland_area_2010[,"Landtype"]==160,]
usgs.areas <- usgs.areas[,-6]
#usgs.areas <- caland_area_2010[,-6]
setwd("/Users/msimmonds/Desktop/LBL/caland/data for updates to land use & cover/USGS_lt_pixels_per_ro")
for (y in years) {
  # read raster processing output (count (pixels), ro, type (1 or 2))
  f <- read.csv(paste0(y, "_USGS_lt_pixels_per_ro.csv"))
  # convert the ro and type to  land category format: reg*100000 + evt*100 + ownership, where (cult evt = 130, dev evt= 160) 
  # first split ro
  a <- cbind(read.fwf(file = textConnection(as.character(f[,"ro"])), 
                      widths = c(1, 1), colClasses = "character", 
                      col.names = c("Region", "Ownership")), f)
  a$Region <- as.numeric(a$Region)
  a$Ownership <- as.numeric(a$Ownership)
  # create landtype column using CALAND lt ID. If 1 then assign 160 (developed), otherwise it's 2 ==> assign 130 (cultivated)
  a$Landtype <- ifelse(a$type == 1, 160, 130)
  # create CALAND landcat column 
  a$Landcat <- a[,"Region"] * 100000 + a[,"Landtype"]*100 + a[,"Ownership"]
  # create area column (each pixel = 30mx30m) in units of sqm
  a$Area_sqm <- a$count * 900
  # reorder columns
  a <- a[,c("Landcat","Region", "Ownership", "Landtype", "ro", "Area_sqm")]
  # match records in a with all landcats for cult and dev
  areas <- a[match(usgs.areas$Landcat,a$Landcat),"Area_sqm"]
  usgs.areas <- cbind(usgs.areas, areas)
  # replace NA with 0's
  usgs.areas$areas[is.na(usgs.areas$areas)] <- 0
  # name column with year in loop
  colnames(usgs.areas)[ncol(usgs.areas)] <- paste0("Area_", y,"_m2")
}

################### (2) create new df of the annual dev/cult area changes for 2010-2050 ###################

# combine header columns with differences between consecutive area columns to get annual area changes
usgs.area.changes <- cbind(usgs.areas[,1:5],usgs.areas[,7:ncol(usgs.areas)] - usgs.areas[,6:(ncol(usgs.areas) - 1)])
# rename the columns to match the years
colnames(usgs.area.changes)[6:ncol(usgs.area.changes)] <- colnames(usgs.areas)[6:(ncol(usgs.areas)-1)]

################### (3) apply the dev/cult area changes to the other landcats ###################

# match the area change columns in usgs.area.changes (152 obs.) with caland_area_2010 by landcat ID
# match records in usgs.area.changes with all landcats for cult and dev
area.changes <- usgs.area.changes[match(caland_area_2010$Landcat,usgs.area.changes$Landcat),6:ncol(usgs.area.changes)]
# merge area changes with all the land categories ID's and other identifiers, omitting 2010 initial areas
caland_area_changes <- cbind(caland_area_2010[-6], area.changes)
# replace NA's with 0
caland_area_changes[is.na(caland_area_changes)] <- 0

# loop through each annual calculation of caland area changes (column 6 to last in caland_area_changes)
for (i in 6:ncol(caland_area_changes)) {
  caland_area_calcs_temp <- caland_area_calcs[,1:8]
  # calc new cult/dev area = current caland cult/dev area + current area.change 
  caland_area_calcs[caland_area_calcs$Landtype==130 | caland_area_calcs$Landtype==160,i+3] <- 
    caland_area_calcs[caland_area_calcs$Landtype==130 | caland_area_calcs$Landtype==160,i+2] + 
    caland_area_changes[caland_area_changes$Landtype==130 | caland_area_changes$Landtype==160,i]
  # name column with year in loop
  colnames(caland_area_calcs)[ncol(caland_area_calcs)] <- paste0("Area.", 2005+i)
 
  # track net changes of cult/dev area per ro with existing cult or dev: 
    # save new area column ncol(caland_area_calcs) to caland_area_calcs_temp so it can be aggregated for each loop
  caland_area_calcs_temp$new.area[caland_area_calcs_temp$Landtype==130 | caland_area_calcs_temp$Landtype==160] <- 
    caland_area_calcs[caland_area_calcs$Landtype==130 | caland_area_calcs$Landtype==160,ncol(caland_area_calcs)]
  # aggregate sum new.area for all cult & dev by ro (new total cult/dev area = 69869052900)
  # in 2011 there are 77 ro's with cult and/or dev
  area.ro.cult.dev <- aggregate(new.area~ro, 
                                caland_area_calcs_temp[caland_area_calcs_temp$Landtype==130 | 
                                                         caland_area_calcs_temp$Landtype==160,], sum)
  # cbind new cult/dev aggregated areas with total ro.area.totals
  ro.area.totals <- cbind(ro.area.totals,area.ro.cult.dev[,2])
  # rename column
  colnames(ro.area.totals)[ncol(ro.area.totals)] <- paste0("Area.cult.dev.", 2005+i)
  # check if each cult/dev ro is shrinking or expanding. If shrinking (neg), then assign + for other ro-landtypes 
  #sign.change <- ifelse((ro.area.totals[,ncol(ro.area.totals)]-ro.area.totals[,ncol(ro.area.totals)-1])>0,-1,1)
  
  # get remaining ro area to distribute (via take away from or add to) the other land types based on change.dir
    # total area.reamining per ro = total ro 2010 initial area - total cult/dev per ro
  next.area.remaining <- ro.area.totals$Area.2010 - ro.area.totals[,ncol(ro.area.totals)]
  
  
  # temp cbind new cols with ro.area.totals
  # ro.area.totals.temp <- cbind(ro.area.totals, sign.change, area.remaining)
  ro.area.totals.temp <- cbind(ro.area.totals, next.area.remaining)
  # match sign.change and area.remaining with caland_area_calcs by landcat 
  #sign <- ro.area.totals.temp[match(caland_area_calcs$ro, ro.area.totals.temp$ro), "sign.change"]
  caland_area_calcs_temp$next.area.remaining <- ro.area.totals.temp[match(caland_area_calcs$ro, ro.area.totals.temp$ro), 
                                                                    "next.area.remaining"]
  
  # match current total cult/dev area per ro with the temp caland calc df
  caland_area_calcs_temp$current.area.remaining <- ro.area.totals[match(caland_area_calcs$ro, ro.area.totals$ro), 
                                                                  paste0("Area.cult.dev.", 2004+i)]

  # calc new area based on ratio of other landtype areas to total ro LESS the current cult/dev area
  caland_area_calcs[caland_area_calcs$Landtype!=130 & caland_area_calcs$Landtype!=160,i+3] <- 
    caland_area_calcs_temp$next.area.remaining[caland_area_calcs_temp$Landtype!=130 & caland_area_calcs_temp$Landtype!=160] *
    (caland_area_calcs[caland_area_calcs$Landtype!=130 & caland_area_calcs$Landtype!=160,i+2] / 
    (caland_area_calcs$ro.total.area[caland_area_changes$Landtype!=130 & caland_area_changes$Landtype!=160] -
       caland_area_calcs_temp$current.area.remaining[caland_area_calcs_temp$Landtype!=130 & caland_area_calcs_temp$Landtype!=160]))
    
  # calc area change of other landtypes 
  caland_area_changes[caland_area_changes$Landtype!=130 & caland_area_changes$Landtype!=160,i] <- 
    caland_area_calcs[caland_area_calcs$Landtype!=130 & caland_area_calcs$Landtype!=160,i+3] -
    caland_area_calcs[caland_area_calcs$Landtype!=130 & caland_area_calcs$Landtype!=160,i+2]
  
  # check sum of area changes equal 0 ######## FALSE ############  -2.277338e-07 for 2010
  sum(caland_area_changes$Area_2010_m2)==0
  # check that new areas are all positive  #### TRUE #####
  all(caland_area_calcs[,i] > 0)
  # check that sum of all new areas equals initial total area ===> TRUE
  sum(caland_area_calcs[,i])==sum(caland_area_calcs[,i-2])
  sum(caland_area_calcs[,i])-sum(caland_area_calcs[,i-2])
}
# check if area.remaining + area.cult.dev.2051 = sum(total initial area) ## TRUE ##
sum(ro.area.totals.temp$Area.cult.dev.2051 + ro.area.totals.temp$next.area.remaining) == sum(caland_area_2010$Area.2010)
# check that sum of all area changes should equal 0 (FALSE due to rounding error) TRUE for between 0.00001 and -0.00001
all(unlist((unlist(lapply(caland_area_changes[,6:ncol(caland_area_changes)], function(x) sum(x)<0.00001 & sum(x)>-0.00001)))))
# check that the sum of areas for each year equals the initial areas 
all(unlist((lapply(caland_area_calcs[,8:ncol(caland_area_calcs)], function(x) sum(x)==sum(caland_area_2010$Area.2010)))))

# save area change outputs
write.csv(caland_area_changes[-(2:5)], paste0("CALAND.Area.Changes.","2010-", last.year, ".csv"))

