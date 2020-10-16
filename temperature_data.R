library(raster)
library(ncdf4)
library(rasterVis)

setwd("/Users/Inga/Documents/GitHub/DoubleBloodFeeds")

max_2010 <- brick("temp_data/TerraClimate_tmax_2010.nc")
min_2010 <- brick("temp_data/TerraClimate_tmin_2010.nc")

max_2011 <- brick("temp_data/TerraClimate_tmax_2011.nc")
min_2011 <- brick("temp_data/TerraClimate_tmin_2011.nc")

max_2012 <- brick("temp_data/TerraClimate_tmax_2012.nc")
min_2012 <- brick("temp_data/TerraClimate_tmin_2012.nc")

max_2013 <- brick("temp_data/TerraClimate_tmax_2013.nc")
min_2013 <- brick("temp_data/TerraClimate_tmin_2013.nc")

max_2014 <- brick("temp_data/TerraClimate_tmax_2014.nc")
min_2014 <- brick("temp_data/TerraClimate_tmin_2014.nc")

max_2015 <- brick("temp_data/TerraClimate_tmax_2015.nc")
min_2015 <- brick("temp_data/TerraClimate_tmin_2015.nc")

max_2016 <- brick("temp_data/TerraClimate_tmax_2016.nc")
min_2016 <- brick("temp_data/TerraClimate_tmin_2016.nc")

max_2017 <- brick("temp_data/TerraClimate_tmax_2017.nc")
min_2017 <- brick("temp_data/TerraClimate_tmin_2017.nc")

max_2018 <- brick("temp_data/TerraClimate_tmax_2018.nc")
min_2018 <- brick("temp_data/TerraClimate_tmin_2018.nc")


jan <- stack(mean(subset(max_2010, 1), subset(min_2010, 1)),
             mean(subset(max_2011, 1), subset(min_2011, 1)),
             mean(subset(max_2012, 1), subset(min_2012, 1)),
             mean(subset(max_2013, 1), subset(min_2013, 1)),
             mean(subset(max_2014, 1), subset(min_2014, 1)),
             mean(subset(max_2015, 1), subset(min_2015, 1)),
             mean(subset(max_2016, 1), subset(min_2016, 1)),
             mean(subset(max_2017, 1), subset(min_2017, 1)),
             mean(subset(max_2018, 1), subset(min_2018, 1)))

mean_jan <- raster::calc(jan, fun = mean)


feb <- stack(mean(subset(max_2010, 2), subset(min_2010, 2)),
             mean(subset(max_2011, 2), subset(min_2011, 2)),
             mean(subset(max_2012, 2), subset(min_2012, 2)),
             mean(subset(max_2013, 2), subset(min_2013, 2)),
             mean(subset(max_2014, 2), subset(min_2014, 2)),
             mean(subset(max_2015, 2), subset(min_2015, 2)),
             mean(subset(max_2016, 2), subset(min_2016, 2)),
             mean(subset(max_2017, 2), subset(min_2017, 2)),
             mean(subset(max_2018, 2), subset(min_2018, 2)))

mean_feb <- raster::calc(feb, fun = mean)


march <- stack(mean(subset(max_2010, 3), subset(min_2010, 3)),
             mean(subset(max_2011, 3), subset(min_2011, 3)),
             mean(subset(max_2012, 3), subset(min_2012, 3)),
             mean(subset(max_2013, 3), subset(min_2013, 3)),
             mean(subset(max_2014, 3), subset(min_2014, 3)),
             mean(subset(max_2015, 3), subset(min_2015, 3)),
             mean(subset(max_2016, 3), subset(min_2016, 3)),
             mean(subset(max_2017, 3), subset(min_2017, 3)),
             mean(subset(max_2018, 3), subset(min_2018, 3)))

mean_march <- raster::calc(march, fun = mean)


april <- stack(mean(subset(max_2010, 4), subset(min_2010, 4)),
             mean(subset(max_2011, 4), subset(min_2011, 4)),
             mean(subset(max_2012, 4), subset(min_2012, 4)),
             mean(subset(max_2013, 4), subset(min_2013, 4)),
             mean(subset(max_2014, 4), subset(min_2014, 4)),
             mean(subset(max_2015, 4), subset(min_2015, 4)),
             mean(subset(max_2016, 4), subset(min_2016, 4)),
             mean(subset(max_2017, 4), subset(min_2017, 4)),
             mean(subset(max_2018, 4), subset(min_2018, 4)))

mean_april <- raster::calc(april, fun = mean)


may <- stack(mean(subset(max_2010, 5), subset(min_2010, 5)),
             mean(subset(max_2011, 5), subset(min_2011, 5)),
             mean(subset(max_2012, 5), subset(min_2012, 5)),
             mean(subset(max_2013, 5), subset(min_2013, 5)),
             mean(subset(max_2014, 5), subset(min_2014, 5)),
             mean(subset(max_2015, 5), subset(min_2015, 5)),
             mean(subset(max_2016, 5), subset(min_2016, 5)),
             mean(subset(max_2017, 5), subset(min_2017, 5)),
             mean(subset(max_2018, 5), subset(min_2018, 5)))

mean_may <- raster::calc(may, fun = mean)


june <- stack(mean(subset(max_2010, 6), subset(min_2010, 6)),
             mean(subset(max_2011, 6), subset(min_2011, 6)),
             mean(subset(max_2012, 6), subset(min_2012, 6)),
             mean(subset(max_2013, 6), subset(min_2013, 6)),
             mean(subset(max_2014, 6), subset(min_2014, 6)),
             mean(subset(max_2015, 6), subset(min_2015, 6)),
             mean(subset(max_2016, 6), subset(min_2016, 6)),
             mean(subset(max_2017, 6), subset(min_2017, 6)),
             mean(subset(max_2018, 6), subset(min_2018, 6)))

mean_june <- raster::calc(june, fun = mean)


july <- stack(mean(subset(max_2010, 7), subset(min_2010, 7)),
             mean(subset(max_2011, 7), subset(min_2011, 7)),
             mean(subset(max_2012, 7), subset(min_2012, 7)),
             mean(subset(max_2013, 7), subset(min_2013, 7)),
             mean(subset(max_2014, 7), subset(min_2014, 7)),
             mean(subset(max_2015, 7), subset(min_2015, 7)),
             mean(subset(max_2016, 7), subset(min_2016, 7)),
             mean(subset(max_2017, 7), subset(min_2017, 7)),
             mean(subset(max_2018, 7), subset(min_2018, 7)))

mean_july <- raster::calc(july, fun = mean)


aug <- stack(mean(subset(max_2010, 8), subset(min_2010, 8)),
             mean(subset(max_2011, 8), subset(min_2011, 8)),
             mean(subset(max_2012, 8), subset(min_2012, 8)),
             mean(subset(max_2013, 8), subset(min_2013, 8)),
             mean(subset(max_2014, 8), subset(min_2014, 8)),
             mean(subset(max_2015, 8), subset(min_2015, 8)),
             mean(subset(max_2016, 8), subset(min_2016, 8)),
             mean(subset(max_2017, 8), subset(min_2017, 8)),
             mean(subset(max_2018, 8), subset(min_2018, 8)))

mean_aug <- raster::calc(aug, fun = mean)


sept <- stack(mean(subset(max_2010, 9), subset(min_2010, 9)),
             mean(subset(max_2011, 9), subset(min_2011, 9)),
             mean(subset(max_2012, 9), subset(min_2012, 9)),
             mean(subset(max_2013, 9), subset(min_2013, 9)),
             mean(subset(max_2014, 9), subset(min_2014, 9)),
             mean(subset(max_2015, 9), subset(min_2015, 9)),
             mean(subset(max_2016, 9), subset(min_2016, 9)),
             mean(subset(max_2017, 9), subset(min_2017, 9)),
             mean(subset(max_2018, 9), subset(min_2018, 9)))

mean_sept <- raster::calc(sept, fun = mean)


oct <- stack(mean(subset(max_2010, 10), subset(min_2010, 10)),
             mean(subset(max_2011, 10), subset(min_2011, 10)),
             mean(subset(max_2012, 10), subset(min_2012, 10)),
             mean(subset(max_2013, 10), subset(min_2013, 10)),
             mean(subset(max_2014, 10), subset(min_2014, 10)),
             mean(subset(max_2015, 10), subset(min_2015, 10)),
             mean(subset(max_2016, 10), subset(min_2016, 10)),
             mean(subset(max_2017, 10), subset(min_2017, 10)),
             mean(subset(max_2018, 10), subset(min_2018, 10)))

mean_oct <- raster::calc(oct, fun = mean)


nov <- stack(mean(subset(max_2010, 11), subset(min_2010, 11)),
             mean(subset(max_2011, 11), subset(min_2011, 11)),
             mean(subset(max_2012, 11), subset(min_2012, 11)),
             mean(subset(max_2013, 11), subset(min_2013, 11)),
             mean(subset(max_2014, 11), subset(min_2014, 11)),
             mean(subset(max_2015, 11), subset(min_2015, 11)),
             mean(subset(max_2016, 11), subset(min_2016, 11)),
             mean(subset(max_2017, 11), subset(min_2017, 11)),
             mean(subset(max_2018, 11), subset(min_2018, 11)))

mean_nov <- raster::calc(nov, fun = mean)


dec <- stack(mean(subset(max_2010, 12), subset(min_2010, 12)),
             mean(subset(max_2011, 12), subset(min_2011, 12)),
             mean(subset(max_2012, 12), subset(min_2012, 12)),
             mean(subset(max_2013, 12), subset(min_2013, 12)),
             mean(subset(max_2014, 12), subset(min_2014, 12)),
             mean(subset(max_2015, 12), subset(min_2015, 12)),
             mean(subset(max_2016, 12), subset(min_2016, 12)),
             mean(subset(max_2017, 12), subset(min_2017, 12)),
             mean(subset(max_2018, 12), subset(min_2018, 12)))

mean_dec <- raster::calc(dec, fun = mean)

