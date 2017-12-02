#!/bin/bash

# change working directory
cd /automount/agh/Projects/skiefer/02-raw-by-year/

# change wd to folder with yearly datasets		####
cd ../02-raw-by-year/

# merge yearly datasets to at-a-stretch-dataset
cdo -b F32 mergetime *.nc ../03-raw-full/a-1957-2016-e4ei-1deg.nc

# change wd to complete dataset				####
cd ../03-raw-full/

# get information about dataset
cdo sinfo a-1957-2016-e4ei-1deg.nc
cdo griddes a-1957-2016-e4ei-1deg.nc
cdo pardes a-1957-2016-e4ei-1deg.nc
cdo showtimestamp a-1957-2016-e4ei-1deg.nc

# interpolate to t63-grid
cdo remapbil,t63grid a-1957-2016-e4ei-1deg.nc b-1957-2016-e4ei-t63-monmean.nc

# select parameters u,v, select grid northern hemisphere, and invert latitude  
cdo -invertlat -sellonlatbox,0,360,90,0 -selparam,-1,-5,-6 b-1957-2016-e4ei-t63-monmean.nc c-1957-2016-e4ei-t63-monmean-zuv-nh.nc

# change wd to root-directory				####
cd ../
cp 03-raw-full/c-1957-2016-e4ei-t63-monmean-zuv-nh.nc ./

