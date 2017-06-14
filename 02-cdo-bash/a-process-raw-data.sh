#!/bin/bash


# change working directory
cd /automount/agh/Projects/skiefer/01-raw-by-month/	####

# merge monthly datasets to yearly datasetsi
#cdo -b F32 mergetime 1957* ../02-raw-by-year/1957-e4.nc
#cdo -b F32 mergetime 1957* ../02-raw-by-year/1957-e4.nc
#cdo -b F32 mergetime 2012* ../02-raw-by-year/2012-ei.nc
#cdo -b F32 mergetime 2013* ../02-raw-by-year/2013-ei.nc
#cdo -b F32 mergetime 2014* ../02-raw-by-year/2014-ei.nc
#cdo -b F32 mergetime 2016* ../02-raw-by-year/2016-ei.nc
#cdo -b F32 mergetime 2016* ../02-raw-by-year/2016-ei.nc

# change wd to folder with yearly datasets		####
cd ../02-raw-by-year/

# merge yearly datasets to at-a-stretch-dataset
#cdo -b F32 mergetime *.nc ../03-raw-full/a-1957-2016-e4ei-1deg.nc

# change wd to complete dataset				####
cd ../03-raw-full/

# interpolate to t63-grid
#cdo remapbil,t63grid a-1957-2016-e4ei-1deg.nc b-1957-2016-e4ei-t63.nc

# calculate daily means
#cdo daymean b-1957-2016-e4ei-t63.nc c-1957-2016-e4ei-t63-daymean.nc

# interpolate to t63 and calculate daily means in one step
# cdo -daymean -remapbil,t63grid a-1957-2016-e4ei-1deg.nc b-1957-2016-e4ei-t63-daymean.nc

# select parameters u,v, select grid northern hemisphere, and invert latitude  
cdo -invertlat -sellonlatbox,0,360,90,0 -selparam,-1,-5,-6 b-1957-2016-e4ei-t63-daymean.nc c-1957-2016-e4ei-t63-daymean-zuv-nh.nc

# change wd to root-directory				####
cd ../

# calculate fields of mean and sd
# cdo -timmean 03-raw-full/c-1957-2016-e4ei-t63-daymean-uv-nh.nc a-1957-2016-e4ei-t63-uv-nh-timmean.nc
# cdo -timstd 03-raw-full/c-1957-2016-e4ei-t63-daymean-uv-nh.nc a-1957-2016-e4ei-t63-uv-nh-timsd.nc

# monthly means
cdo -monmean 03-raw-full/c-1957-2016-e4ei-t63-daymean-zuv-nh.nc b-1957-2016-e4ei-t63-zuv-nh-monmean.nc
# cdo -ymonmean 03-raw-full/c-1957-2016-e4ei-t63-daymean-uv-nh.nc b-1957-2016-e4ei-t63-uv-nh-ymonmean.nc
# cdo -monstd 03-raw-full/c-1957-2016-e4ei-t63-daymean-uv-nh.nc b-1957-2016-e4ei-t63-uv-nh-monsd.nc
# cdo -ymonstd 03-raw-full/c-1957-2016-e4ei-t63-daymean-uv-nh.nc b-1957-2016-e4ei-t63-uv-nh-ymonsd.nc

# seasonal means
# cdo -seasmean 03-raw-full/c-1957-2016-e4ei-t63-daymean-uv-nh.nc c-1957-2016-e4ei-t63-uv-nh-seasmean.nc
# cdo -yseasmean 03-raw-full/c-1957-2016-e4ei-t63-daymean-uv-nh.nc c-1957-2016-e4ei-t63-uv-nh-yseasmean.nc
# cdo -seasstd 03-raw-full/c-1957-2016-e4ei-t63-daymean-uv-nh.nc c-1957-2016-e4ei-t63-uv-nh-seassd.nc
# cdo -yseasstd 03-raw-full/c-1957-2016-e4ei-t63-daymean-uv-nh.nc c-1957-2016-e4ei-t63-uv-nh-yseassd.nc

# seasonal running means
# cdo -b F32 -mergetime -runmean,2 -selseason,DJF c-1957-2016-e4ei-t63-uv-nh-seasmean.nc -runmean,2 -selseason,MAM c-1957-2016-e4ei-t63-uv-nh-seasmean.nc -runmean,2 -selseason,JJA c-1957-2016-e4ei-t63-uv-nh-seasmean.nc -runmean,2 -selseason,SON c-1957-2016-e4ei-t63-uv-nh-seasmean.nc d-1957-2016-e4ei-t63-uv-nh-seasmean-runmean.nc


## Orographie-Datenei-invariant.nc
#cdo selparam,-5 e4-invariant.nc e4-orography.nc
#cdo selparam,-6 ei-invariant.nc ei-orography.nc
#cdo -v -W -b F32 mergetime e4-orography.nc ei-orography.nc e4ei-orography.nc
#cdo timmean e4ei-orography.nc e4ei-geopotential.nc
#cdo -invertlat -remapbil,t63grid e4ei-geopotential.nc e4ei-t63-geopotential.nc











