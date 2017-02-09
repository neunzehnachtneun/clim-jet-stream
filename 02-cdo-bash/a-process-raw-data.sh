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
# cdo -daymean -remapbil,t63grid a-1957-2016-e4ei-1deg.nc c-1957-2016-e4ei-t63-daymean.nc

# distribute horizontal grid
# cdo -distgrid,1,2 c-1957-2016-e4ei-t63-daymean.nc d-1957-2016-e4ei-t63-daymean-
# mv d-1957-2016-e4ei-t63-daymean-00000.nc  d-1957-2016-e4ei-t63-daymean-sh.nc
# mv d-1957-2016-e4ei-t63-daymean-00001.nc  d-1957-2016-e4ei-t63-daymean-nh.nc

# extract u v
# cdo -selparam,-5,-6 d-1957-2016-e4ei-t63-daymean-nh.nc e-1957-2016-e4ei-t63-daymean-nh-uv.nc

# change wd to root-directory				####
cd ../

# calculate fields of mean and sd
# cdo -timmean 03-raw-full/e-1957-2016-e4ei-t63-daymean-nh-uv.nc a-1957-2016-e4ei-t63-nh-uv-timmean.nc
# cdo -timstd 03-raw-full/e-1957-2016-e4ei-t63-daymean-nh-uv.nc a-1957-2016-e4ei-t63-nh-uv-timsd.nc

# monthly means
# cdo -monmean 03-raw-full/e-1957-2016-e4ei-t63-daymean-nh-uv.nc b-1957-2016-e4ei-t63-nh-uv-monmean.nc
# cdo -ymonmean 03-raw-full/e-1957-2016-e4ei-t63-daymean-nh-uv.nc b-1957-2016-e4ei-t63-nh-uv-ymonmean.nc
# cdo -monstd 03-raw-full/e-1957-2016-e4ei-t63-daymean-nh-uv.nc b-1957-2016-e4ei-t63-nh-uv-monsd.nc
# cdo -ymonstd 03-raw-full/e-1957-2016-e4ei-t63-daymean-nh-uv.nc b-1957-2016-e4ei-t63-nh-uv-ymonsd.nc

# seasonal means
# cdo -seasmean 03-raw-full/e-1957-2016-e4ei-t63-daymean-nh-uv.nc c-1957-2016-e4ei-t63-nh-uv-seasmean.nc
# cdo -yseasmean 03-raw-full/e-1957-2016-e4ei-t63-daymean-nh-uv.nc c-1957-2016-e4ei-t63-nh-uv-yseasmean.nc
# cdo -seasstd 03-raw-full/e-1957-2016-e4ei-t63-daymean-nh-uv.nc c-1957-2016-e4ei-t63-nh-uv-seassd.nc
# cdo -yseasstd 03-raw-full/e-1957-2016-e4ei-t63-daymean-nh-uv.nc c-1957-2016-e4ei-t63-nh-uv-yseassd.nc

# seasonal running means
cdo -b F32 -mergetime -runmean,2 -selseason,DJF c-1957-2016-e4ei-t63-nh-uv-seasmean.nc -runmean,2 -selseason,MAM c-1957-2016-e4ei-t63-nh-uv-seasmean.nc -runmean,2 -selseason,JJA c-1957-2016-e4ei-t63-nh-uv-seasmean.nc -runmean,2 -selseason,SON c-1957-2016-e4ei-t63-nh-uv-seasmean.nc d-1957-2016-e4ei-t63-nh-uv-seasmean-runmean.nc


## Orographie-Datenei-invariant.nc
#cdo selparam,-5 e4-invariant.nc e4-orography.nc
#cdo selparam,-6 ei-invariant.nc ei-orography.nc
#cdo -v -W -b F32 mergetime e4-orography.nc ei-orography.nc e4ei-orography.nc
#cdo timmean e4ei-orography.nc e4ei-geopotential.nc
#cdo -invertlat -remapbil,t63grid e4ei-geopotential.nc e4ei-t63-geopotential.nc




###
# change wd to folder with bash-script
cd "$(dirname "$0")"




