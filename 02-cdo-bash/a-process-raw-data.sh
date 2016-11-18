# Programm zum Verarbeiten der Raw-nc-files

# monatliche mittelwerte
# cdo monmean 1958-2015-e4ei-t63.nc 1958-2015-e4ei-t63-monmean.nc
# cdo selparam,-5,-6 1958-2015-e4ei-t63-monmean.nc 1958-2015-e4ei-t63-uv-monmean.nc
# monatliche standardabweichungen
# cdo monstd 1958-2015-e4ei-t63.nc 1958-2015-e4ei-t63-monstd.nc
# cdo selparam,-5,-6 1958-2015-e4ei-t63-monstd.nc 1958-2015-e4ei-t63-uv-monstd.nc

# u- und v-windkomponente aus datensatz ziehen
cdo selparam,-5,-6 1958-2015-e4ei-t63.nc 1958-2015-e4ei-t63-uv.nc

# nord- und südhemisphäre trennen
cdo distgrid,1,2 1958-2015-e4ei-t63-uv.nc ofile
mv ofile00000.nc 1958-2015-e4ei-t63-sh-uv.nc
mv ofile00001.nc 1958-2015-e4ei-t63-nh-uv.nc 

# saisonale mittelwerte
cdo seasmean 1958-2015-e4ei-t63-nh-uv.nc 1958-2015-e4ei-t63-nh-uv-seasmean.nc
# saisonale standardabweichungen
cdo seasstd 1958-2015-e4ei-t63-nh-uv.nc 1958-2015-e4ei-t63-nh-uv-seasstd.nc

# monatliche mittelwerte
cdo monmean 1958-2015-e4ei-t63-nh-uv.nc 1958-2015-e4ei-t63-nh-uv-monmean.nc
# monatliche standardabweichungen
cdo monstd 1958-2015-e4ei-t63-nh-uv.nc 1958-2015-e4ei-t63-nh-uv-monstd.nc




