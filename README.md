#  r-code-git
Eine M.Sc.-Thesis über die Klimatologie des Jetstreams auf der Nordhemisphäre

### Ordnerstruktur
##### 00-archiv
Nicht länger benötigte Skripte und Programme
##### 01-ecmwf-py
Python-Skripte zum Bezug der ERA-40- und ERA-Interim-Datensätze
##### 02-cdo-bash
Bash-Skripte zum Verarbeiten der Datensätze mittels cdo (https://code.zmaw.de/projects/cdo/)
##### 03-data-nc
Verarbeitete Datensätze: Monatliche und saisonale Mittelwerte, mittlerer Zustand sowie saisonaler moving-average auf Nordhemisphäre

### Dateistruktur
##### a-read_era_ncdf.r
Routine zum Einlesen von ncdf-Daten
##### b-locate_jetstream_discrete_2d.r
Routine zum Auffinden des Jets über meridionales Maximum
##### c-locate_jetstream_polynomial_2d.r
Routine zum Auffinden der Jets über Least-Squares-Methode mit Chebyshev-Polynomen
##### d-compare_seq_vs_std_fit.r
Skript zum Vergleich des sequentiellen und des Standard-Fits
##### e-analyse_seasonal_change.r
Routine zur Analyse der saisonalen Veränderungen
##### f-mean_jet.r
Berechnung des mittleren Zustands der Atmosphäre aus kombiniertem Datensatz ERA-40 + ERA-Interim 1957-2016
##### g-get-orography.r
Berechnug der Orography und deren Anteil am Zonalwind-Maximum
##### m-plots-master.r
Standard-Plot-Routinen

### To Do


