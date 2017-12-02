#  clim-jet-stream
Eine M.Sc.-Thesis über die Klimatologie des Jetstreams auf der Nordhemisphäre. Zeitraum Sep 1957 bis Jun 2017.
Dies ist das Code-Repository. Es ist geplant, das Text-Repository später in dieses zu überführen.


## Gründsätzliches
Die Verzeichnisse und Ordner dieser Arbeit sind hierarchisch gegliedert. Das heißt, dass Ordnernamen mit einer niedrigen Zahl zu Beginn des Projekts wichtig sind, während solche mit einer höheren Zahl in einer späteren Phase wichtig sind. Für Skripte gilt das größtenteils analog, jedoch werden dort Buchstaben statt Zahlen verwendet. Lediglich im Hauptordner wird anders vorgegangen. Hier sind die Skripte beginnend mit a-c die Hauptskripte, diese rufen Funktionen aus den Skripten d-m auf und verarbeiten so die Daten.


## Ordnerstruktur
### 00-archiv
Dateien und Unterordner in diesem Verzeichnis werden lediglich noch temporär als Vorlagen benötigt.

### 01-ecmwf-py
Die Python-Skripte in diesem Ordner dienen dem automatisierten Download der ERA-40- und ERA-Interim-Datensätze. Hierfür wird ein Account beim ECMWF sowie dessen Python-Paket benötigt.

### 02-cdo-bash
Dieser Ordner beinhaltet bash-Skripte zum Verarbeiten der monats- und/oder jahresweise heruntergeladenen Datensätze der ECMWF-Reanalysen.
Bash-Skripte zum Verarbeiten der Datensätze mittels [cdo](https://code.zmaw.de/)

### 03-pckg.cheb
In diesem Ordner befindet sich das für R geschriebene Paket pckg.cheb. Zweck des Packages ist es, über beliebige Datensätze einen Least-Squares-Fit mittels Chebyshev-Polynomen laufen zu lassen und über die ersten beiden Ableitungen dieses Fits die Maxima des Datensatzes zu errechnen.

### 04-data-nc
Dieser Ordner enthält die mit cdo vorverarbeiteten Datensätzte bestehend aus jeweils dem Mittelwert und der Standardabweichung über unterschiedliche Zeitskalen berechnet aus den täglichen Mittelwerten des kombinierten ERA-40- und ERA-Interim-Datensatz. Diese umfassen:
1. monatliche Mittelwerte


## Datei-/Skriptstruktur
### a-analyse_monthly_data.r
Skript zur Untersuchung von saisonalen Änderungen auf der Basis von Monatsmitteln

### aa-monthly_jet_detection.r
Einlesen des kombinierten ERA-40-Interim-Datensatzes und Anwendung der Jetstream-Detektions-Methoden auf diesen.

### ab-monthly_format_data.r
Formatierung der Datensätze als dataframe/tibble mit allen zentralen Größen.

### ac-visualize_results-case.r
Visualisierung von stichprobenartigen Einzelfällen für die Plausibilitätsanalyse.

### ad-visualize_results-comp-dijkcheb.r
Vergleich der beiden Methoden in Streudiagramen und Ellipsen differenziert nach Jettypen.

### ae-visualize_results-hovmoller.r
Visualisierung von Hovmöllerdiagramen für Positionen und Geschwindigkeiten der mittels Chebyshev- und Dijkstra-Methoden gefundenen polaren und subtropischen Jetstreams.

### d-jetstream_detection_schemes.r
Verschiedene Routinen zum Auffinden des Jets:
1. Zonales Maximum des Zonalwindes
2. Filtern der beiden stärksten zonalen Maxima des Least-Squares-Fits des Zonalwindes
3. Kürzeste Wegstrecke über Dijkstra-Algorithmus (nach Molnos et al. 2017)

### e-get-orography.r
Berechnung der Orographie und Einfluss auf zonales Maximum des zonalen Vorticity-Anteils
> noch nicht fertig

### f-help-functions.r
Kleine Hilfsfunktionen

### g-citations-bibtex.r
Ausgabe der bibtex-Zitationen zur Referenzierung dieser im Text der MA.

### h-prepare-seaicedata.r
Der Seeeis-Datensatz liegt monatsweise (Ein File für Jan, eines für Feb etc.), dieser Datensatz wird hier eingelesen und als csv in einem konsitenten Datensatz übergeben.

## To-Do-Liste
- [x] Skripte vervollständigen und überprüfen
- [x] Code PIK implementieren
- [x] Finalisierung der Methodik
- [x] Visualisierung erster Ergebnisse
- [x] Berechnung und Visualisierung von Hovmöller-Diagrammen 
- [ ] Zusammenhang Jetposition und arktisches Seeeis
- [ ] Finalisierung der Plots
- [ ] Konsistenzcheck und Kommentierung des Codes
- [ ] ~~Bezier-Curves ausprobieren~~

