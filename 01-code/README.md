#  clim-jet-stream
In diesem Ordner liegt der Quellcode für die Detektion und die Analyse der/s Jetstreams.
Wie im Hauptordner sind die Dateien und Ordner hierarchisch sortiert.

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

### f-help-functions.r
Kleine Hilfsfunktionen

### g-citations-bibtex.r
Ausgabe der bibtex-Zitationen zur Referenzierung dieser im Text der MA.

### h-prepare-seaicedata.r
Der Seeeis-Datensatz liegt monatsweise (Ein File für Jan, eines für Feb etc.), dieser Datensatz wird hier eingelesen und als csv in einem konsitenten Datensatz übergeben.

