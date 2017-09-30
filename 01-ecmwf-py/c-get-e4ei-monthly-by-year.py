#!/usr/bin/env py27

import datetime as dt

import numpy as np
from ecmwfapi import ECMWFDataServer

server = ECMWFDataServer()

dir = "/automount/agh/Projects/skiefer/02-raw-by-year/"

year_first = 1957
year_last = 1978

years_vec = np.arange(year_first, (year_last + 1))


for i in range(len(years_vec)):
    date_loop = str(dt.date(years_vec[i], 1, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 2, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 3, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 4, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 5, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 6, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 7, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 8, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 9, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 10, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 11, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 12, 1).strftime('%Y%m%d'))
    target_loop = dir+str(years_vec[i])+'-e4.nc'
    server.retrieve({
        "class": "e4",
        "dataset": "era40",
        "format": "netcdf",
        "date": date_loop,
        "grid": "1.00/1.00",
        "levelist": "100/150/200/250/300/400",
        "levtype": "pl",
        "param": "129.128/130.128/131.128/132.128/135.128/155.128",
        "step": "0",
        "stream": "moda",
        "type": "an",
        "target": target_loop
    })
print('E4-datasets successfully downloaded.')


year_first = 1979
year_last = 2017

years_vec = np.arange(year_first, (year_last + 1))

for i in range(len(years_vec)):
    date_loop = str(dt.date(years_vec[i], 1, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 2, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 3, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 4, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 5, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 6, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 7, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 8, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 9, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 10, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 11, 1).strftime('%Y%m%d')) + '/' + \
        str(dt.date(years_vec[i], 12, 1).strftime('%Y%m%d'))
    target_loop = dir+str(years_vec[i])+'-ei.nc'
    server.retrieve({
        "class": "ei",
        "dataset": "interim",
        "format": "netcdf",
        "date": date_loop,
        "grid": "1.00/1.00",
        "levelist": "100/150/200/250/300/400",
        "levtype": "pl",
        "param": "129.128/130.128/131.128/132.128/135.128/155.128",
        "step": "0",
        "stream": "moda",
        "type": "an",
        "target": target_loop
    })
print('EI-datasets successfully downloaded.')