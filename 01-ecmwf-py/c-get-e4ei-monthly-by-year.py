#!/usr/bin/env py27

from ecmwfapi import ECMWFDataServer

import numpy as np
import datetime as dt
import calendar

server = ECMWFDataServer()

dir = "/automount/agh/Projects/skiefer/"

year_first = 1957
year_last = 1978

years_vec = np.arange(year_first, (year_last + 1))
months_vec = np.arange(1, 13)


for i in range(len(years_vec)):
    date_loop_start = dt.date(years_vec[i], 1, 1)
    date_loop_end = dt.date(years_vec[i], 12, 31)
    date_loop = str(date_loop_start)+'/to/'+str(date_loop_end)
    target_loop = dir+str(date_loop_start.year)+'-e4.nc'
    server.retrieve({
    	"class": "e4",
    	"dataset": "era40",
        "format": "netcdf",
    	"date": date_loop,
	"grid": "1.00/1.00",
        'levelist': "100/150/200/250/300/400",
    	"levtype": "pl",
    	"param": "129.128/130.128/131.128/132.128/135.128/155.128",
    	"step": "0",
    	"stream": "moda",
    	"type": "an",
    	"target": target_loop,
    })
print('E4-datasets successfully downloaded.')


year_first = 1979
year_last = 2017

years_vec = np.arange(year_first, (year_last + 1))
months_vec = np.arange(1, 13)

for i in range(len(years_vec)):
    date_loop_start = dt.date(years_vec[i], 1, 1)
    date_loop_end = dt.date(years_vec[i], 12, 31)
    date_loop = str(date_loop_start)+'/to/'+str(date_loop_end)
    target_loop = dir+str(date_loop_start.year)+'-ei.nc'
    server.retrieve({
        'class': "ei",
        'dataset': "interim",
        "format": "netcdf",
    	"date": date_loop,
	"grid": "1.00/1.00",
        'levelist': "100/150/200/250/300/400",
    	"levtype": "pl",
    	"param": "129.128/130.128/131.128/132.128/135.128/155.128",
    	"step": "0",
    	"stream": "moda",
    	"type": "an",
    	"target": target_loop,
    })
print('EI-datasets successfully downloaded.')

