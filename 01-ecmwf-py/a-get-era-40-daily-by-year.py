#!/usr/bin/env py27

from ecmwfapi import ECMWFDataServer

import numpy as np
import datetime as dt
#import calendar

server = ECMWFDataServer()

dir = "/automount/agh/Projects/skiefer/"

#date_first_start = dt.date(1958, 1, 1)
#date_first_end = dt.date(1958, 1, 31)
#date_last_end = dt.date(1978, 12, 1)
#date_last_end = dt.date(1978, 12, 31)

year_first = 1958
year_last = 1958

years_vec = np.arange(year_first, (year_last + 1))
months_vec = np.arange(1, 13)


for i in range(len(years_vec)):
    date_loop_start = dt.date(years_vec[i], 1, 1)
    date_loop_end = dt.date(years_vec[i], 12, 31)
    date_loop = str(date_loop_start)+'/to/'+str(date_loop_end)
    target_loop = dir+str(date_loop_start.year)+'-e4.nc'
    server.retrieve({
        'class': "e4",
        'dataset': "era40",
        'date': date_loop,
        'expver': "1",
        'format': "netcdf",
        'grid': "1.00/1.00",
        'levelist': "100/150/200/250/300/400/500/600/700/775/850/925/1000",
        'levtype': "pl",
        'param': "129.128/130.128/131.128/132.128/135.128/155.128",
        'step': "0",
        'stream': "oper",
        'target': target_loop,
        'time': "00/06/12/18",
        'type': "an"
    })


print('Done successfully.')

