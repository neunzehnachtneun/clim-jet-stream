#!/usr/bin/env py27

from ecmwfapi import ECMWFDataServer

import numpy as np
import datetime as dt
import calendar

server = ECMWFDataServer()

dir = "/automount/agh/Projects/skiefer/"

year_first = 1957
year_last = 1957

years_vec = np.arange(year_first, (year_last + 1))
months_vec = np.arange(1, 13)

for i in range(len(years_vec)):
    for ii in range(len(months_vec)):
        date_loop_start = dt.date(years_vec[i], months_vec[ii], 1)
        month_len = calendar.monthrange(years_vec[i], months_vec[ii])[1]
        date_loop_end = dt.date(years_vec[i], months_vec[ii], month_len)
        date_loop = str(date_loop_start) + '/to/' + str(date_loop_end)
        target_loop = dir + str(date_loop_start.year) + '-' + str(date_loop_start.month) + '-e4.nc'
        server.retrieve({
            'class': "e4",
            'dataset': "era40",
            'date': date_loop,
            'expver': "1",
            'format': "netcdf",
            'grid': "1.00/1.00",
            'levelist': "100/150/200/250/300/400/",
            'levtype': "pl",
            'param': "129.128/130.128/131.128/132.128/135.128/155.128",
            'step': "0",
            'stream': "oper",
            'target': target_loop,
            'time': "00/06/12/18",
            'type': "an"
        })

print('Done successfully.')


