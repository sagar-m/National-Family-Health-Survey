#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar  4 19:46:22 2018

@author: sherrymukim
"""

import pandas as pd

data = pd.read_stata('/Users/sherrymukim/Documents/nfhs/IAHR71DT/IAHR71FL.DTA',chunksize=5000)

for chunk in data:
    print(chunk[0].info())

data.to_csv('/Users/sherrymukim/Documents/nfhs/IAHR71DT/IAHR71FL2.csv')

########################

def load_large_dta(fname):
    import sys

    #cols = ['hv201','hv202','hv204','hv205','hv206','hv207','hv208','hv209','hv210','hv211','hv212','hv213','hv214','hv215','hv216','hv217','hv218','hv219','hv220','hv221','hv225','hv226','hv227','hv228','hv230a','hv230b','hv232','hv232b','hv234','hv234a','hv235','hv236','hv237','hv237a','hv237b','hv237c','hv237d','hv237e','hv237f','hv237g','hv237h','hv237i','hv237j','hv237k','hv237x','hv237z','hv238','hv239','hv240','hv241','hv242','hv243a','hv243b','hv243c','hv243d','hv244','hv245','hv246','hv246a','hv246b','hv246c','hv246d','hv246e','hv246f','hv247','hv248','hv249','hv250','hv251','hv252','hv253','hv270','hv271','hml1','hml2','hhid','hv000','hv001','hv002','hv003','hv009','hv010','hv011','hv012','hv014','hv015','hv020','hv024','hv025','hv026','hv035','hv104','hv105','hv101','hv106','hv107','hv108 ','hv109 ','hv110 ','hv115','hv120','hv121','hv122','hv123','hv124','hv129']
    cols = ['hv201','hv202','hv204','hv205','hv206','hv207','hv208','hv209','hv210','hv211','hv212','hv213','hv214','hv215','hv216','hv217','hv218','hv219','hv220','hv221','hv225','hv226','hv227','hv228','hv230a','hv230b','hv232','hv232b','hv234','hv234a','hv235','hv236','hv237','hv237a','hv237b','hv237c','hv237d','hv237e','hv237f','hv237g','hv237h','hv237i','hv237j','hv237k','hv237x','hv237z','hv238','hv239','hv240','hv241','hv242','hv243a','hv243b','hv243c','hv243d','hv244','hv245','hv246','hv246a','hv246b','hv246c','hv246d','hv246e','hv246f','hv247','hv252','hv253','hv270','hv271','hml1','hml2','hhid','hv000','hv001','hv002','hv003','hv009','hv010','hv011','hv012','hv013','hv014','hv015','hv020','hv024','hv025','hv026','hv035']
    reader = pd.read_stata(fname, iterator=True, columns=cols)
    #reader = pd.read_stata(fname, iterator=True)
    df = pd.DataFrame()

    try:
        chunk = reader.get_chunk(100*100)
        while len(chunk) > 0:
            df = df.append(chunk, ignore_index=True)
            chunk = reader.get_chunk(100*100)
            print('.'),
            sys.stdout.flush()
    except (StopIteration, KeyboardInterrupt):
        pass

    print('\nloaded {} rows'.format(len(df)))

    return df

#import pandas as pd

%time df = load_large_dta('/Users/sherrymukim/Documents/nfhs/IAHR71DT/IAHR72DT/IAHR72FL.DTA')

df.to_csv('/Users/sherrymukim/Documents/nfhs/IAHR71DT/IAHR72FL2.csv',sep=',',header=True, index=False)

################

# dask generator

# Define the generator: dataframes
dataframes = (pd.read_stata(fname, iterator=True) for file in filenames)

# Create the list comprehension: monthly_delayed
#monthly_delayed = [pct_delayed(df) for df in dataframes]

###################
import pandas as pd

from dask import delayed

@delayed 
def read_one(filename):     
    return pd.read_stata(filename) 

#print columns
@delayed
def print_cols(df):
    return df.head()

# Loop over the provided filenames list and call read_one: df
#for file in filenames:
df = read_one('/Users/sherrymukim/Documents/nfhs/IAHR71DT/IAHR71FL.DTA')

result = print_cols(df)

%time print(result.compute())

##############3

# Define @delayed-function read_flights
@delayed
def read_flights(filename):

    # Read in the DataFrame: df
    df = pd.read_csv(filename, parse_dates=['FL_DATE'])

    # Calculate df['WEATHER_DELAY']
    df['WEATHER_DELAY'] = df['WEATHER_DELAY'].replace(0,np.nan)

    # Return df
    return df


# Loop over filenames with index filename
for filename in filenames:
    # Apply read_flights to filename; append to dataframes
    dataframes.append(read_flights(filename))

# Compute flight delays: flight_delays
flight_delays = dd.from_delayed(dataframes)

# Print average of 'WEATHER_DELAY' column of flight_delays
print(flight_delays['WEATHER_DELAY'].mean().compute())


df_cols = pd.read_csv('/Users/sherrymukim/Documents/nfhs/IAHR71DT/IAHR71FL.csv')
df_cols = df_cols.iloc[:3,:]
df_cols.head()



