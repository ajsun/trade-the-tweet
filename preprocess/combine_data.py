import pandas as pd
import numpy as np
import json
import time
import sys
from datetime import datetime
import csv

def combine_data(year, month):
    if month < 10:
        filename = str(year) + '.0' + str(month) + ".csv"
    else: 
        filename = str(year) + '.' + str(month) + ".csv"

	print "Combining %s..." % filename
	start_time = time.time()
	try:
		df = pd.read_csv(filename, delimiter = ',', quoting = csv.QUOTE_ALL)
	except:
		print "Couldn't find file %s" % filename
		return
	df['Dates'] = pd.to_datetime(df['Dates'].astype(str), format = '%Y-%m-%d')
	df['Bodys'] = df['Bodys'].astype(str)
	df2 = df.groupby('Dates')['Bodys'].apply(lambda x: "%s" % ' '.join(x)).reset_index()
	export_name = str(year) + '.' + str(month) + '_raw.csv'
	df2.to_csv(export_name, index=False, encoding="utf-8", quoting = csv.QUOTE_ALL)

	print "%s done, saved as %s. Elapsed time:%9.2f" % (filename, export_name, time.time()-start_time)