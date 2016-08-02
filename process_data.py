import pandas as pd
import numpy as np
import json
import time
import sys
from datetime import datetime
import re
import csv
import combine_data
import multiprocessing
from clean_text import clean_text
from send_mail import send_email

class cleanProcess(multiprocessing.Process):
	def __init__(self, year):
		multiprocessing.Process.__init__(self)
		self.year = year
	def run(self):
		print "STARTING PROCESS " + str(self.year)
		for month in range(1,13):
			print "***Starting to clean %d %d" % (self.year, month)
			#clean_text(self.year, month)
			combine_data.combine_data(self.year, month)
			print "***Done with %d %d" % (self.year, month)
		#content = 'Clean-Up done with %s' % str(self.year)
		#send_email(content)
		print "EXITING PROCESS " + str(self.year)

if __name__ == '__main__':
	years = [2011,2012, 2013, 2014, 2015]
	p = [cleanProcess(x) for x in years]
	for i in range(0,len(years)):
		p[i].start()
	print "Exiting Main"