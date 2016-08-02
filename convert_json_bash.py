import pandas as pd
import numpy as np
import json
import time
import sys
import re
import csv


file_end = ".stocktwits_msgs.json"
line_split = re.compile('(?:"[^"]*"|.)+')
# y = int(sys.argv[1])
# m = int(sys.argv[2])
dates = []
bodys = []
# if m < 10:
#     filename = str(y) + '.0' + str(m) + file_end
# else: 
#     filename = str(y) + '.' + str(m) + file_end
filename = sys.argv[1]
y = int(filename[0:4])
m = int(filename[5:7])

start_time = time.time()    
print "Opening json file: %d %d" % (y, m)
sys.stdout.flush()
try: 
    with open(filename, 'r') as txt:

        for line in txt:
            stock_twits = json.loads(line)
            body = re.sub('\r\n|\n|\r', ' ', stock_twits['body'])
            if (body == '' or body == ""):
                print "Empty Body... Not Saving. JSON below:"
                print line
            else:
                # grabs the first 10 characters (e.g. 2011-01-01) in order to get the date #
                dates.append(stock_twits['object']['postedTime'][0:10])
                bodys.append(body)

                # intra day method below
                # post_time = int(stock_twits['object']['postedTime'][11:13])
                # if post_time < 10:
                #     dates.append(stock_twits['object']['postedTime'][0:10] + 'a')
                #     bodys.append(body)
                # elif post_time < 16:
                #     dates.append(stock_twits['object']['postedTime'][0:10] + 'm')
                #     bodys.append(body)
                # else:
                #     dates.append(stock_twits['object']['postedTime'][0:10] + 'p')
                #     bodys.append(body)

    print "Done! Elapsed time: ", time.time() - start_time
    data = []
    data.extend(zip(dates, bodys))
    df = pd.DataFrame(data=data, columns=["Dates", "Bodys"])
    if m < 10:
        export_name = str(y) + '.0' + str(m) + ".csv"
    else: 
        export_name = str(y) + '.' + str(m) + ".csv"
    df.to_csv(export_name, index=False, encoding="utf-8", quoting = csv.QUOTE_ALL)  
    print "Saving %d %d file..." % (y, m)
    print "Saved as %s" % export_name
except IOError:
    print "No file: ", filename
        
        

        







