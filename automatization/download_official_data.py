#Download the Data Base from Federal Goverment

import csv
import urllib.request
from io import StringIO

import requests, zipfile, io
import os

from datetime import date

os.chdir("/home/admon/covid19-mx-data/data-raw")
today = date.today() - datetime.timedelta(days=1)
d1 = today.strftime("%y%m%d")
file_name = d1 + "COVID19MEXICO.csv"

#zip_file_url = "http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
zip_file_url = "http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
r = requests.get(zip_file_url)
z = zipfile.ZipFile(io.BytesIO(r.content))
#z.extractall(path = '/home/admon/covid19-mx-data/data-raw')
z.extractall(path = '/home/admon/covid19-mx-data/data-raw')

#Rename csv file since april 25
os.rename("COVID19MEXICO.csv",file_name)
