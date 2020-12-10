#Download the Data Base from Federal Goverment

import csv
import urllib.request
from io import StringIO

import requests, zipfile, io
zip_file_url = "http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
zip_file_url = "http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
r = requests.get(zip_file_url)
z = zipfile.ZipFile(io.BytesIO(r.content))
z.extractall(path = '/Users/marianafernandez/Documents/PADeCI/covid19-mx-data/data-raw/')
