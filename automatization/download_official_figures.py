#1. Importamos librerias necesarias
import requests
import re
import pandas as pd
import datetime


#Obtenemos día del sistema
#Para date de DataFrame
fecha = datetime.date.today()
print(fecha) 

#Para nombre de archivo
timestr = fecha.strftime("%Y_%m_%d")
print(timestr)

#Url de donde vamos a extraer la información
url = 'https://coronavirus.gob.mx/datos/Overview/info/getInfo.php'

#Siguiente dia, siguiente tupla. 
#Leemos nuestros dataFrame ya hecho
#DF_temp = pd.read_excel("/Users/marianafernandez/Documents/COSMO/covid19-mx-Data/data-raw/BaseSSA_"+ timestr +".csv")
DF_temp = pd.read_excel("/Users/marianafernandez/Documents/COSMO/covid19-mx-Data/data-raw/BaseSSA_2020_06_03.xlsx")


DF_temp

#. Creamos diccionario
# Actualizamos datos

value_1 = "Confirmados"
value_2 = "Negativos"
value_3 = "Sospechosos"
value_4 = "Defunciones"
value_5 = "Hospitalizados"

data = {
    "sPatType": {
        "key_1": value_1, 
        "key_2": value_2,
        "key_3": value_3,
        "key_4": value_4,
        "key_5": value_5,
    },
    "cve": "000",
    "nom": 'Nacional'
}

raw_data = requests.post(url, data=data).text
confirmados = int(re.search(r'document\.getElementById\("gsPosDIV"\)\.innerHTML = \((\d+)', raw_data).group(1))
negativos = int(re.search(r'document\.getElementById\("gsNegDIV"\)\.innerHTML = \((\d+)', raw_data).group(1))
sospechosos = int(re.search(r'document\.getElementById\("gsSosDIV"\)\.innerHTML = \((\d+)', raw_data).group(1))
defunciones = int(re.search(r'document\.getElementById\("gsDefDIV"\)\.innerHTML = \((\d+)', raw_data).group(1))
#hospitalizados = re.search(r'document\.getElementById\("vHos"\)\.innerHTML =  \\d+(?:\\.\\d+)?%', raw_data).group(1)
hospitalizados = 34.24


#Añadimos las tuplas siguientes
new_row3 = {'Fecha en reporte':fecha, 'Confirmados en reporte':confirmados,'Negativos en reporte':negativos, 'Sospechosos en reporte': sospechosos,
            'Defunciones en reporte': defunciones , "Porcentaje hospitalizados": hospitalizados}

DF_temp = DF_temp.append(new_row3, ignore_index = True)

#DF_temp = DF_temp.drop('Unnamed: 0', 1)


#DF_temp.to_excel("BaseSSA_"+ timestr +".csv", encoding="utf-8")
DF_temp.to_excel("BaseSSA_" + timestr + ".xlsx", sheet_name='out_vars',encoding="utf-8")


DF_temp

















