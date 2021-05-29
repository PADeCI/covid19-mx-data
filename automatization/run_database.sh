
#!/bin/sh

echo "Git pull"
git pull
echo "Descargando base oficial"

python3 download_official_data.py

echo "####################################"
echo "BASE DESCARGADA EXITOSAMENTE DE SSA"
echo "###################################"

Rscript "/home/admon/covid19-mx-data/main.R"

echo "###################################"
echo "BASE ACTUALIZADA EXITOSAMENTE" 
echo "##################################" 

#For everyday
#date=$(date +"%Y-%m-%d")

#For Yesterday
yest=$(date --date="yesterday")
echo "$yest"
yest=$(date --date="yesterday" +"%Y-%m-%d")
echo "The database was last verified on $yest"


message="Raw and Clean Data from SSA for "
final_message=$message$yest
echo $final_message

git add /home/admon/covid19-mx-data/
git commit -m "'$final_message'"
git push
