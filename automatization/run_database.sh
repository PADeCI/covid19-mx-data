
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
sum=1
date=$(date +"%Y-%m-%d" -$s)

message="Raw and Clean Data from SSA for "
final_message=$message$date
echo $final_message

git add /home/admon/covid19-mx-data/
git commit -m "'$final_message'"
git push
