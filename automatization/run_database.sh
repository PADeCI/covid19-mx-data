
#!/bin/sh

echo "Git pull"
git pull
echo "Descargando base oficial"

#python3 download_official_data.py

echo "####################################"
echo "BASE DESCARGADA EXITOSAMENTE DE SSA"
echo "###################################"

Rscript hello.R
 
echo "###################################"
echo "BASE ACTUALIZADA EXITOSAMENTE" 
echo "##################################" 

date=$(date +"%Y-%m-%d")

message="Raw and Clean Data from SSA for "
final_message=$message$date
echo $final_message

git add .
git commit -m "'$final_message'"
git push
