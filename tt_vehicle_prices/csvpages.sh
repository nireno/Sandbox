touch out.csv
rm out.csv

#header
echo "url;id;plate;make;model;year;mileage;price;transmission" >> out.csv

for file in pages/*; do 
	./csvpage.sh "$file";
done
