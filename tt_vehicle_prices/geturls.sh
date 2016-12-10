#! /bin/bash

domain="http://www.trinicarsforsale.com"
url="$domain/database/featuredcarsList.php?search=SEARCH"
echo "test"
while getopts ":l:h:k:m:" opt; do
  case $opt in
    l) 
       echo "lowest price: $OPTARG"
       pl="$OPTARG" ;;&
    h) 
       echo "highest price: $OPTARG"
       ph="$OPTARG" ;;&
    k) echo "make: $OPTARG"
       pk="$OPTARG" ;;&
    m) echo "model: $OPTARG"
       pm=$OPTARG ;;
    \?) 
      echo "Invalid option: -$OPTARG"
      exit 1
      ;;
    :)
      echo "Option -$OPTARG requires an argument."
      exit 1
      ;;
  esac
done
url="$url&make_of_car_keyword=$pk&model_of_car_keyword=$pm&asking_price_min=$pl&asking_price_max=$ph"
echo "tcfs url: $url"

selector="table[width=729] td[width=199] font:nth-child(8) text{}"
pageCount=`curl $url | ./pup $selector`
echo "$pageCount pages of search results found. Hit enter to continue..."
read _

touch urls.txt
rm urls.txt
selector="table[width=360] b a[href^=/database] attr{href}"
for (( i = 1; i <= pageCount; i++ )); do
	searchUrl="$url&page=$i"
	echo $searchUrl
	cat <(curl $searchUrl | ./pup $selector) >> urls.txt
done	

sed -i "s@^@${domain}@" urls.txt
./getpages.sh
./csvpages.sh