#! /bin/bash

pagePath="$1"
urlPrefix="http://www.trinicarsforsale.com"

#url
echo -n $urlPrefix >> out.csv
./pup -f $pagePath "table[width=510] tr:nth-child(21) td:nth-child(2) a attr{href}" | xargs | tr -d '\n' >> out.csv
echo -n ';' >> out.csv
#id
./pup -f $pagePath 'table[width="510"] tr:nth-child(1) td:nth-child(1) text{}' | xargs | tr -d '\n' >> out.csv
echo -n ';' >> out.csv
#registration-series
./pup -f $pagePath 'table[width="510"] tr:nth-child(1) td:nth-child(2) text{}' | xargs | tr -d '\n' >> out.csv
echo -n ';' >> out.csv
#make
./pup -f $pagePath 'table[width="510"] tr:nth-child(3) td:nth-child(2) text{}' | xargs | tr -d '\n' >> out.csv
echo -n ';' >> out.csv
#model
./pup -f $pagePath 'table[width="510"] tr:nth-child(4) td:nth-child(2) text{}' | xargs | tr -d '\n' >> out.csv
echo -n ';' >> out.csv
#year
./pup -f $pagePath 'table[width="510"] tr:nth-child(5) td:nth-child(2) text{}' | xargs | tr -d '\n' >> out.csv
echo -n ';' >> out.csv
#mileage
./pup -f $pagePath 'table[width="510"] tr:nth-child(8) td:nth-child(2) text{}' | xargs | tr -dc '0-9' >> out.csv
echo -n ';' >> out.csv
#price
./pup -f $pagePath 'table[width="510"] tr:nth-child(17) td:nth-child(2) text{}' | xargs | tr -dc '0-9' >> out.csv
echo -n ';' >> out.csv
#transmission
./pup -f $pagePath 'table[width="510"] tr:nth-child(9) td:nth-child(2) text{}' | xargs >> out.csv
