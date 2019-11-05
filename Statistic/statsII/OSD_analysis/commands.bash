### bash commands
### format the amino acid content table 
tar xfv osd.traits.tar
cd osd.traits
ls *.gz | while read line; do tar zxfv $line; done
ls *.dir/*amino-acid-content.csv | while read line; do tail -n +2 $line ; done > aa-content.csv 
head -1 mg1002-OSD117_traits.dir/mg1002-OSD117.amino-acid-content.csv > header.csv
cat header.csv aa-content.csv  > tmp.csv
mv tmp.csv aa-content.csv

### format the simple traits table
ls *.dir/*.simple-traits.csv | while read line; do tail -1 $line; done > simple.traits.csv
head -1 mg1002-OSD117_traits.dir/mg1002-OSD117.simple-traits.csv > header.csv
cat header.csv simple.traits.csv > tmp.csv
mv tmp.csv simple.traits.csv

