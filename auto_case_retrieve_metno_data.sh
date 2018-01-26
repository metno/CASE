yyyy1=1950
yyyy2=2018
conf="/disk1/data/case/case_20180112/etc/case.conf"
for var in TG RR TX TN; do
  echo "R --vanilla $yyyy1 $yyyy2 $var $conf < case_retrieve_metno_data.R > ./log/case_retrieve_metno_data_$var.log"
  R --vanilla $yyyy1 $yyyy2 $var $conf < case_retrieve_metno_data.R > ./log/case_retrieve_metno_data_$var.log
done
exit 0
