conf=/disk1/data/case/case_20180112/etc/case.conf
for var in TG RR TX TN; do
  R --vanilla 1970.01.01 2018.01.24 $var $conf < case_archive_by_date.R
done
exit 0
