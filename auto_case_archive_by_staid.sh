for var in TG RR TX TN; do
  R --vanilla $var /disk1/data/case/case_20180112/etc/case.conf < case_archive_by_staid.R
done
exit 0
