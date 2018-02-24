date=$1
for var in TN TX TG RR; do
  ./plot_domain_obs.R $date $var /lustre/storeB/project/metkl/senorge2/case/case_20180112/etc/case.conf --case.path /lustre/storeB/project/metkl/senorge2/case/case_20180112 --geoinfo.path /lustre/storeB/users/cristianl/geoinfo
done
exit 0
