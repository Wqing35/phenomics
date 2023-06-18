cd /mdshare/node8/txmdata/Phenomics

bash /home/timo/software/webConnect.sh

wget -O /mdshare/node8/txmdata/Phenomics/weekly_online_paper_metrices/SearchResults.csv --no-check-certificate "https://link.springer.com/search/csv?query=&search-within=Jounal&facet-journal-id=43657"

/usr/bin/Rscript /mdshare/node8/txmdata/Phenomics/weekly_online_paper_metrices/1_get_metrices.R
/usr/bin/Rscript /mdshare/node8/txmdata/Phenomics/weekly_online_paper_metrices/2_plot_fig2.R
/usr/bin/Rscript /mdshare/node8/txmdata/Phenomics/weekly_online_paper_metrices/2_plot_fig3.R
/usr/bin/Rscript /mdshare/node8/txmdata/Phenomics/weekly_online_paper_metrices/2_plot_fig4.R
/usr/bin/Rscript /mdshare/node8/txmdata/Phenomics/weekly_online_paper_metrices/3_get_readme.R
/usr/bin/Rscript /mdshare/node8/txmdata/Phenomics/weekly_online_paper_metrices/4_pred_IF.R
/usr/bin/Rscript /mdshare/node8/txmdata/Phenomics/weekly_online_paper_metrices/5_each_month_IF.R

rm -rf /mdshare/node8/txmdata/Phenomics/Rplots.pdf
rm -rf /mdshare/node8/txmdata/Phenomics/.Rhistory

git add -A; DATE=$(date "+%Y%m%d"); git commit -m $DATE --no-verify; git push




