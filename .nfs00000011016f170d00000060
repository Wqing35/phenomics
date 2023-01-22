cd /mdshare/node8/tianlejin/Phenomics

bash /mdshare/node8/tianlejin/software/webConnect.sh

wget -O /mdshare/node8/tianlejin/Phenomics/weekly_online_paper_metrices/SearchResults.csv --no-check-certificate "https://link.springer.com/search/csv?query=&search-within=Jounal&facet-journal-id=43657"

Rscript /mdshare/node8/tianlejin/Phenomics/weekly_online_paper_metrices/1_get_metrices.R
Rscript /mdshare/node8/tianlejin/Phenomics/weekly_online_paper_metrices/2_plot_fig2.R
Rscript /mdshare/node8/tianlejin/Phenomics/weekly_online_paper_metrices/2_plot_fig3.R
Rscript /mdshare/node8/tianlejin/Phenomics/weekly_online_paper_metrices/2_plot_fig4.R
Rscript /mdshare/node8/tianlejin/Phenomics/weekly_online_paper_metrices/3_get_readme.R
Rscript /mdshare/node8/tianlejin/Phenomics/weekly_online_paper_metrices/4_pred_IF.R

rm -rf /mdshare/node8/tianlejin/Phenomics/Rplots.pdf

git add -A
DATE=$(date "+%Y%m%d")
git commit -m $DATE --no-verify
git push



