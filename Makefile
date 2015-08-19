
all: output print_ver/ozone.pdf

tmp/ozone_raw.RData:
	Rscript -e "source('01_load.R')"
  
tmp/ozone_clean.RData: tmp/ozone_raw.RData
	Rscript -e "load('tmp/ozone_raw.RData'); source('02_clean.R')"
	
tmp/analysed.RData: tmp/ozone_clean.RData
	Rscript -e "load('tmp/ozone_clean.RData'); source('03_analysis.R')"
	
output: tmp/analysed.RData
	Rscript -e "load('tmp/analysed.RData'); source('04_output.R')"
	
print_ver/ozone.pdf: tmp/analysed.Rdata
	Rscript -e "rmarkdown::render('print_ver/ozone.Rmd'); extrafont::embed_fonts('print_ver/ozone.pdf')"
