md:
	Rscript -e "rmarkdown::render('README.Rmd', output_file = 'README.md')"

site:
	Rscript -e "rmarkdown::render('README.Rmd', output_file = 'README.md')"
	Rscript -e "pkgdown::build_site()"

check:
	Rscript -e "devtools::check()"

checkfast:
	Rscript -e "devtools::check(build_args = '--no-build-vignettes')"

test:
	Rscript -e "devtools::test()"

cov:
	Rscript -e "covr::package_coverage(type = 'all', combine_types = FALSE, line_exclusions = list('R/plots.R', 'R/theme_nima.R', 'R/theme_jetblack.R', 'R/utils.R'))"

doc:
	Rscript -e "devtools::document()"

build:
	Rscript -e "devtools::build()"

buildfast:
	Rscript -e "devtools::build(vignettes = FALSE)"

style:
	Rscript -e "styler::style_pkg()"
