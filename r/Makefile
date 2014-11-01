.PHONY:
libs: lib/rdyncall

lib/rdyncall:
	curl -O http://cran.r-project.org/src/contrib/Archive/rdyncall/rdyncall_0.7.5.tar.gz
	mkdir -p lib
	R CMD INSTALL rdyncall_0.7.5.tar.gz -l lib/
	rm rdyncall_0.7.5.tar.gz
