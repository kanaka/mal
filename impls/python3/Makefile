# make check sources=reader.py    may be convenient
sources ?= *.py

f8 += D100 # Missing docstring in public module
f8 += D101 # Missing docstring in public class
f8 += D102 # Missing docstring in public method
f8 += D103 # Missing docstring in public function
f8 += D105 # Missing docstring in magic method
f8 += D107 # Missing docstring in __init__
f8 += I100 # order of import statements (incompatible with pylint)
f8 += W503 # line break before binary operator (incompatible with 504)
pl += missing-module-docstring
pl += missing-class-docstring
pl += missing-function-docstring
pl += R0801 # Similar lines in 2 files (steps...)

all:

check:
	pylint --disable=$(shell echo $(pl) | sed 's/ /,/g') $(sources)
	mypy $(sources)
	flake8 --ignore=$(shell echo $(f8) | sed 's/ /,/g') $(sources)

clean:
	rm -f *~
	rm -fr __pycache__/ .mypy_cache/
