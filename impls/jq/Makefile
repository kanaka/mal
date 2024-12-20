all:

clean:
	rm -fr .mypy_cache/

check:
	flake8 run
	pylint run
	mypy run

.PHONY: all clean check
