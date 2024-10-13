step%:
	swift build --product $@
	[ -L .build/$@ ] || ln -s "$(shell swift build --show-bin-path)/$@" .build/$@
clean:
	rm -fr .build/
