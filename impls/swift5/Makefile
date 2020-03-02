step%: 
	mkdir -p ./out
	swift build --product $@
	cp "$(shell swift build --show-bin-path)/$@" "./out/$@"

clean:
	rm -rf ./out
	rm -rf ./.build

