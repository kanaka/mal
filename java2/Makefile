%.class: src/mal/
	@rm -rf $*
	@test -d lib || mkdir -p lib 
	@if [ ! -f lib/jline-2.13.jar ]; \
	then \
	echo "downloading jline-2.13.jar form maven repo";\
	curl -o lib/jline-2.13.jar "http://central.maven.org/maven2/jline/jline/2.13/jline-2.13.jar" ;\
	fi
	@javac -cp lib/jline-2.13.jar src/mal/*.java -d .

clean:
	@rm -rf out/
	@rm -rf mal/
