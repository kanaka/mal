SOURCES_BASE = types.scala reader.scala printer.scala
SOURCES_LISP = env.scala core.scala stepA_mal.scala
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

TARGET_DIR=target/scala-2.11

all: $(TARGET_DIR)/mal.jar

dist: mal

mal: $(TARGET_DIR)/mal.jar
	cp $< $@

$(TARGET_DIR)/mal.jar:
	sbt assembly

$(TARGET_DIR)/classes/step%.class: step%.scala $(SOURCES)
	sbt assembly

clean:
	rm -rf mal target

.PHONY: all dist clean
