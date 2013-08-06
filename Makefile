VERSION := $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)

all: package

test:
	swipl -s tests -g md_tests -t halt

clean:
	rm -rf tmp/

tmp/markdown-$(VERSION).tgz:
	mkdir -p tmp
	tar cvzf $@ prolog test pack.pl README.md tests.pl data

package: tmp/markdown-$(VERSION).tgz

.PHONY: test clean package
