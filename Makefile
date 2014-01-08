version:=$(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)

test:
	swipl -s tests/tests.pl -g run_tests -t halt

clean:
	rm -rf tmp/

tmp/markdown-$(version).tgz:
	mkdir -p tmp
	tar cvzf $@ prolog test pack.pl README.md tests.pl data

package: tmp/markdown-$(version).tgz

.PHONY: test clean package
