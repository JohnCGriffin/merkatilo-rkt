

merkatilo: merkatilo-test-data
	@raco make main.rkt chart.rkt

documentation:
	raco setup -l merkatilo && echo '.navsettop { display:none }' >> doc/manual-style.css

rkt-doc.tar: documentation
	(cd doc && tar cf ../rkt-doc.tar .)

clean:
	@rm -rf `find . -type d -name compiled` doc rkt-doc.tar coverage

/tmp/merkatilo-test-data/ema-3.txt:
	@rm -rf /tmp/merkatilo-test-data && (cd /tmp && git clone https://github.com/JohnCGriffin/merkatilo-test-data)

merkatilo-test-data: /tmp/merkatilo-test-data/ema-3.txt
	@test -d /tmp/merkatilo-test-data

test: merkatilo
	@raco test -x . 

version:
	@git describe --abbrev=0

bench:
	racket standard-bench.rkt
