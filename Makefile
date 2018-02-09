

merkatilo: merkatilo-test-data
	@raco make main.rkt chart.rkt

clean:
	@rm -rf `find . -type d -name compiled`

/tmp/merkatilo-test-data/ema-3.txt:
	@rm -rf /tmp/merkatilo-test-data && (cd /tmp && git clone https://github.com/JohnCGriffin/merkatilo-test-data)

merkatilo-test-data: /tmp/merkatilo-test-data/ema-3.txt
	@test -d /tmp/merkatilo-test-data

test: merkatilo
	@raco test `fgrep -l rackunit *.rkt core/*.rkt private/*.rkt`

