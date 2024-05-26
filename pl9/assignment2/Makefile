run: clean compile
	erl -noshell -s app run -s init stop

compile:
	erlc app.erl

clean:
	rm -f app.beam || true