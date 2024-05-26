c1: cleanC compileC
	erl -noshell -sname client1 \
		-s client start client1 router@VAL-235 \
		-s client list_servers_from_router client1 \
		-s client join_server client1 "Erlang Chat Server" \

c2:
	erl -noshell -sname client2 \
		-s client start client2 router@VAL-235 \
		-s client list_servers_from_router client2 \
		-s client join_server client2 "Erlang Chat Server" \
		-s client send_message client2 "Erlang Chat Server" "Hello erlang enthusiasts!"

r: cleanR compileR
	erl -noshell -sname router -run router start

s: cleanS compileS
	erl -noshell -sname server -s server start "Erlang Chat Server" router@VAL-235

compileC:
	erlc client.erl

cleanC:
	rm -f client.beam || true

compileR:
	erlc router.erl

cleanR:
	rm -f router.beam || true

compileS:
	erlc server.erl

cleanS:
	rm -f server.beam || true