# TheyChat!

### Clients use Cases

* Join Server. ✅ 
* Leave Server. ✅ 
* Send/Receive messages (Broadcast). ✅ 

## Router use cases

* Give the pid of the server to the client upon request. ✅ 
* router
    * restarts router_monitor when it fails. ✅ 
    * restarts server_monitor when it fails. ✅ 
* router_monitor 
    * restarts router when it fails. ✅ 
    * restores router state (list of servers). ❌ 

## Server use cases

* Clients can chat by sharing messages. ✅ 
* Remove client when it fails. ✅ 
* server_monitor 
    * restarts the server_chat in case of failure. ✅ 
    * restores server_chat state (list of clients). ❌ 