# TheyChat!

## Clients

### Use cases

* Join Server. ✅ 
* Leave Server.
* Send/Receive messages. (Broadcast to server) ✅ 

## Router

### Use cases

* Give the pid of the server to the client upon request. ✅ 
* Monitor and solve server failures.

### State

* List of servers.

### Architecture

router and router_monitor supervise each other, if one fails the other restarts.

router restarts server_monitor in case of failure.

## Server

### Use cases

* Clients can chat by sharing messages. ✅ 
* Fail-safe mechanims.

### State

* Keeps a list of clients.

### Architecture

server_monitor restarts the server_chat in case of failure.

server_chat will remove a client from the client list when the client fails.

## Process Monitors

### Use Cases

* Restart the monitored process in case of failure.

* State backup. Restarts with the state the process had before being killed.

## Grading Criteria

* Multiple clients and at least one server communicating through messages. 25%

* Router basic implementation.10%

* Router fault-tolerance. 10%

* Router supervises server. 10%

* Server basic functionality. 15%

* Server fault-tolerance. 15%

* Supports different servers (for different topics). 15%
