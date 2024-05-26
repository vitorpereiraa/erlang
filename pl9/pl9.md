# Introduction to distributed Erlang

1. Create a process that, whenever receives a message, prints it along with the id of the sender and then, sends it back to the sender.

2. Create a simple publisher/subscriber where a server process (publisher) shows messages to all its subscribed clients.

# Perspective
This assignment introduces fault-tolerant programming in the functional language Erlang. In summary, upon completion you should understand:
* How to create processes.
* How to communicate with processes with message passing.
* How to monitor process behaviour by using links.
* How to build a simple fault-tolerant system of processes.