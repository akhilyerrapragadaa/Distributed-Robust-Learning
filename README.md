# Distributed-Robust-Learning

This project contains all-reduce implementation using scala and kompics. 

## Overview

The project has 1 part:

- A server library that is responsible for creating workers (servers)

The bootstrapping procedure for the servers, requires one server to be marked as a bootstrap server, which the other servers (bootstrap clients) check in with, before the system starts up. The bootstrap server also assigns predecessors and successors for each node including itself.

### Building

Start sbt with

```bash
sbt
```

In the sbt REPL build the project with

```bash
compile
```


Before running the project you need to create assembly files for the server:

```bash
server/assembly
```

### Running

#### Bootstrap Server Node
To run a bootstrap server node execute:

```
java -jar server/target/scala-2.13/server.jar -p 45678
```

This will start the bootstrap server on localhost:45678.

#### Normal Server Node
After you started a bootstrap server on `<bsip>:<bsport>`, again from the `server` directory execute:

```
java -jar server/target/scala-2.13/server.jar -p 45679 -s <bsip>:<bsport>
```
This will start the bootstrap server on localhost:45679, and ask it to connect to the bootstrap server at `<bsip>:<bsport>`.
Make sure you start every node on a different port if they are all running directly on the local machine.

By default you need 4 nodes (including the bootstrap server), before the system will actually generate assignments (predecessor and successor) and begins all-reduce implementation with a randomly generated vector in each worker. Note that the current implementation only supports worker length proportional to vector length.
The number can be changed in the configuration file (cf. [Kompics docs](http://kompics.github.io/current/tutorial/networking/basic/basic.html#cleanup-config-files-classmatchers-and-assembly) for background on Kompics configurations).
