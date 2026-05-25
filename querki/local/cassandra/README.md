# Running Cassandra Locally

We used to do development using ccm, but that turned
into a nightmare of Python maintenance. So we're switched
to just using plain old Docker.

To run a local Cassandra, open a terminal in this directory
and say
```shell
docker compose up -d
```

You can then use these commands to confirm that it's running:
```shell
docker compose ps
docker compose logs cassandra
```

To log into a local cqlsh, say:
```shell
docker exec -it cassandra-local cqlsh
```
