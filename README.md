# Pantry


## Running

``` shell
docker-compose up
```

### Configuration

The following environment variables are read by the application to configure
its behavior

- APP_PORT (default=80): The port the web server will listen to incomming requests on
- DB_URL (default=postgres://postgres@localhost:5432/postgres): the connection url that app will use to connect to the database

## Development

starting the compiler in file watcher mode:

``` shell
stack test --fast --haddock-deps --file-watch
```

## Todo

* finish kubernetes deployment architecture
  * db in pod? (use StatefulSet)
  * OR hosted db?
* tag based search
* front end
