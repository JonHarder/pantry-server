# Pantry


## Running

``` shell
docker-compose up
```

### Configuration

The following environment variables are read by the application to configure
its behavior

- APP_PORT (default=80): The port the web server will listen to incomming requests on

## Development

starting the compiler in file watcher mode:

``` shell
stack test --fast --haddock-deps --file-watch
```
