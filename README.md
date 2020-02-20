# Pantry


## Running

``` shell
docker-compose up
```

### Configuration

The following environment variables are read by the application to configure
its behavior

- APP_PORT (default=80): The port the web server will listen to incomming requests on
- PANTRY_DB_SERVICE_SERVICE_HOST (default="localhost"): the host name of the database

## Todo

* finish kubernetes deployment architecture
  * statefulset
  * storage class for gce-pd
  * ~~persistent volume~~
  * ~~persistent volume claim~~
  * https://kubernetes.io/docs/tasks/configure-pod-container/configure-persistent-volume-storage/
* ~~tag based search~~
* front end
