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

#### Terraform Config
You will need to set two environmental variables on your machine to run the terraform: 
- GOOGLE_APPLICATION_CREDENTIALS
- GOOGLE_CLOUD_KEYFILE_JSON

These both should point to your JSON file with your google cloud creds. 

## Todo

* finish kubernetes deployment architecture
  * statefulset
  * storage class for gce-pd
  * ~~persistent volume~~
  * ~~persistent volume claim~~
  * https://kubernetes.io/docs/tasks/configure-pod-container/configure-persistent-volume-storage/
* ~~tag based search~~
* front end
