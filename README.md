# How to get this working?

## Compile

First of all, you need to clone the repository with the server sources and build it using stack.
Usually it is enough to just run `stack build`, all the libraries will be compiled automatically.
Eventually building can crash because of some external libraries that are not installed on your machine.
If you are using Linux, you can just install them yourself. For example, if you are using Ubuntu and
the pq library is missing, just install `libpq-dev` package using `# apt install libpq-dev`. Generally, for most cases if (libname) is
missing, the package lib(libname)-dev is what you need.

## Create database

After the application is compiled, it still can't work as it needs also a database. Here PostgreSQL
database is used, so you need to install it on you machine.
First of all, you need to create database.
The instruction here supposes that database works on the same machine with web-server;
if it is not true, a little changes should be made: see postgreSQL documentaion.
We will refer to the database name as databaseName. Also you will need the user
for the application to work with the database.
We will refer to it as to databaseOwner.
So, next commands are to be run using postgreSQL superuser:

```sql
    CREATE DATABASE databaseName;
    CREATE USER databaseOwner PASSWORD <ownerPassword>;
    ALTER DATABASE databaseName OWNER TO databaseOwner;
```
The application gets all the data from the configuration file, so you need to put it there.
See configuration file format below. In this section we will use file config.dhall.

After users are created, you need to create the entire database. It is rather easy since
the web-server supports migrations and has a separate command-line argument to apply all the
migrations to the given database. To run migrations, use `stack exec weather-exe -- ./config.conf -m`.

In the end, you should let PostgreSQL know, who can connect to the database and what authentication method
should be used. If the database is on the same machine with the server, the next line is to be added to
`pg_hba.conf`:

`local   databaseName    databaseOwner   password`

# Configuration file
The example of the configuration file is inside the `src` directory.

```
{ dhallDatabaseName = "weather_db"
, dhallDatabaseUser = "weather_owner"
, dhallDatabasePassword = "0000"

, dhallFillerCities =
    [ 499717 -- rzhev 
    , 543899 -- kostomuksha
    , 4171563 -- stPetersburg
    , 499622 -- moskvorechie-saburovo 
    ]

, dhallFillerSleepTimeSeconds = 600

, dhallServerTimeEpsSeconds = 1000
, dhallServerLatEps = 0.2
, dhallServerLonEps = 0.1
, dhallServerPort = 8081
}
```

Note that since Dhall is used for the configuration, you should
use `./src/config.dhall`, not `src/config.dhall` when starting the server.


# Command line options

The first command-line option is always path to the configuration file.
The second one is for the name of an environmental variable which contains OpenWeather API key.
The order of subsequent options can be arbitrary.

+ `-m` is used to perform the migrations;
+ `--test-config` is used to test server configuration. Data from config file will be get and then
process will terminate with success exitcode;
+ `-l` is used to define logging settings. Currently logger can only log messages with
given priority or higher. For example, to log Warning, Error and Fatal messages use;
`-l Warning`. The default is `-l Debug`, which logs all messages;
+ `--logpathfillers <path>` is used to define the path to the log file of fillers;
+ `--logpathserver <path>` is used to define the path to the log file of server.


