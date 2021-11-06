-- { dhallDatabaseName = "weatherdb"
{ dhallDatabaseName = "weather_migrations"
, dhallDatabaseUser = "weather_owner"
, dhallDatabasePassword = "0000"

, dhallFillerCities =
    [ 499717 -- rzhev 
    , 543899 -- kostomuksha
    , 4171563 -- stPetersburg
    , 499622 -- moskvorechie-saburovo
    ] -- List Integer

, dhallFillerSleepTimeSeconds = 600 

, dhallServerTimeEpsSeconds = 1000
, dhallServerLatEps = 0.2
, dhallServerLonEps = 0.1
, dhallServerPort = 8081
}


