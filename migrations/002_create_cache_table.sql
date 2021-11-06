CREATE TABLE weather.cache (
    coord_longitude double precision NOT NULL,
    coord_latitude double precision NOT NULL,

    dt integer NOT NULL,

    base text,
    timezone integer,
    city_id integer NOT NULL,
    city_name text,

    weather_id integer,
    weather_main text,
    weather_description text,
    weather_icon text, 

    main_temp double precision,
    main_feels_like double precision,
    main_temp_min double precision,
    main_temp_max double precision,
    main_pressure double precision,
    main_humidity double precision,

    wind_speed double precision,
    wind_deg double precision,
    wind_gust double precision,

    clouds_all integer,

    rain1h double precision,
    rain3h double precision,

    snow1h double precision,
    snow3h double precision,

    sys_type integer,
    sys_id integer,
    sys_message double precision,
    sys_country text,
    sys_sunrise integer,
    sys_sunset integer,
    PRIMARY KEY (dt, city_id)
);
