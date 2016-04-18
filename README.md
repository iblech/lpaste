# lpaste

The codebase for http://lpaste.net/

## Build

    $ stack build

## Database setup

    $ sudo su postgres --command 'createuser lpaste -P'
    $ sudo su postgres --command 'createdb lpaste -O lpaste'
    $ cat sql/schema.sql | psql -U lpaste -h 127.0.0.1 -d lpaste

## Configuration & Running

    $ cp lpaste.conf.sample lpaste.conf

Edit lpaste.conf.

    $ stack exec lpaste lpaste.conf
