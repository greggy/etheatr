Web application "etheatr"
======================

This have been made as test project for hiring process.

It's an OTP application that emulate a servise for reserve
tickets in a cinema. The main data is Screen Id and Imdb Id.

 - Screen Id is the room identication;
 - Imdb Id is the identificator from IMDB.

Database
--------

The project uses mongodb as database. I use this [project](https://github.com/greggy/vagrant-ansible-mongodb)
that helps me to start up virtual box with fresh mongodb-server.

How To Compile, Build And Start The Project
-----------------------------------------

    $ make compile
    $ make release
    $ make console

How To Rebuild The Project
-------------------------

    $ make rebuild

How To Run Tests
---------------

    $ make tests

Web API
-------

URL | Method | Args | Description
--- | ---- | ----------- | ------------
/movie/lists | GET | {} | List of screens
/movie/add | PUT | screen_id, imdb_id, limit | Create the new screen
/movie/ScreenId | GET | {} | Get screen info
/movie/get_seat | PUT | screen_id, imdb_id | Get a seet in the screen