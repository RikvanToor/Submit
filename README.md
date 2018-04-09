# Submit

This is a submission system built in Haskell, using Servant, Persistent, and PostreSQL.

# Install/build guide

1. Clone this repository if you haven't already and navigate to the folder in which you've cloned it in your terminal.
1. The current release of _Esqueleto_ does not support the newest version of _Persistent_ yet. We have to download and build _Esqueleto_ manually.
 
    Run `git clone https://github.com/bitemyapp/esqueleto.git esqueleto`
1. Install PostgreSQL on your system. For arch-based systems this is as simple as

    `sudo pacman -S postgresql`
1. Run `sudo -i -u postgres` to change to the postgres user that was created during the install of PostgreSQL.
1. As the postgres user, run `initdb --locale en_US.UTF-8 -E UTF8 -D '/var/lib/postgres/data'` to initialize a database.
1. Enter `exit` to return to your normal user account.
1. Run `sudo systemctl start postgres.service` to start Postgres.
1. Go back to the postgres user: `sudo -i -u postgres`
1. Run `createuser --interactive`. Call the user 'submit' and make it a superuser. Do not give it a password.
1. Run `createdb submit -U submit` to create a database for the application
1. Go back to your own user by entering `exit`.
1. `stack setup`
1. `./run` builds and runs the application. This builds up the database tables as well.
1. Now, to fill the database with some sample data, press `Ctrl+C` to stop the server once it's running and enter `psql -f database/data.sql -d submit -U submit`.
1. Restart the application by entering `./run` again.
1. Go to `http://localhost:8081` in your browser and check out the website. The sample accounts are:

    username | password | role
    --- | --- | ---
    wouter | test | Teacher
    alejandro | test | Teacher
    4239776 | test | Student