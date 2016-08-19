# To-do list

My notes while I'm writing a simple to-do list application in Haskell on Ubuntu.

**Warning:** Notes may be incomplete

## Milestones

* Milestone 1: Create, read, update, and delete tasks using Haskell
* Milestone 2: Add in `newtype`s
* Milestone 3: Split models and database functions into separate files
* Milestone 4: Add models
* Milestone 5: ???

## Milestone 1 

**Goal:** Create, read, update, and delete tasks using Haskell


### Install Haskell Stack

* Follow instructions on the [Haskell Stack](http://docs.haskellstack.org/en/stable/README/#how-to-install) website for Ubuntu 
* Run `stack setup` to install the compiler
* Add `~/.local/bin` to your environment `PATH` variable
  * I have mine in set in `~/.zshenv` since I use `zsh` instead of `bash`
    * `export PATH=$PATH:$HOME/.local/bin`


### Install PostgreSQL

* Install PostgreSQL server along with development dependencies
  * `sudo apt-get install postgresql postgresql-contrib libpq-dev`


### Set up PostgreSQL for use
* Run `psql` as the `postgres` user to manage the database server
  * `sudo -u postgres psql`
* Create the database in `psql`
  * `CREATE DATABASE todolist;`
* Create a new superuser with a password
  * `CREATE USER todoadmin SUPERUSER PASSWORD 'a';`
* Exit `psql`
  * `\q`


### Create our database table to store tasks
* Run `psql` with our new `todoadmin` user
  * `psql --host=localhost --username=todoadmin --dbname=todolist`
* Create our table
 
  ```
  CREATE TABLE tasks (
    id serial PRIMARY KEY,
    label text,
    completed boolean DEFAULT FALSE
  );
  ```
* Exit `psql`
  * `\q`
  
### Create our Haskell project using `stack`

* Create our `todolist` project using the `simple` template
  * `stack new todolist simple`
* Switch to do the `todolist` directory
  * `cd todolist`
* Make sure the project can build
  * `stack build`

### Set up Git
* Initialize Git
  * `git init`
* Add in `.gitignore`

  ```
  dist
  dist-*
  cabal-dev
  *.o
  *.hi
  *.chi
  *.chs.h
  *.dyn_o
  *.dyn_hi
  .hpc
  .hsenv
  .cabal-sandbox/
  cabal.sandbox.config
  *.prof
  *.aux
  *.hp
  *.eventlog
  .stack-work/
  cabal.project.local
  ```


### Pull in Haskell dependencies

* Edit `todolist.cabal`
* Add `postgresql-simple` and `text` as dependencies under `build-depends`
* Turn on compiler warnings by adding `ghc-options: -Wall`
* Run `stack build` to download and build all dependencies

* Use your editor of choice to write code


### Start writing code

* Use your editor of choice to start writing code
