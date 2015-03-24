# pollock

[EN](#En) | [RU](#Ru) | [UA](#Ua)

# En

`pollock` is a complete web application written in Haskell, intended as education project and reference for beginners. You can see development changes by looking at the git commit history of this repository, each stage represent small step in development. I start from simplistic "Hello, World!" app to a fully-functional social voting application with PostgreSQL database integration.

## Starting out

Begin by entering the following commands into your terminal window.  You'll need to have Haskell installed.

```sh
$ git clone git://github.com/
$ cd pollock
$ git checkout stage-1
$ cabal install
$ ./dist/build/pollock/pollock
```

Now, open a browser and go to [http://localhost:8000/](http://localhost:8000/).

You should see "Hello, Pollock!" displayed.  If you look at [src/Main.hs in stage-1](https://github.com/ryantrinkle/memoise/blob/lesson-1/src/Main.hs), you can see that this is achieved with just a few simple lines of Haskell.

To shut down the pollock server, press Ctrl-C in your terminal window.  Once you've taken a look around in Stage 1, you can move on to Stage 2 by running

```sh
$ git checkout stage-2
$ cabal install
$ ./dist/build/pollock/pollock
```

## Stages

Although looking at the checked out code may be helpful, a more informative way to look at this project is by viewing the commit history.  Each stage is a single git commit, designed to add a single piece of functionality, as you might do if you were creating a website from scratch.

You can take a look at the changes made in each lession here:

* [Stage 1]()
* [Stage 2]()
* [Stage 3]()
* [Stage 4]()
* [Stage 5]()
* [Stage 6]()
* [Stage 7]()

Note: You need PostgreSQL installed for all following stages.

* [Stage 8]()
* [Stage 9]()
* [Stage 10]()
* [Stage 11]()
* [Stage 12]()
* [Stage 13]()
* [Stage 14]()

# Ru


# Ua