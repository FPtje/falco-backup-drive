# Falco's backup to external drive program

A program designed solely for personal use. I have an external drive that I use
for backups, and odd backup needs. This program automates those backups.

This program is intended to run on a small server that I have running inside my
home.

This program is also an exploration of software architecture principles in
Haskell. It uses the
[`effectful`](https://github.com/haskell-effectful/effectful) library to build
something that should have a "grow-friendly" architecture. Whether it actually
does is something I will find out, as I intend to extend this program with more
and more features, as I back up more of my digital world.

Initially, this program will cut many corners. Configuration is initially just a
json file, logging is just a `putStrLn`, and secrets are just reading
environment variables. I don't want to think about these things right at the
start, but I _do_ want it to be easy to extend these things later.

## Why not use an existing backup tool?

I would, were it not for the odd backup needs. One such need is that I want to
back up micro SD cards that I actively use in my daily life. I want to be able
to initiate a backup by simply connecting the SD card to the server. The server
should then automatically mount the SD card, perform the backup, and then
unmount it again so I can take it out.

Where possible, I prefer using external tools (e.g. `rsync`), as I don't think
reinventing wheels is an ideal expenditure of my time.

## Support

Since this is an application for personal use, I chose not to support this
repository. This is also why issues are disabled on the repository. If you would
like to use this program anyway, please let me know by creating a pull request
removing this very section. Please write down the motivation for using this
program in the PR's description. If this program provides enough value for other
people, I will reconsider this support status.
