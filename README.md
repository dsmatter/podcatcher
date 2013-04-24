# Podcatcher
A simple terminal-based podcast client written in Haskell

## Dependencies
- Make
- GHC
- Cabal packages:
    - download-curl
    - process
    - missingh
    - feed
    - xml

## Installation
    make

## Usage
The tool treats all subdirectories containing an "feed.url" file as a podcast subscription.
For example:

    $ tree
    ├── Podcast1
    │   └── feed.url
    ├── Podcast2
    │   ├── episode01.mp3
    │   ├── episode02.mp3
    │   └── feed.url
    └── podcatcher

Once everything is set up, just run the tool

    ./podcatcher