#!/usr/bin/env nix-shell
#! nix-shell -i bash
# FIXME: Add rsync to the env somehow
# Use
# https://discourse.nixos.org/t/best-way-to-augment-a-nix-shell-for-dev-utilities/157/6
# once we finally update nixpkgs again.
set -eux

#FIXME: This may work, but hasn't been actually tested yet. Fun times!

TODAY=$(date +%Y%m%d)

build () {
    ## Basic (?) cabal steps
    rm -fr dist
    cabal configure \
        --enable-tests \
        --enable-coverage \
        --disable-shared \
        --disable-optimization
    cabal build -j
    # This also calls hpc
    cabal test

    ## Use cabal to call haddock
    cabal haddock --hyperlink-source | grep ') in' \
        | tee haddock-coverage-report-${TODAY}.txt
}

report () {
    ## Set up gh-pages for rsyncing
    wd=$(mktemp -d)
    git worktree add ${wd} gh-pages
    trap "rm -rf ${wd}; git worktree prune" EXIT

    ## Rsync the haddocks and the hpc report
    rsync -ric --delete \
        dist/hpc/vanilla/html/databrary-1/ \
        ${wd}/coverage
    rsync -ric --delete dist/doc/html/databrary/ ${wd}/haddocks/

    ## Update and finish
    ( # New subshell for nested traps
        cd ${wd}
        trap "cd -" EXIT
        git commit --no-gpg-sign --all -m "Update ${TODAY}"
    )
}

build
report
