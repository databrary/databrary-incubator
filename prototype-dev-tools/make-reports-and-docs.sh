#!/usr/bin/env nix-shell
#! nix-shell -i bash
# FIXME: Add rsync to the env somehow
# Use
# https://discourse.nixos.org/t/best-way-to-augment-a-nix-shell-for-dev-utilities/157/6
# once we finally update nixpkgs again.
set -eux

TODAY=$(date +%Y-%m-%d)

DISTDIR=dist-reports
CABAL="cabal --builddir=$PWD/$DISTDIR"

build () {
    ## Basic (?) cabal steps
    $CABAL configure \
        --enable-tests \
        --enable-coverage \
        --disable-shared \
        --disable-optimization
    $CABAL build -j
    # This also calls hpc
    $CABAL test

    ## Use cabal to call haddock
    $CABAL haddock --hyperlink-source | grep ') in' \
        | tee haddock-coverage-report-${TODAY}.txt
}

report () {
    ## Set up gh-pages for rsyncing
    wd=$(mktemp -d)
    git worktree add ${wd} gh-pages
    trap "rm -rf ${wd}; git worktree prune" RETURN
    trap "#### WORKTREE PRESERVED AT ${wd}" ERR

    ## Rsync the haddocks and the hpc report
    rsync -ric --delete \
        $DISTDIR/hpc/vanilla/html/databrary-1/ \
        ${wd}/coverage
    rsync -ric --delete $DISTDIR/doc/html/databrary/ ${wd}/haddocks/

    ## Update and finish
    ( # New subshell for nested traps
        cd ${wd}
        trap "cd -" EXIT
        git commit --no-gpg-sign --no-verify --all -m "Update ${TODAY}"
    )
}

build
report
