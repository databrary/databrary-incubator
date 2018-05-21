#!/usr/bin/env nix-shell
#! nix-shell -i bash
# FIXME: Add rsync to the env somehow
# Use
# https://discourse.nixos.org/t/best-way-to-augment-a-nix-shell-for-dev-utilities/157/6
# once we finally update nixpkgs again.
set -eu

# TODO: Include commit date as well as report creation date.
# TODO: Clean up hpc summary
TODAY=$(date +%Y-%m-%d)

DISTDIR=dist-reports
CABAL="cabal --builddir=$PWD/$DISTDIR"

haddock_report_file=haddock-coverage-report-${TODAY}.txt

make_haddock_summary () {
    perl -ne '
        BEGIN { $n = 0; $d = 0; }
        m%(\d+) */ *(\d+)% or next;
        $n += $1; $d += $2;
        END { printf "%.2f%% %d %d\n",  ($n / $d) * 100,  $n, $d; }' $@
}

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
        | tee $haddock_report_file
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
        # Get frontend docs, too
        git submodule update --remote
        git commit --no-gpg-sign --no-verify --all -m "Update ${TODAY}"
        git push
    )
}

summary () {
    echo
    echo '##################################################'
    echo '#####                SUMMARY                 #####'
    echo '##################################################'
    echo 'Haddock coverage numbers:'
    echo -n '    '
    make_haddock_summary $haddock_report_file
    echo 'HPC coverage numbers:'
    echo -n '    '
    tail -2 $DISTDIR/hpc/vanilla/html/databrary-1/hpc_index.html \
        | head -1 \
        | grep -o -P '^.{40}'
}

build
report
summary
