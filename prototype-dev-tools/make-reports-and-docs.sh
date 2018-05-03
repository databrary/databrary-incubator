#!/usr/bin/env nix-shell
#! nix-shell -i bash
# FIXME: Add rsync to the env somehow
set -eu

#FIXME: This may work, but hasn't been actually tested yet. Fun times!

>&2 echo "Set up gh-pages first!"
exit 1

TODAY=$(date +%Y%m%d)

cabal configure --enable-tests --enable-coverage --disable-shared --disable-optimization
cabal build -j
cabal test
cabal haddock | grep ') in' \
    | tee haddock_coverage_report-${TODAY}.txt
hpc report dist/hpc/vanilla/tix/databrary-1/databrary-1.tix \
    --hpcdir=./dist/hpc/vanilla/mix/databrary-1 \
    --hpcdir=./dist/hpc/vanilla/mix/discovered \
    --exclude=Paths_databrary \
    | tee hpc-report-${TODAY}.txt
wd=$(mktemp -d)
git worktree add ${wd} gh-pages
trap "rm -rf ${wd}; git worktree prune" EXIT

rsync -ric --delete \
    dist/hpc/vanilla/html/discovered/databrary-1-4IJypVjWiZDEnHoKzlHmLx/ \
    ${wd}/coverage
rsync -ric --delete --exclude=.git dist/doc/html/databrary/ ${wd}/haddocks/

cd ${wd}
git commit --all -m "Update ${TODAY}"
