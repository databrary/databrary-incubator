# Databrary

* http://databrary.org/
* http://databrary.github.io/databrary/

## Installation

There is a [build script](build.sh) that automates most of the installation steps for ubuntu 16.04.2 (obviously you'll have to look a little closer if you're on a different distro or release). The rest of this section basically just explains that script. 

---

fdk-aac (a dependency of ffmpeg) is in multiverse repos.
```bash
# uncomment all multiverse
sed -i "/^# deb.*multiverse/ s/^# //" /etc/apt/sources.list
echo 'deb http://us.archive.ubuntu.com/ubuntu/ xenial multiverse' >> /etc/apt/sources.list
echo 'deb http://us.archive.ubuntu.com/ubuntu/ xenial-updates multiverse' >> /etc/apt/sources.list
```

various dependencies
```bash
apt-get update && apt-get install -y libgmp-dev git yasm npm libcrack2-dev gcc g++ autoconf automake zlib1g-dev \
		   libmp3lame-dev libx264-dev libfdk-aac-dev libavformat-dev libswscale-dev libavcodec-dev libavutil-dev curl \
		   pkg-config vim ssmtp postgresql-client libgmp3-dev libz-dev
```

node has a nonstandard executable name (`nodejs`) on ubuntu/debian.
```bash
ln -s /usr/bin/nodejs /usr/bin/node
```

compile ffmpeg from source because you need extra codecs like `x264`, `fdk-aac`, and `mp3lame`. this takes a while.

```bash
cd /usr/src
git clone git://source.ffmpeg.org/ffmpeg.git
cd ffmpeg
git remote update origin
ffmpeg=2.8.x && ffmpeg=`git describe --abbrev=0 origin/release/${ffmpeg%.x}` && ffmpeg=${ffmpeg#u} && git checkout $ffmpeg
./configure --enable-gpl --enable-version3 --enable-nonfree --enable-libx264 --enable-libfdk-aac --enable-libmp3lame 
make && make install
```

get docker for postgres and solr containers

```bash
wget -qO- https://get.docker.com/ | sh
```

add your user to docker group (so that you don't need to `sudo docker ...`)

```bash
usermod -aG docker $USER
# update groups without login/logout
su - $USER
```

compile ghc and cabal from source. be sure to take a closer look at the cabal-install version number (but ghc is fixed @ 7.10.3). this takes a while.
```bash
mkdir ~/src && cd ~/src

wget https://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3b-x86_64-deb8-linux.tar.xz && tar -xvf ghc-7.10.3b-x86_64-deb8-linux.tar.xz
cd ghc-7.10.3 && ./configure && sudo make install

cd ~/src

# cabal
wget https://www.haskell.org/cabal/release/cabal-install-1.24.0.2/cabal-install-1.24.0.2.tar.gz && tar -xvf cabal-install-1.24.0.2.tar.gz
cd cabal-install-1.24.0.2 && EXTRA_CONFIGURE_OPTS= sudo ./bootstrap.sh --global
export PATH=$PATH:/usr/local/bin
cabal update
```

get databrary master

```bash
cd ~/src && git clone git://github.com/databrary/databrary
```

create postgres and solr docker containers

```bash
cd ~/src/databrary/Docker
# persist postgres data backing
docker volume create --name databrary_postgres_store
docker build -t databrary_postgres postgres/
docker run -d -v databrary_postgres_store:/var/lib/postgresql/data -p 5432:5432 --rm --name databrary_postgres databrary_postgres
# create databrary user with password databrary123
# postgres password is mysecretpassword (set in docker build)
./wait-for-postgres.sh localhost "docker exec databrary_postgres /usr/local/src/databrary/init-user-db.sh"

# persist solr data backing
docker volume create --name databrary_solr_store
docker build -t databrary_solr solr/
docker run -d -v databrary_solr_store:/opt/solr -p 8983:8983 --rm --name databrary_solr databrary_solr
# create databrary core
./wait-for-solr.sh localhost "docker exec databrary_solr solr create -c databrary_core -d /databrary_conf"
```

check to make sure the containers are running by running `docker ps`. finally compile and install databrary in a cabal sandbox (don't try to do it outside of sandbox

```bash
cd ~/src/databrary
cabal update
cabal sandbox init
# parser dependency
cabal install happy --force-reinstalls
cabal install --only-dependencies --force-reinstalls
```

if you want turn on the development flag (which does various things - search this repo for DEVEL) then you can run

```bash
cabal configure -f devel
```

otherwise just

```bash
cabal configure
```

the config file `databrary.conf` has to exist before you compile. you can `cp example.conf databrary.conf` and change things. take note in particular of the `store` section and basically anywhere else you see a path.


```bash
cp example.conf databrary.conf
yes | cabal install 
```

this should place the executable `databrary-$git-release` in `~/src/databrary/.cabal-sandbox/bin`. 
to run databrary execute this from the directory where your `databrary.conf` is

```bash
databrary_datadir=. databrary_sysconfdir=. ~/src/databrary/.cabal-sandbox/bin/databrary-$git-release
```


## License

Copyright (C) 2013-2016 New York University

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
