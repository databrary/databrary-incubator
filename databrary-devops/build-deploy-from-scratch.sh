BRANCH="master"
rm -rf ~/src/databrary
mkdir ~/src && cd ~/src && git clone https://github.com/databrary/databrary
cd ~/src/databrary && git checkout $BRANCH
# trigger config file creation
./deploy stage
