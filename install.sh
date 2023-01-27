# This install script is deperacted for Arch, sadv
# is now available on the AUR

echo \> building...
mkdir out
sh build

echo
echo \> installing to \"/usr/local/bin\" as sadv
sudo cp sadv /usr/local/bin/sadv
echo \> installed!
