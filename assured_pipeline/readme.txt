1) Build module
make -f /usr/share/selinux/devel/Makefile pipeline.pp

2) Install module
sudo semodule -i pipeline.pp

3) create directory structure
mkdir assured_pipeline
cd assured_pipelinue
mkdir d1
mkdir d2
mkdir d3
mkdir d4

4) Run restorecon
sudo restorecon -Rv assured_pipeline/

5) Run
./master.sh

6) Check output
cat d4/out.txt

7) Check for SELinux denials
sudo audit2allow -al

8) Remove module
sudo  semodule -r pipeline
