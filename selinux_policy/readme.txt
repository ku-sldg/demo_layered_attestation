1) Build module
make -f /usr/share/selinux/devel/Makefile demo_pipeline.pp

	This step needs the SELinux policy development packages.

	The fc file assumes this is running somewhere in your home directory.
	"HOME_DIR" is special in an SELinux fc file. If you run the demo
	elsewhere, then replace "HOME_DIR" with the path where the demo is.

2) Install module
sudo semodule -i demo_pipeline.pp

3) Update labeling
sudo restorecon -RFv $REPO/LayeredAttestation/src/demo_layered_attestation/

4) If you want more details about the denials, try:
sudo ausearch -m avc -ts today &> today-01.txt
