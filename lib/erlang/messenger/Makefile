all: code

code: clean
	rake compile

clean:
	rake clean

package-debian: code
	mkdir -p project_name/usr/lib/erlang/lib/project_name/ebin/ && cp source_file_name.beam project_name/usr/lib/erlang/lib/project_name/ebin/source_file_name.beam
	mkdir -p project_name/DEBIAN/ && cp control project_name/DEBIAN/control
	dpkg -b project_name project_name.deb

install-debian: package-debian
	dpkg -i project_name.deb
