###############################################################################
# symfuzz Makefile                                                            #
#                                                                             #
# Copyright (c) 2014, Sang Kil Cha                                            #
# All rights reserved.                                                        #
# This software is free software; you can redistribute it and/or              #
# modify it under the terms of the GNU Library General Public                 #
# License version 2, with the special exception on linking                    #
# described in file LICENSE.                                                  #
#                                                                             #
# This software is distributed in the hope that it will be useful,            #
# but WITHOUT ANY WARRANTY; without even the implied warranty of              #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        #
###############################################################################

all: depcheck PIN src

stamp:
	mkdir -p stamp

stamp/pindownload:
	wget 'http://software.intel.com/sites/landingpage/pintool/downloads/pin-2.14-67254-gcc.4.4.7-linux.tar.gz' \
		&& touch stamp/pindownload

stamp/pinunpack:
	tar xfz pin-2.14-67254-gcc.4.4.7-linux.tar.gz \
		&& mv pin-2.14-67254-gcc.4.4.7-linux pin \
		&& touch stamp/pinunpack

stamp/pinrm:
	rm -f pin-2.14-67254-gcc.4.4.7-linux.tar.gz \
		&& touch stamp/pinrm

stamp/srcconf:
	cd src; ./configure \
		&& touch ../stamp/srcconf

PIN: stamp stamp/pindownload stamp/pinunpack stamp/pinrm

src:
	make -C src

depcheck: Makefile.dep
	@buildtools/depcheck.sh $<

clean:
	rm -rf stamp
	make -C src clean

.PHONY: all PIN src depcheck clean
