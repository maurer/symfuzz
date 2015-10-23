###############################################################################
# SymFuzz Makefile                                                            #
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

OCAMLBUILD=ocamlbuild

OFUZZ_SUBDIRS=-Is libinput,analyzer \
	      -Xs buildtools,pinapi,instrumentor,unittest

TARGET_BINARY=symfuzz

all: depcheck
	make -C pinapi
	$(OCAMLBUILD) -no-links $(OFUZZ_SUBDIRS) \
		symfuzz.native libanalysis.native depreader.native
	make -C instrumentor
	ln -sf _build/analyzer/symfuzz.native $(TARGET_BINARY)
	ln -sf _build/analyzer/depreader.native depreader

unittest: unittest/Makefile
	@make -C unittest

clean: depcheck
	make -C pinapi clean
	$(OCAMLBUILD) -clean
	rm -f $(TARGET_BINARY) depreader
	make -C instrumentor clean

clean-log:
	@rm -rf *.log
	@rm -rf symfuzzlog-*

depcheck: ../Makefile.dep
	@../buildtools/depcheck.sh $<

.PHONY: all clean clean-log depcheck
