#
# Makefile for ABRHLibs/src/syntrax/Parser
#
# ABRHLibs -- a personal library of Haskell modules
# Copyright (C) 2007, Andrew Rock
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#

### variables #################################################################

# The subdirectories requiring recursive make
SUB_DIRS =

# All the ebnf files
EBNFFILES = $(wildcard *.ebnf)

# All the ebnf base names
BASES = $(basename $(EBNFFILES))

# the syntrax command
SYNTRAX = syntrax

# All the .eps file names
EPSFILES = $(addsuffix .eps, $(BASES))

# the command to convert eps to pdf
EPSTOPDF = epstopdf

# All the .pdf file names
PDFFILES = $(addsuffix .pdf, $(BASES))

### rules #####################################################################

# convert .eps to .pdf
%.pdf : %.eps
	$(EPSTOPDF) $<

### phony targets #############################################################

# Everything
.PHONY : all
all : identify $(EPSFILES) $(PDFFILES)
	
# Clean up intermediate products
.PHONY : clean
clean : subdirs_clean identify
	rm -r -f $(EPSFILES) 

# Clean up ALL products
.PHONY : CLEAN
CLEAN : clean subdirs_CLEAN identify
	rm -r -f $(PDFFILES) 

# Print the directory we are working in
.PHONY : identify
identify :
	@ echo "######## MAKING" `pwd`
	
# Process the subdirectories
.PHONY : subdirs_all
subdirs_all : 
	for d in $(SUB_DIRS);            \
	do                              \
	   $(MAKE) --directory=$$d all; \
	done

.PHONY : subdirs_clean
subdirs_clean : 
	for d in $(SUB_DIRS);            \
	do                              \
	   $(MAKE) --directory=$$d clean; \
	done

.PHONY : subdirs_CLEAN
subdirs_CLEAN : 
	for d in $(SUB_DIRS);            \
	do                              \
	   $(MAKE) --directory=$$d CLEAN; \
	done
	
### apply syntrax #############################################################

$(EPSFILES) : $(EBNFFILES)
	syntrax --fill *.ebnf
