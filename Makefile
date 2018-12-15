PPATH:=$(shell pwd)
TARGET1=BANNHL
TARGET2=BANNHC
TARGET3=paper

DOCPATH=Docs
CC=idris
CFLAGS=-O2 -o
DC=latexmk
DFLAGS=-pdf -f -shell-escape
SRC=Src

.PHONY: docs

all: docs

docs:
	@cd $(PPATH)/$(SRC)/; $(DC) $(DFLAGS) $(TARGET3).tex
	@cd $(PPATH)/$(SRC)/; $(DC) -c $(TARGET3).tex
	#remove minted report folder
	@rm -rf $(PPATH)/$(SRC)/_minted-report
	#remove any unwanted backups
	@rm -f $(PPATH)/$(SRC)/$(TARGET3).tex.bak
	@mkdir -p $(PPATH)/$(DOCPATH)
	@mv $(PPATH)/$(SRC)/$(TARGET3).pdf $(PPATH)/$(DOCPATH)
	#clean up after latexmk
	@rm $(PPATH)/$(SRC)/*.aux
	@rm $(PPATH)/$(SRC)/*.fls
	@rm $(PPATH)/$(SRC)/*.fdb_latexmk
  
clean:
	@rm -f $(PPATH)/bin/$(TARGET1)
	@rm -f $(PPATH)/bin/$(TARGET2)
