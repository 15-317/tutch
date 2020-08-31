# tutch 
# Copyright (C) Andreas Abel, 2001

# ---------------------------------------------------------------
# Please edit the following lines
# ---------------------------------------------------------------

# What is SML/NJ called?
sml = sml
destdir = /afs/andrew/course/15/317/tutch-2017
# sml = sml
# destdir = /afs/andrew/scs/cs/15-399/
afswindrive = Z:

# ---------------------------------------------------------------
# Do not edit the following lines
# ---------------------------------------------------------------

default : tutch

all : tutch status

tutch :
	$(sml) < tutch.sml

status:
	$(sml) < status.sml

install: tutch status
	cp -r bin/.heap $(destdir)/bin
	mkdir -p $(destdir)/bin/unix $(destdir)/bin/win32 $(destdir)/bin/cygwin
	sed -e "s#%TUTCHDIR#"$(destdir)"#g" \
	    -e "s#%SML#$(sml)#g" bin/.tutch.sh \
	> $(destdir)/bin/unix/tutch ;
	chmod a+x $(destdir)/bin/unix/tutch ;
#	> $(destdir)/bin/tutch ;
#	chmod a+x $(destdir)/bin/tutch
	sed -e "s#%TUTCHDIR#"$(destdir)"#g" \
	    -e "s#%SML#$(sml)#g" bin/.tutch.sh \
            -e "s#/afs#$(afswindrive)#" \
	| sed -e 1n -e "s#/#\\\\#g" \
	> $(destdir)/bin/cygwin/tutch ;
	chmod a+x $(destdir)/bin/cygwin/tutch
#	> $(destdir)/bin/tutch-cygwin ;
#	chmod a+x $(destdir)/bin/tutch
	sed -e "s#%TUTCHDIR#"$(destdir)"#g" \
	    -e "s#%SML#$(sml)#g" bin/.tutch.bat \
            -e "s#/afs#$(afswindrive)#" \
	| sed -e 1n -e "s#/#\\\\#g" \
	> $(destdir)/bin/win32/tutch.bat
	chmod a+x $(destdir)/bin/win32/tutch.bat
#	> $(destdir)/bin/tutch.bat
#	chmod a+x $(destdir)/bin/tutch.bat
	sed -e "s#%TUTCHDIR#"$(destdir)"#g" \
	    -e "s#%SML#$(sml)#g" bin/.status.sh \
	> $(destdir)/bin/unix/status
	chmod a+x $(destdir)/bin/unix/status
#	> $(destdir)/bin/status ;
#	chmod a+x $(destdir)/bin/status ;
	sed -e "s#%TUTCHDIR#"$(destdir)"#g" \
	    -e "s#%SML#$(sml)#g" bin/.status.sh \
            -e "s#/afs#$(afswindrive)#" \
	| sed -e 1n -e "s#/#\\\\#g" \
	> $(destdir)/bin/cygwin/status ;
	chmod a+x $(destdir)/bin/cygwin/status
#	> $(destdir)/bin/status-cygwin ;
#	chmod a+x $(destdir)/bin/status
	sed -e "s#%TUTCHDIR#"$(destdir)"#g" \
	    -e "s#%SML#$(sml)#g" bin/.status.bat \
            -e "s#/afs#$(afswindrive)#" \
	| sed -e 1n -e "s#/#\\\\#g" \
	> $(destdir)/bin/win32/status.bat
	chmod a+x $(destdir)/bin/win32/status.bat
#	> $(destdir)/bin/status.bat
#	chmod a+x $(destdir)/bin/status.bat
	sed -e "s#%TUTCHDIR#"$(destdir)"#g" \
	    -e "s#%SML#$(sml)#g" bin/.submit.sh \
	> $(destdir)/bin/unix/submit ;
	chmod a+x $(destdir)/bin/unix/submit ;
#	> $(destdir)/bin/submit ;
#	chmod a+x $(destdir)/bin/submit ;
	sed -e "s#%TUTCHDIR#"$(destdir)"#g" \
	    -e "s#%SML#$(sml)#g" bin/.submit.sh \
            -e "s#/afs#$(afswindrive)#" \
	| sed -e 1n -e "s#/#\\\\#g" \
	> $(destdir)/bin/cygwin/submit ;
	chmod a+x $(destdir)/bin/cygwin/submit
#	> $(destdir)/bin/submit-cygwin ;
#	chmod a+x $(destdir)/bin/submit-cygwin
	sed -e "s#%TUTCHDIR#"$(destdir)"#g" \
	    -e "s#%SML#$(sml)#g" bin/.submit.bat \
            -e "s#/afs#$(afswindrive)#" \
	| sed -e 1n -e "s#/#\\\\#g" \
	> $(destdir)/bin/win32/submit.bat
	chmod a+x $(destdir)/bin/win32/submit.bat
#	> $(destdir)/bin/submit.bat
#	chmod a+x $(destdir)/bin/submit.bat ;

