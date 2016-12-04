#!/bin/sh


cd ..
[ -d out/lnx/units.fpc ] || mkdir -p out/lnx/units.fpc


# BBS
cp src/elebbs/baseinc/bbs.inc src/elebbs/baseinc/compiler.inc 
fpc -B -Sd -CX -O1 -gl -FEout/lnx -FUout/lnx/units.fpc src/elebbs/elebbs.pas @etc/fpc_unit.lst @etc/fpc_incl.lst


# Utils
cp src/elebbs/baseinc/util.inc src/elebbs/baseinc/compiler.inc 
# elemail.pas elemon.pas elenews.pas eleserv.pas 
for source in elefile.pas elemgr.pas elenode.pas eleuser.pas elexer/elexer.pas elconfig.pas
do
	fpc -B -Sd -CX -O1 -gl -FEout/lnx -FUout/lnx/units.fpc src/elebbs/$source @etc/fpc_unit.lst @etc/fpc_incl.lst
done



# There is a conflict betweeen -Ox and -Op
# http://bugs.freepascal.org/view.php?id=10924

#   FAIL: -O1 -Op 386
#   OK:   -O1
#   OK:   -Op 386

#Procedure Dummy;
#Begin
#End;
#Begin
# While True Do Dummy;
#End.

