#!/usr/bin/perl

#chdir  /home1/ncar_fdda/data/GMODJOBS/GE7CHUAN/GFS_WCTRL/

if (! -e "d3.map.pad.icingINST.png") {
 system("cp /home1/ncar_fdda/data/GMODJOBS/GE7CHUAN/housekeeping/icing/d3*map*pad*png .");
}

foreach $f (`ls -1 -rt d3*trans*icingACC.png d3*trans*icingINST.png | tail -4`) {
 chomp($f);
 print "processing:  $f\ n";
 
 $fout1=$f;
 $fout1 =~ s/png/map.png/;
 $fout2=$f;
 $fout2 =~ s/png/satmap.png/;
 if (! -e "$fout1") {
  system ("convert -trim $f atmp.gif;gifmerge -255,255,255 atmp.gif > atmp.tran.gif");
  system ("convert -gravity center d3.map.pad.icingINST.png atmp.tran.gif -composite $fout1");
  system ("convert -gravity center d3.satmap.pad.icingINST.png atmp.tran.gif -composite $fout2");
 }
}

foreach $f (`ls -1 -rt d3*trans*lineRH.png d3*trans*lineSPD.png  d3*trans*lineT.png | tail -6`) {
 chomp($f);
 print "processing:  $f\ n";

 $fout1=$f;
 $fout1 =~ s/png/map.png/;
 $fout2=$f;
 $fout2 =~ s/png/satmap.png/;
 if (! -e "$fout1") {
  system ("convert -trim $f atmp.gif; gifmerge -255,255,255 atmp.gif > atmp.tran.gif");
  system ("convert -gravity center d3.map.pad.lineSPD.png atmp.tran.gif -composite $fout1");
  system ("convert -gravity center d3.satmap.pad.lineSPD.png atmp.tran.gif -composite $fout2");
 }
}

exit
