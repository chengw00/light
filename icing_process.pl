#!/usr/bin/perl

#       Configurable forecast options

$LOGIN = $ENV{'LOGNAME'};

$GMID = $ARGV[0];
$MEMBER = $ARGV[1];

$GMID = "GE7CHUAN" if(! $GMID);
$MEMBER = "GFS_WCTRL" if (! $MEMBER);
 
require "/home/$LOGIN/data/GMODJOBS/$GMID/flexinput.pl" if(-s "/home/$LOGIN/data/GMODJOBS/$GMID/flexinput.pl");

$CYC_INT = 6 if(! $CYC_INT); 
$FCST_LENGTH = $CYC_INT if(! $FCST_LENGTH);
$D4_LENGTH = 13 if (! $D4_LENGTH);

print " $LOGIN $GMID $MEMBER $CYC_INT $FCST_LENGTH $D4_LENGTH \n" ;

#print "  $FCST_LENGTH $D4_LENGTH  \n ";

system("cd /home/$LOGIN/data/GMODJOBS/$GMID/housekeeping/icing ; icing_process.csh $GMID $MEMBER $CYC_INT $FCST_LENGTH $D4_LENGTH ");

#system("cd /home/$LOGIN/data/GMODJOBS/$GMID/housekeeping/icing ; icing_process.csh $GMID $MEMBER $CYC_INT $FCST_LENGTH $D4_LENGTH 2015121712 ");
