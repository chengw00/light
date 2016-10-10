#!/bin/csh

set AGE_LIMIT = 172800   # 2 days in sec, files will be processed
                         # when its age exceeds this time
set TIME_NOW  = ` date +%s `

set GMID = GE7CHUAN

#set PATH = $1
#
#if ( $1 == "" ) then
# echo "PATH EMPTY: EXIT"
# exit
#endif

# =============
set MEMBER = $1

if ( $1 == "" ) then
 set MEMBER = GFS_WCTRL
endif
# ==================

echo "actually clean $GMID "

set LOGIN = ` whoami `

set PATH = /home1/${LOGIN}/data/cycles/$GMID/prod/$MEMBER/icing/working

set filelist = ` /bin/ls -1 $PATH `

foreach ff ( $filelist )
 set TIME_FILE = ` stat -c %Y ${PATH}/$ff `
 @  age = $TIME_NOW - $TIME_FILE
  if ( $age > $AGE_LIMIT ) then
   echo "removing $ff "
   rm -r -f $PATH/$ff
  else
   echo "not time to remove" $ff
  endif
end

# ======= clean archive ========
set AGE_LIMIT = 1036800    # 12 days in sec, files will be processed
                           # when its age exceeds this time
                           #
                           #
set PATH = /home1/${LOGIN}/data/cycles/$GMID/prod/$MEMBER/icing/running

set DIR = ( netcdf ascii ascii_2d )

foreach dd ( $DIR )
 set filelist = ` /bin/ls -1 $PATH/$dd `

 foreach ff ( $filelist )
  set TIME_FILE = ` stat -c %Y ${PATH}/$dd/$ff `
  @  age = $TIME_NOW - $TIME_FILE
   if ( $age > $AGE_LIMIT ) then
    echo "removing $ff "
    rm -f $PATH/$dd/$ff
   else
    echo "not time to remove" $ff
   endif
 end
end


set AGE_LIMIT = 172800   # 2 days in sec, files will be processed
                           # when its age exceeds this time
                           #
                           #
set PATH = /home1/${LOGIN}/data/cycles/$GMID/prod/$MEMBER/icing/running

set DIR = ( images )

foreach dd ( $DIR )
 set filelist = ` /bin/ls -1 $PATH/$dd `

 foreach ff ( $filelist )
  set TIME_FILE = ` stat -c %Y ${PATH}/$dd/$ff `
  @  age = $TIME_NOW - $TIME_FILE
   if ( $age > $AGE_LIMIT ) then
    echo "removing $ff "
    rm -f $PATH/$dd/$ff
   else
    echo "not time to remove" $ff
   endif
 end
end
