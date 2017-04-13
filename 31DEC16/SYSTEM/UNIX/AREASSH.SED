s/^!\(  *\)"\(.*\)"$/#\1\2/
s/# *\(AREAS.DAT        \)\(.*\) */# Usage: . AREAS.SH\2/
s/^!/#/
t quit
s/^\([^#][^ ]*\)\( *\)\([^ ]*\)\(.*\)/\1=$AIPS_VERSION\/\3\
export \1/
s/\([A-Z0-9]\)\(\.\)\([A-Za-z0-9]\)/\1\/\3/g
s:/LIBR\([^D]\):/\$ARCH/LIBR\1:g
s:/LIBRDBG:/\$ARCH/LIBRDBG:g
s:/LOAD:/\$ARCH/LOAD:g
s:/PREP:/\$ARCH/PREP:g
s:/MEMORY:/\$ARCH/MEMORY:g
s:/ERRORS:/\$ARCH/ERRORS/\$HOST:g
s/\([0-9]\)\(\/\)\([0-9]\)/\1.\3/
t quit
: quit
b
