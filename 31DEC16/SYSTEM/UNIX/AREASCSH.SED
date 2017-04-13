s/!/#/
s/\(# *" *\)\(AREAS.DAT              \)\(.*\)/\1Usage: source AREAS.CSH\3/
t quit
s/^\([^#][^ ]*\)\( *\)\([^ ]*\)\(.*\)/setenv \1\2$AIPS_VERSION\/\3\4/
s/\([A-Z0-9]\)\(\.\)\([A-Za-z0-9]\)/\1\/\3/g
s:/LIBR$:/\$ARCH/LIBR:g
s:/LIBRDBG$:/\$ARCH/LIBRDBG:g
s:/LOAD:/\$ARCH/LOAD:g
s:/PREP:/\$ARCH/PREP:g
s:/MEMORY:/\$ARCH/MEMORY:g
s:/ERRORS:/\$ARCH/ERRORS/\$HOST:g
s/\([0-9]\)\(\/\)\([0-9]\)/\1.\3/
t quit
: quit
b
