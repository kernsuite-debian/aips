#-----------------------------------------------------------------------
#;  Copyright (C) 1995-1998, 2001-2004, 2013
#;  Associated Universities, Inc. Washington DC, USA.
#;
#;  This program is free software; you can redistribute it and/or
#;  modify it under the terms of the GNU General Public License as
#;  published by the Free Software Foundation; either version 2 of
#;  the License, or (at your option) any later version.
#;
#;  This program is distributed in the hope that it will be useful,
#;  but WITHOUT ANY WARRANTY; without even the implied warranty of
#;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#;  GNU General Public License for more details.
#;
#;  You should have received a copy of the GNU General Public
#;  License along with this program; if not, write to the Free
#;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
#;  MA 02139, USA.
#;
#;  Correspondence concerning AIPS should be addressed as follows:
#;         Internet email: aipsmail@nrao.edu.
#;         Postal address: AIPS Project Office
#;                         National Radio Astronomy Observatory
#;                         520 Edgemont Road
#;                         Charlottesville, VA 22903-2475 USA
#-----------------------------------------------------------------------
# Usage: . TVDEVS.SH
#-----------------------------------------------------------------------
# Define TV devices and if necessary, start TV servers.
#
# The HOSTS.LIST file specifies which hosts are TV servers, displays.
# This script reads that file and does one of the following:
#
#   *** if the TVOPT environment variable is defined but is not valid,
#       it will be reset to "NONE" before the script exit.
#
#   *** if the TVOPT environment variable is defined and is valid, the
#       appropriate values of TVHOST, TVDISP, TVALT, TVDEV, TVDEVxx,
#       TKDEV, TKDEVxx, TTDEV, and TTDEVxx  will be returned.
#
# A more detailed description of the variables to be set follows:
#
#    TVHOST  Host on which the TV server is to be run, or the host with
#            the hardware tv physically connected to it.  Generated
#            automatically from HOSTS.LIST (hosts with a "+" in col. 1)
#
#    TVDISP  Host on which the TV server is to be displayed (X11), or
#            the hardware device name.
#
#    TVALT   translates to TVALTyz where the "y" digit identifies a
#            possible alternate load area LOADy (now rarely used so this
#            is almost always 0), and the "z" digit identifies the
#            windowing server type (anymore always set to 3 for X11).
#            The original meanings for x and y are:
#             * TVALT00: no windowing system is currently running
#               TVALT01: Sunview/Suntools Screen Server (SSS, obsolete)
#               TVALT02: XView Screen Server (XVSS; also obsolete)
#             * TVALT03: X11 AIPS TV Server (XAS)
#               TVALT04: Hardwired TV that uses default $LOAD binaries
#               TVALT24: As above, use alternate $LOAD2 binaries
#               TVALT44: Likewise except $LOAD4.
#            Information on hard-wired TV's such as the IIS or IVAS or
#            DeAnza must be added here in TVHOSTS and TVALT (note that
#            the latter is now deprecated and unused; this may render
#            such devices unusable).  Asterisks indicate the only
#            values which are likely to work.
#
#    TVDEV   translates to TVDEVxx where xx is the (ehex) TV number.
#
#    TVDEVxx translates to the name of the TV device which has extended
#            hexadecimal unit number xx.  The TV device number
#            corresponds to the order in which the server appears in the
#            TVHOSTS environment variable, i.e. in the HOSTS.LIST file.
#
#    TKDEV   translates to TKDEVxx where xx is the (ehex) TV number.
#
#    TKDEVxx translates to the name of the graphics device which has
#            extended hex number xx.  The TK device number assigned is
#            the same as the TV device number.
#
#    TTDEV   translates to TTDEVxx where xx is the (ehex) TV number.
#
#    TKDEVxx translates to the name of the message window.  The TT
#            device number assigned is the same as the TV device number.
#
#    TVLOK   translates to TVLOKxx where xx is the (ehex) TV number.
#
#    TVLOKxx translates to the name of the lockfile used when using Unix
#            sockets for the TV Lock daemon (TVSERV.EXE, TVSRVx).
#
#    TVN     The Unix socket number to use if relevant.
#
#    AIPSTVS Number of AIPS TVs (TVHOSTS plus TVSERVS)
#
# NOTE: If $AIPS_ROOT/STARTING_TV_NUMBER exists and contains a non-zero
# number, that is taken as the starting TV number for Unix sockets.
# This is for compatibility with earlier multiple TV schemes.
#-----------------------------------------------------------------------
#                                       See 15OCT97 or older versions
#                                       for what this was used for.
TVALTS=""
# -------------- do not change anything below this line ----------------
#
ni2() {
  if [ "`echo -n YES`" = "YES" ] ; then
    echo -n "TVDEVS.SH: $*"
  else
    echo "TVDEVS.SH: $*\c"
  fi
}
ei2() {
  echo "TVDEVS.SH: $*"
}
spc() {
  echo " "
}

#                                       The following will get the
#                                       regular hosts, tv servers and x
#                                       terminals from HOSTS.LIST.
TVHOST2=`grep '^[+]  .*' $AIPS_ROOT/HOSTS.LIST | grep $SITE | \
         awk '{print $2}' | tr '[A-Z]' '[a-z]'`
TVSERVS=`grep '^[-]  .*' $AIPS_ROOT/HOSTS.LIST | grep $SITE | \
         awk '{print $2}' | tr '[A-Z]' '[a-z]'`
XTERMS=`grep '^[=]  .*' $AIPS_ROOT/HOSTS.LIST  | grep $SITE | \
         awk '{printf "%s:%s\n",$2,$5}' | tr '[A-Z]' '[a-z]'`
#                                       No blank at beginning of string
if [ "$TVHOSTS" != "" ] ; then
   TVHOSTS="$TVHOSTS $TVHOST2"
else
   TVHOSTS="$TVHOST2"
fi
AIPSTVS=`echo $TVHOSTS $TVSERVS | wc -w`
#                                       Export everything we can now.
export TVOPT TVHOST TVDISP TVALT TVDEV TKDEV TTDEV AIPSTVS TVN
export TVDEV TTDEV TKDEV TVLOK
#
#                                       Set default values.
TVALT=TVALT00
TVDEV=TVDEV00
TKDEV=TKDEV00
TTDEV=TTDEV00
TVLOK=TVLOK00
#                                       Only do rest if TV is defined
if [ "$TVOPT" != "" ] ; then
#                                       If $TVOPT is "HELP", be kind
#                                       This actually never happens...
  if [ "$TVOPT" = "HELP" ] ; then
    TVOPT="NONE"
    ei2 "You can choose any of the following TV servers or devices."
    spc
    for tv in $TVHOSTS $XTERMS ; do
      case $tv in
        *:*) ei2 "  $tv        (display machine : aips machine)"
             ;;
        *+*) ei2 "  $tv        (hostname + hardware TV device name)"
             ;;
        *)   ei2 "  $tv        (workstation)"
             ;;
      esac
    done
    spc
    ni2 "Enter your choice: "
    read TVOPT
  fi
#                                       Resolve hardwire TV aliases.
  for tvhost in $TVHOSTS ; do
    case $tvhost in
      *+${TVOPT})
        TVOPT=$tvhost
        break;;
    esac
  done
#                                       Check for brainless displays.
  for xterm in $XTERMS ; do
    case $xterm in
      ${TVOPT}:*)
        TVOPT=$xterm
        break;;
    esac
  done
#                                       Parse TVOPT into TVHOST & TVDISP
  case $TVOPT in
    *:)
      TVHOST=`$AIPS_ROOT/SETNAME`
      TVDISP=`echo $TVOPT | sed -e 's/:.*//'`
      TVOPT=${TVDISP}:${TVHOST}
      ;;
#                                       Should never happen, but...
    :*)
      TVHOST=`echo $TVOPT | sed -e 's/://'`
      TVDISP=`$AIPS_ROOT/SETNAME`
      TVOPT=${TVDISP}:${TVHOST}
      ;;
    *:*)
      TVHOST=`echo $TVOPT | sed -e 's/.*://'`
      TVDISP=`echo $TVOPT | sed -e 's/:.*//'`
      TVOPT=${TVDISP}:${TVHOST}
      ;;
    *+*)
      TVHOST=`echo $TVOPT | sed -e 's/+.*//'`
      TVDISP=`echo $TVOPT | sed -e 's/.*+//'`
      TVOPT=${TVHOST}+${TVDISP}
      ei2 "Your hardware (non-workstation) TV is $TVOPT"
      ;;
    *)
      TVHOST="$TVOPT"
      TVDISP="$TVOPT"
      TVOPT=${TVDISP}:${TVHOST}
      ;;
  esac
#                                       Ensure we have a valid TVHOST.
#                                       but skip it if TVHOST=NONE and
#                                       hope there's no NONE.nrao.edu!
  if [ "$TVHOST" = NONE ] ; then
    stat=0
  else
    echo "$TVHOSTS" "$TVSERVS" | grep "$TVHOST" >/dev/null
    stat=$?
  fi
  if [ $stat -ne 0 ] ; then
    if [ "$USEUNIXSOCK" = YES ] ; then
      ei2 "Local TV assigned on $TVHOST for display $DISPLAY"
    elif [ "$TVHOST" = "$TVDISP" ] ; then
      ei2 "Remote TV assigned for TVHOST $TVHOST  ***"
    else
      ei2 "Unrecognized TVHOST; $TVHOST"
    fi
  fi
#                                       Determine TVALT.
  if [ "$TVHOST" != "NONE" ] ; then
    case $TVOPT in
      *:*)
#                                       Colon implies X11 so use XAS.
           TVALT=TVALT03;;
      *+*)
#                                       Hardwired TV.
#                                       Resolve load number from TVALTS
        found=false
        if [ "$TVALTS" != "" ] ; then
          for tvalt in $TVALTS ; do
            digit=`echo $tvalt | awk -F: '{print $2}'`
            if [ "$TVDISP" = "`echo $tvalt | awk -F: '{print $1}'`" ]
            then
              found=true
              break
            fi
          done
          if [ "$found" = true ] ; then
            TVALT="TVALT${digit}4"
          else
            ei2 "There is no entry in TVALTS (TVDEVS.SH) for $TVOPT"
            ei2 "TV related items may not work..."
            TVALT=TVALT04
          fi
        else
#                                       TVALTS not set, assume default
#                                       LOAD.
          TVALT=TVALT04
        fi
        ;;
    esac
#                                       Decode the TV number.
    tvnum=1
#                                       Need to look at full TVOPT for
#                                       hardwired TV's
    tvh=$TVHOST
    case $TVOPT in
      *+*) tvh=$TVOPT;;
    esac
    for tvhost in $TVHOSTS $TVSERVS ; do
      [ "$tvh" = "$tvhost" ] && break
      tvnum=`expr $tvnum + 1`
    done
#                                       Allocate a TV device number.
    tvhex=`EHEX $tvnum`
    case $tvhex in
      [0-9A-Z]) tvhex="0$tvhex";;
    esac
    TVDEV=TVDEV$tvhex
    [ "$DEBUG" = YES ] && \
     ei2 "Debug: TV number initially set to $tvnum ($tvhex)"
#                                       Set device or service name.  If
#                                       a real device, it must be of the
#                                       form "/dev/foo" and "foo" must
#                                       be what appears after the "+" in
#                                       the TV name.
    case $TVALT in
      TVALT?4)
        eval "$TVDEV=/dev/$TVDISP"
        eval "export $TVDEV";;
#                                       TV Servers.  Unix or Inet?
      *)
        if [ "$USEUNIXSOCK" = YES ] ; then
          [ "$AIPSTMP" = "" ] && AIPSTMP=/tmp
#                                       Multiple Unix sockets allowed.
#                                       Figure out which one to use.
          TVN=0
          TVD=""
          if [ "$UNIXTVN" != "0" ] ; then
             OTH=`ls $AIPSTMP/XAS.${UNIXTVN}.* \
                     $AIPSTMP/LOK.${UNIXTVN}.* \
                     $AIPSTMP/TKS.${UNIXTVN}.* \
                     $AIPSTMP/MSS.${UNIXTVN}.* \
                  2>/dev/null | awk -F\. '{print $NF}' | head -n 1`
             if [ "$OTH" != "" ] ; then
                if [ "$ARCH" = "MACINT" ] ; then
                   ei2 "I am assuming the display is what you want"
                   TVD=$UNIXTVN
                elif [ "$OTH" != "$TVDISP" ] ; then
                   ei2 "Sorry, UNIX TV $UNIXTVN in use on display $OTH"
                   ei2 "Will assume you want first available slot"
                else
                   TVD=$UNIXTVN
                fi
             else
                TVD=$UNIXTVN
             fi
             [ "$DEBUG" = YES ] && ei2 "accepting usock number $TVD"
          elif [ "$NEWUNIXSOCK" != YES ] ; then
#                                       See if there's a socket for this
#                                       display already created.  If so,
#                                       use the first available one.
            [ "$DEBUG" = YES ] && ei2 "scanning $AIPSTMP/---.n.$TVDISP"
            TVD=`ls  $AIPSTMP/XAS.?.$TVDISP \
                     $AIPSTMP/LOK.?.$TVDISP \
                     $AIPSTMP/TKS.?.$TVDISP \
                     $AIPSTMP/MSS.?.$TVDISP 2>/dev/null | \
           sed -e "s,$AIPSTMP/[A-Z][A-Z][A-Z]\.\([A-Z0-9]\)\..*,\1,g" | \
                 sort | head -n 1`
            [ "$DEBUG" = YES ] && ei2 "done.  TVD=$TVD"
          else
            TVD=""
          fi
          if [ "$TVD" = "" ] ; then
#                                       Find first free slot for socket.
            [ "$DEBUG" = YES ] &&
             ei2 "no existing socket for $TVDISP, scan..."
            i=1
            if [ -s $AIPS_ROOT/STARTING_TV_NUMBER ] ; then
	       j=`head -n 1 $AIPS_ROOT/STARTING_TV_NUMBER`
               if [ "$j" != "" ] ; then
                  if [ $j -lt 36 -a $j -gt 0 ] ; then
                      i=$j
                  fi
               fi
            fi
            while [ $i -lt 36 ] ; do
              ei=`EHEX $i`
#                                       Not safe for collisions
              OTH=`ls $AIPSTMP/XAS.${ei}.* \
                      $AIPSTMP/LOK.${ei}.* \
                      $AIPSTMP/TKS.${ei}.* \
                      $AIPSTMP/MSS.${ei}.* 2>/dev/null | head -n 1`
              if [ "$OTH" = "" ] ; then
                TVN=$ei
                [ "$DEBUG" = YES ] && ei2 "first free slot is $TVN"
                break
              fi
              i=`expr $i + 1`
            done
#           if [ $TVN -eq 0 ] ; then
            if [ `REHEX $TVN` -eq 0 ] ; then
              ei2 "No free socket slots left!"
              exit 1
            fi
          else
            [ "$DEBUG" = YES ] && ei2 "found existing $TVD socket"
            TVN=$TVD
          fi
	  tvn=`REHEX $TVN`
          tvn=`expr $tvn + $AIPSTVS`
          tvhex=`EHEX $tvn`
          case $tvhex in
            [0-9A-Z]) tvhex="0$tvhex";;
          esac
          TVDEV=TVDEV$tvhex
          TKDEV=TKDEV$tvhex
          TTDEV=TTDEV$tvhex
          TVLOK=TVLOK$tvhex
          eval "$TVDEV=$AIPSTMP/XAS.$TVN.$TVDISP"
          eval "$TTDEV=$AIPSTMP/MSS.$TVN.$TVDISP"
          eval "$TKDEV=$AIPSTMP/TKS.$TVN.$TVDISP"
          eval "$TVLOK=$AIPSTMP/LOK.$TVN.$TVDISP"
          eval "export $TVDEV $TTDEV $TKDEV $TVLOK"
          if [ "$DEBUG" = YES ] ; then
            ni2  "Unix Sockets, overriding TVDEV to be "
            echo "$TVDEV ($AIPSTVS + $TVN = $tvn [$tvhex])"
            ni2 "                                and $TVDEV to be "
            eval "echo \$$TVDEV"
            ni2 "                                and $TKDEV to be "
            eval "echo \$$TKDEV"
            ni2 "                                and $TTDEV to be "
            eval "echo \$$TTDEV"
            ni2 "                                and $TVLOK to be "
            eval "echo \$$TVLOK"
          fi
        else
#                                       Allow for buffered I/O
#                                       This never worked well... needs
#                                       an overhaul?
          socket="sssin"
          [ "$AIPS_TV_BUFFERED" = "YES" ] && socket="sssinb"
#
          eval "TVDEV$tvhex=${socket}:$TVHOST"
          eval "export $TVDEV"
        fi;;
    esac
#                                       Graphics/Message assignment for
#                                       TEKSERVER and MSGSERVER.
    if [ "$USEUNIXSOCK" != YES ] ; then
      TKDEV=TKDEV$tvhex
      eval "$TKDEV=$TVHOST"
      eval "export $TKDEV"
      TTDEV=TTDEV$tvhex
      eval "$TTDEV=$TVHOST"
      eval "export $TTDEV"
#                                       How come TVLOK doesn't need to
#                                       be set for INET sockets?
    fi
  fi
fi
