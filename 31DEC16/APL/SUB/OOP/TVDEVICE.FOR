C    TV device Class Module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "TV" class library
C# Map-util TV-Util Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998-2000, 2002, 2006, 2008-2009, 2011
C;  Copyright (C) 2013-2015
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   A TV object is an interface to the TV display, either a real TV
C   display device or the XAS TV server.  Only one TV object may be
C   active (open) at a time.
C   Usage notes:
C      1) A TV object may be associated with an image or uvdata object
C         before opening by setting the value of 'TVOBJECT' before
C         opening.  When the TV object is then opened, that object may
C         be displayed on the TV.
C   Class public members:
C      TVOBJECT  C*32  Name of displayed uvdata or image object.
C      TVPARENT  C*32  Name of parent object of TVOBJECT
C   Class public members ONLY after class is open
C      TVCHANS   I(16) 16 grey channels selected
C      GRCHANS   I(8)  8 graphics overlay channels selected
C      WINLOAD   I     Graphics channel to use to display any WINDOW
C                      in the object automatically on load.
C   Class  private data:
C      MYNAME    C*32  Name of open TV-device
C      TVOBJ     C*32  Name of associated object
C      TVOBJC    C*8   Type of associated object
C      TVPAR     C*32  Name of associated object
C      TVPARC    C*8   Type of associated object
C      TVCHNS    I(16) Selected TV channels
C      GRCHNS    I(8)  Selected graphics channels
C      TVSTAT    I(16) Grey plane status: -1 unknown, 0 zero, 1 written
C      GRSTAT    I(8)  Graphic plane status: -1 unknown, 0 zero, 1
C                      written, 2 menu
C      WINLOD    I     Specified window graphics plane
C
C   Public functions:  Object management
C     TVDCRE (name, ierr)
C        Creates a TV-device object with name "name".
C     TVDDES (name, ierr)
C        Destroys the TV-device object with name "name";
C     TVDZAP (name, ierr)
C        Destroys the TV-device object with name "name";
C        (Same as TVDDES)
C     TVDOPN (name, status, ierr)
C        Opens a TV-device object
C     TVDCLO (name, ierr)
C        Closes a TV-device object
C     TVDGET (name, keywrd, type, dim, value, valuec, ierr)
C        Return keyword value.
C     TVDPUT (name, keywrd, type, dim, value, valuec, ierr)
C        Store keyword value.
C     TVDOKA (name, tvstat, grstat, ierr)
C        Return error if name not active, statuses otherwise
C     TVDRST (name, tvstat, grstat, ierr)
C        Reset TV status - be careful!
C
C   Public functions:  TV display
C
C     TVDBLK (name, type, chans, ierr)
C        interactive blink/enhance displays
C     TVDBOX (name, chan, maxbox, nbox, blc, trc, ierr)
C        create/revise up to maxbox boxes
C     TVDCAT (name, opcode, chan, ixy, catblk, ierr)
C        read/write TV catalog
C     TVDCHR (name, x, y, angle, type, chan, string, ierr)
C        write characters on the TV
C     TVDCRN (name, tlc, ierr)
C        return TV coordinates of corners that are visible
C     TVDFUN (name, opcode, chans, ierr)
C        zoom, scroll, color, enhance functions
C     TVDGRC (name, opcode, chan, rgb, ierr)
C        read or write graphics plane colors
C     TVDINT (name, corn, first, tvxy, tvbutt, ierr)
C        Interact with the TV by standard means.  The calling program
C        must issue the user instructions and carry out any requested
C        operations.
C     TVDLAB (name, type, chan, ltype, doacro, ierr)
C        label image of type type
C     TVDLIN (name, type, chan, npoint, x, y, ierr)
C        connect npoint vertices with lines
C     TVDLN3 (name, type, chan, color, npoint, x, y, ierr)
C        connect npoint vertices with 3-color lines
C     TVDLOD (name, disk, cno, chan, pixrng, blc, trc, pinc, funtyp,
C           tvcorn, ierr)
C        loads image from disk, cno to TV chan
C     TVDMEN (name, type, ncol, nrows, grchs, topsep, sidsep, ishelp,
C           choics, timlim, leave, ntitle, title, choice, tvbutt, ierr)
C        paint menu, select item
C     TVDOFM (name, oper, chan, ncont, ofmfil, ierr)
C        do various OFM read/write and modify operations
C     TVDOPR (name, opcode, chan, ierr)
C        init, clear, on, off specified graphics or grey;
C        hold/resume updating display
C     TVDPOS (name, domenu, timlim, tvxy, tvbutt, ierr)
C        get tv position within a time limit
C     TVDROM (name, type, chans, ierr)
C        roam display on loaded images
C     TVDSTR (name, type, chan, nstars, stpos, stparm, ierr)
C        plot "star" symbols on the TV
C     TVDTVW (name, opcode, win, ierr)
C        read or write the TV display window
C     TVDVAL (name, chan, ierr)
C        do interactive display of image value under cursor
C     TVDWED (name, type, chan, blc, length, width, tvcat, ierr)
C        draw a step wedge (or zeros) on TV w header tvcat
C     TVDWIN (name, chan, blc, trc, ierr)
C        find 1 window to the TV image
C     TVDXPR (name, type, ncol, nrows, grchs, topsep, choics, leave,
C        tty, n, choicx, shortx, alphax, optokx, choice, ierr)
C        Displays a menu on a graphics channel, highlights the expert
C        codes and reads the terminal to determine the desired operation
C     TVDZOM (name, opcode, zoom, ierr)
C        read or write the TV zoom parameters
C
C   Private functions:
C      TVBGET (name, keywrd, type, dim, value, valuec, ierr)
C         Fetches private member.
C      TVBPUT (name, keywrd, type, dim, value, valuec, ierr)
C         Stores private member.
C-----------------------------------------------------------------------
LOCAL INCLUDE 'TVDEV.INC'
C                                       Include for TV class.
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PTVC.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   TVBSIZ
      PARAMETER (TVBSIZ=(MAXIMG))
      INTEGER   TVCHNS(16), GRCHNS(8), TVSTAT(16), GRSTAT(8), WINLOD,
     *   TVSCRB(MAXIMG), CATSAV(256)
C                                       note TVSCRB must be larger
C                                       of MAXIMG and 3*TVMOFM
      REAL      TVOFM(3*TVMOFM)
      LOGICAL   ACTIVE
      CHARACTER MYNAME*32, TVOBJ*32, TVOBJC*8, TVPAR*32, TVPARC*8
      COMMON /TVDCCM/ CATSAV, TVSCRB, TVOFM, ACTIVE, TVCHNS,
     *   GRCHNS, TVSTAT, GRSTAT, WINLOD
      COMMON /TVDMCC/ MYNAME, TVOBJ, TVOBJC, TVPAR, TVPARC
C                                                          End TVDEV.INC
LOCAL END
      SUBROUTINE TVDCRE (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Creates a TVDEVICE object with name "name"
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'TVDEV.INC'
      INTEGER  IDIM(7), DUMMY(1)
      CHARACTER BNAME*32
      DATA BNAME /'    '/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Create AIPS object
      CALL OBCREA (NAME, 'TVDEVICE', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Initialize - ass. obj.
      IDIM(1) = LEN (BNAME)
      IDIM(2) = 1
      IDIM(3) = 0
      CALL TVDPUT (NAME, 'TVOBJECT', OOACAR, IDIM, DUMMY, BNAME, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TVDPUT (NAME, 'TVPARENT', OOACAR, IDIM, DUMMY, BNAME, IERR)
C                                       Error
 990  IF (IERR.NE.0) THEN
         MSGTXT = 'TVDCRE: ERROR CREATING ' // NAME
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
      END
      SUBROUTINE TVDDES (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the TVDEVICE object with name "name";
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C-----------------------------------------------------------------------
      IERR = 0
C                                       Close
      CALL TVDCLO (NAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Delete object
      CALL OBFREE (NAME, IERR)
C
 999  RETURN
      END
      SUBROUTINE TVDZAP (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the TVDEVICE object with name "name"; same as TVDDES.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      CALL TVDDES (NAME,IERR)
C
 999  RETURN
      END
      SUBROUTINE TVDOPN (NAME, STATUS, IERR)
C-----------------------------------------------------------------------
C   Public
C   Opens a TVDEVICE file.  Marks TVDEVICE common as active.
C   Inputs:
C      NAME   C*?   The name of the object.
C      STATUS C*4   Not used.
C   Output:
C      IERR  I     Error return code, 0=OK, 5=data invalid
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), STATUS*4
      INTEGER   IERR
C
      INCLUDE 'TVDEV.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INTEGER   DIM(7), TYPE, OBJNUM, CLASNO, DUMMY(1)
C-----------------------------------------------------------------------
      IERR = 0
C                                       TVDEVICE common must be inactive
      IF (ACTIVE) THEN
         IERR = 1
         MSGTXT = 'TVDOPN: ATTEMPT TO ACTIVATE SECOND TV-DEVICE OBJECT'
         GO TO 990
         END IF
      ACTIVE = .TRUE.
C                                       Save name
      MYNAME = NAME
C                                       Open TV-device
      CALL TVDGET (NAME, 'TVOBJECT', TYPE, DIM, DUMMY, TVOBJ, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL TVDGET (NAME, 'TVPARENT', TYPE, DIM, DUMMY, TVPAR, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       open TV-device excl
      CALL TVOPEN (SBUFF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TVDOPEN: UNABLE TO OPEN TV DEVICE ITSELF'
         GO TO 990
         END IF
C                                       Init parameters
C                                       Get associated object class.
      IF (TVOBJ.NE.' ') THEN
         CALL OBNAME (TVOBJ, OBJNUM, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL OBCLAS (OBJNUM, CLASNO, TVOBJC, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TVOBJC = ' '
         END IF
      IF (TVPAR.NE.' ') THEN
         CALL OBNAME (TVPAR, OBJNUM, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL OBCLAS (OBJNUM, CLASNO, TVPARC, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TVPARC = ' '
         END IF
      CALL FILL (16, 0, TVCHNS)
      CALL FILL (8, 0, GRCHNS)
      CALL FILL (16, -1, TVSTAT)
      CALL FILL (8, -1, GRSTAT)
      WINLOD = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  MSGTXT = 'TVDOPN: ERROR OPENING ' // NAME
      CALL MSGWRT (8)
      ACTIVE = .FALSE.
      MYNAME = ' '
C
 999  RETURN
      END
      SUBROUTINE TVDCLO (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Closes TVDEVICE completing output to TV-device if necessary.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'TVDEV.INC'
      INCLUDE 'INCS:CLASSIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       No action if inactive
      IF (.NOT.ACTIVE) GO TO 999
C                                       Make sure right one
      IF (NAME.NE.MYNAME) THEN
         IERR = 2
         MSGTXT = 'ATTEMPT TO ACCESS CLOSED TV OBJECT'
         GO TO 990
         END IF
C                                       Close print.
      CALL TVCLOS (SBUFF, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Mark as inactive
      ACTIVE = .FALSE.
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  MSGTXT = 'TVDCLO: ERROR CLOSING ' // NAME
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE TVDGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Returns the dimensionality and value(s) associated with a given
C   keyword.
C   Inputs:
C      NAME     C*?   The name of the object.
C      KEYWRD   C*?   The name of the keyword in form 'MEM1.MEM2...'
C   Outputs:
C      TYPE     I     Data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM      I(*)  Dimensionality of value, an axis dimension of zero
C                     means that that dimension and higher are
C                     undefined.
C      VALUE    ?(*)  The value associated with keyword.
C      VALUEC   C*?   Associated value (character)
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   OBJNUM
C-----------------------------------------------------------------------
      IERR = 0
C                                       Is this a base class?
      CALL TVBGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       IERR = 1 means not recognized.
C                                       Lookup NAME
      IF (IERR.EQ.1) THEN
         CALL OBNAME (NAME, OBJNUM, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OBGET (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
      END
      SUBROUTINE TVDPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Stores the value (array) associated with a given keyword.
C   Inputs:
C      NAME     C*?   The name of the object.
C      KEYWRD   C*?   The name of the keyword in form 'MEM1.MEM2...'
C      TYPE     I     Data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM      I(*)  Dimensionality of value, an axis dimension of zero
C                     means that that dimension and higher are
C                     undefined.
C      VALUE    ?(*)  The value associated with keyword.
C      VALUEC   C*?   Associated value (character)
C   Outputs:
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   OBJNUM
C-----------------------------------------------------------------------
      IERR = 0
C                                       Is this a base class?
      CALL TVBPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       IERR = 1 means not recognized.
C                                       Lookup NAME
      IF (IERR.EQ.1) THEN
         CALL OBNAME (NAME, OBJNUM, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OBPUT (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
      END
      SUBROUTINE TVDOKA (NAME, TVST, GRST, IERR)
C-----------------------------------------------------------------------
C   Public
C   Returns okay if NAME is active
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      TVST  I(16) TV status code: -1 ?, 0 zero, 1 written
C      GRST  I(8)  TV status code: -1 ?, 0 zero, 1 written, 2 menu ok
C      IERR  I     Error return code, 0=OK, 1=nothing active
C                     2 something else active
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   TVST(16), GRST(8), IERR
C
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 1
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 2
      IF (NAME.NE.MYNAME) GO TO 999
      CALL COPY (16, TVSTAT, TVST)
      CALL COPY (8, GRSTAT, GRST)
      IERR = 0
C
 999  RETURN
      END
      SUBROUTINE TVDRST (NAME, TVST, GRST, IERR)
C-----------------------------------------------------------------------
C   Public
C   Sets tv and graphics channel statuses: -1 unknown, 0, zero, 1
C   written, 2 menu ok.  Be careful - call TVDOKA, chnage the 1 or
C   things you affected and then call TVDRST.
C   Inputs:
C      NAME  C*?    The name of the object.
C      TVST  I(16)  TV channel status
C      GRST  I(8)   Graphics channel status
C   Output:
C      IERR  I     Error return code, 0=OK, 1=nothing active
C                     2 something else active, 3 bad GRCH
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   TVST(16), GRST(8), IERR
C
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 1
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 2
      IF (NAME.NE.MYNAME) GO TO 999
      CALL COPY (16, TVST, TVSTAT)
      CALL COPY (8, GRST, GRSTAT)
      IERR = 0
C
 999  RETURN
      END
C
C   Private functions:
C
      SUBROUTINE TVBGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Private
C   If KEYWRD refers to a recognized member base class then fetch the
C   value.
C   Inputs:
C      NAME     C*?   The name of the object.
C      KEYWRD   C*?   Keyword in form 'mem1.mem2...'
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of value, an axis dimension of zero
C                    means that that dimension and higher are
C                    undefined.
C      VALUE   ?     associated value (non character)
C      VALUEC  C*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find., 2=Failed
C-----------------------------------------------------------------------
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC*(*)
C
      INTEGER   POINT, NMEMS, IMEM, LOOP
      PARAMETER (NMEMS = 5)
      CHARACTER MEMBER*16, MEMS(NMEMS)*16
      INCLUDE 'TVDEV.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA MEMS /'TVOBJECT', 'TVPARENT', 'TVCHANS', 'GRCHANS',
     *   'WINLOAD'/
C-----------------------------------------------------------------------
      IERR = 1
C                                       Look for base class.member name
C                                       in KEYWRD.
      POINT = INDEX (KEYWRD, '.')
      IF (POINT.LE.0) THEN
         MEMBER = KEYWRD
      ELSE
         MEMBER = KEYWRD(1:POINT-1)
         END IF
C                                       Search list of recognized base
C                                       classes.
      IMEM = -1
      DO 10 LOOP = 1,NMEMS
         IF (MEMBER.EQ.MEMS(LOOP)) IMEM = LOOP
 10      CONTINUE
C                                       If no base class and member not
C                                       recognized then return.
      IF ((IMEM.LE.0) .AND. (POINT.LE.0)) GO TO 999
      IERR = 0
C                                       Find it?, if not, complain and
C                                       die.
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'TVBGET: UNKNOWN MEMBER: ' // MEMBER // ' IN ' // NAME
         CALL MSGWRT (7)
C                                       Get from common
      ELSE
         DIM(2) = 1
         IERR = 0
C                                       TVOBJECT
         IF (IMEM.EQ.1) THEN
            VALUEC = TVOBJ
            TYPE = OOACAR
            DIM(1) = LEN (TVOBJ)
C                                       TVOBJECT
         ELSE IF (IMEM.EQ.2) THEN
            VALUEC = TVPAR
            TYPE = OOACAR
            DIM(1) = LEN (TVPAR)
C                                       TVCHANS
         ELSE IF (IMEM.EQ.3) THEN
            CALL COPY (16, TVCHNS, VALUE)
            TYPE = OOAINT
            DIM(1) = 16
C                                       GRCHANS
         ELSE IF (IMEM.EQ.4) THEN
            CALL COPY (8, GRCHNS, VALUE)
            TYPE = OOAINT
            DIM(1) = 8
C                                       WINLOAD
         ELSE IF (IMEM.EQ.5) THEN
            VALUE(1) = WINLOD
            TYPE = OOAINT
            DIM(1) = 1
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE TVBPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Private
C   If KEYWRD refers to a recognized member base class then store the
C   value.
C   Inputs:
C      NAME    C*?   The name of the object.
C      KEYWRD  C*?   Keyword in form 'mem1.mem2...'
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of value, an axis dimension of zero
C                    means that that dimension and higher are
C                    undefined.
C      VALUE   ?     associated value (non character)
C      VALUEC  C*?   associated value (character)
C   Outputs:
C      IERR    I     Error code, 0=OK.  1=> didn't find.
C-----------------------------------------------------------------------
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC*(*)
C
      INTEGER   POINT, NMEMS, IMEM, LOOP, OBJNUM, CLASNO
      PARAMETER (NMEMS = 5)
      CHARACTER MEMBER*16, MEMS(NMEMS)*16
      INCLUDE 'TVDEV.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA MEMS /'TVOBJECT', 'TVPARENT', 'TVCHANS', 'GRCHANS',
     *   'WINLOAD'/
C-----------------------------------------------------------------------
      IERR = 1
C                                       Save member name
      POINT = INDEX (KEYWRD, '.')
      IF (POINT.LE.0) THEN
         MEMBER = KEYWRD
      ELSE
         MEMBER = KEYWRD(1:POINT-1)
         END IF
C                                       Search list of recognized base
C                                       classes.
      IMEM = -1
      DO 10 LOOP = 1,NMEMS
         IF (MEMBER.EQ.MEMS(LOOP)) IMEM = LOOP
 10      CONTINUE
C                                       If no base class and member not
C                                       recognized then return.
      IF ((IMEM.LE.0) .AND. (POINT.LE.0)) GO TO 999
      IERR = 0
C                                       Find it?, if not, complain and
C                                       die.
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'TVBPUT: UNKNOWN MEMBER: ' // MEMBER // ' IN ' // NAME
C                                       Save in common
C                                       TVOBJECT
      ELSE IF (IMEM.EQ.1) THEN
         TVOBJ = VALUEC
         IERR = 0
         IF (TVOBJ.NE.' ') THEN
            MSGTXT = 'TVBPUT: UNKNOWN CLASS FOR OBJECT ' // TVOBJ
            CALL OBNAME (TVOBJ, OBJNUM, IERR)
            IF (IERR.NE.0) GO TO 995
            CALL OBCLAS (OBJNUM, CLASNO, TVOBJC, IERR)
         ELSE
            TVOBJC = ' '
            END IF
C                                       TVOBJECT
      ELSE IF (IMEM.EQ.2) THEN
         TVPAR = VALUEC
         IERR = 0
         IF (TVPAR.NE.' ') THEN
            MSGTXT = 'TVBPUT: UNKNOWN CLASS FOR OBJECT ' // TVPAR
            CALL OBNAME (TVPAR, OBJNUM, IERR)
            IF (IERR.NE.0) GO TO 995
            CALL OBCLAS (OBJNUM, CLASNO, TVPARC, IERR)
         ELSE
            TVPARC = ' '
            END IF
C                                       TVCHANS
      ELSE IF (IMEM.EQ.3) THEN
         CALL COPY (16, VALUE, TVCHNS)
C                                       TVCHANS
      ELSE IF (IMEM.EQ.4) THEN
         CALL COPY (8, VALUE, GRCHNS)
C                                       WINLOAD
      ELSE IF (IMEM.EQ.5) THEN
         WINLOD = VALUE(1)
         END IF
C
 995  IF (IERR.NE.0) CALL MSGWRT (7)
C
 999  RETURN
      END
C-----------------------------------------------------------------------
C   Public TV functions:
C-----------------------------------------------------------------------
      SUBROUTINE TVDBLK (NAME, TYPE, CHANS, IERR)
C-----------------------------------------------------------------------
C   interactive blink/enhance displays
C   Inputs:
C      NAME     C*?    Name of open TV device object
C      CHANS    I(2)   Two channel numbers to blink: default TVCHANS
C   Output:
C      IERR     I      Error code: > 10 => object not open properly
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   TYPE, CHANS(2), IERR
C
      INTEGER   ICS(2), IT
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
C                                       set parms
      ICS(1) = CHANS(1)
      IF ((ICS(1).LE.1) .OR. (ICS(1).GT.NGRAY)) ICS(1) = 1
      ICS(2) = CHANS(2)
      IF ((ICS(2).LE.1) .OR. (ICS(2).GT.NGRAY)) ICS(2) = 2
      IF (ICS(1).EQ.ICS(2)) ICS(2) = 3 - ICS(1)
      IF (ICS(2).LE.0) ICS(2) = 7 - ICS(1)
      IF (ICS(2).LE.0) ICS(2) = 1
      IT = MAX (1, MIN (2, TYPE))
C                                       do it
      CALL TVBLNK (IT, ICS, TVSCRB, IERR)
C
 999  RETURN
      END
      SUBROUTINE TVDBOX (NAME, MSLEV, CHAN, MAXBOX, NBOX, BLC, TRC,
     *   IERR)
C-----------------------------------------------------------------------
C   create/revise up to maxbox boxes into the current TV image
C   Inputs:
C      NAME     C*?      Open TV-DEVICE object
C      MSLEV    I        message level to use after boxes set: 0 none
C      CHAN     I        Graphics channel number: default GRCHNS(1)
C      MAXBOX   I        Max allowed number of boxes: 0 => slice
C                           < 0 => display only
C   In/Out:
C      NBOX     I        Number of boxes preset (in), set (out)
C                           in < 0 => call delete box function
C      BLC      R(7,*)   Bottom left corner in imge pixels
C      TRC      R(7,*)   Top right corner in image pixels
C   Output:
C      IERR     I        Error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   MSLEV, CHAN, MAXBOX, NBOX, IERR
      REAL      BLC(7,*), TRC(7,*)
C
      INTEGER   IGR(2), I
      LOGICAL   UNIQUE
      INCLUDE 'TVDEV.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
C                                       find the image catalog block
      CALL COPY (256, CATBLK, CATSAV)
      CALL TVFIND (NGRAY, 'MA', I, UNIQUE, CATBLK, TVSCRB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       do it
      IGR(1) = CHAN
      IF (IGR(1).EQ.0) IGR(1) = MAX (1, MIN (GRCHNS(1), NGRAPH))
      IF (IGR(1).EQ.0) IGR(1) = MAX (1, MIN (3, NGRAPH - 1))
      IGR(2) = 7
      GRSTAT(IGR(1)) = 1
      IF (MAXBOX.NE.0) THEN
         IF (NBOX.GE.0) THEN
            CALL GRBOXS (IGR, MAXBOX, NBOX, BLC, TRC, TVSCRB, IERR)
         ELSE
            NBOX = -NBOX
            CALL GRBOXD (IGR, NBOX, BLC, TRC, TVSCRB, IERR)
            END IF
      ELSE
         CALL GRSLIC (I, BLC, TRC, TVSCRB, IERR)
         END IF
      IF ((IERR.EQ.0) .AND. (MSLEV.GT.0)) THEN
         IF (MAXBOX.GT.0) THEN
            DO 10 I = 1,NBOX
               WRITE (MSGTXT,1000) I, BLC(1,I), BLC(2,I), TRC(1,I),
     *            TRC(2,I)
               CALL MSGWRT (MSLEV)
 10            CONTINUE
         ELSE IF (MAXBOX.EQ.0) THEN
            WRITE (MSGTXT,1010) BLC(1,1), BLC(2,1), TRC(1,1),
     *         TRC(2,1)
            CALL MSGWRT (MSLEV)
            END IF
         END IF
C
 990  CALL COPY (256, CATSAV, CATBLK)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TVDBOX: BOX(',I2,') = ',4F8.2)
 1010 FORMAT ('TVDBOX: SLICE BLC = ',2F8.2,'  TRC = ',2F8.2)
      END
      SUBROUTINE TVDCAT (NAME, OPCODE, CHAN, IXY, CATBLK, IERR)
C-----------------------------------------------------------------------
C   reads/writes the TV image catalog
C   Inputs:
C      NAME     C*?      TV-DEVICE object already opened
C      OPCODE   C*4      'READ', 'WRIT'
C      CHAN     I        Channel number 1 -> NGRAY+NGRAPH
C      IXY      I(2)     X, Y pixels on TV for READ only
C   In/Out:
C      CATBLK   I(256)   image header (in on WRIT, out on READ)
C   Output:
C      IERR     I        error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), OPCODE*4
      INTEGER   CHAN, IXY(2), CATBLK(256), IERR
C
      INCLUDE 'TVDEV.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
C                                       read
      IF (OPCODE.EQ.'READ') THEN
         CALL YCREAD (CHAN, IXY(1), IXY(2), CATBLK, IERR)
C                                       write
      ELSE IF (OPCODE.EQ.'WRIT') THEN
         CALL YCWRIT (CHAN, CATBLK(IICOR), CATBLK, TVSCRB, IERR)
C                                       error
      ELSE
         MSGTXT = 'TVDCAT: BAD OPCODE = ''' // OPCODE // ''''
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
      END
      SUBROUTINE TVDCHR (NAME, X, Y, ANGLE, TYPE, CHAN, STRING, IERR)
C-----------------------------------------------------------------------
C   write characters on the TV
C   Inputs:
C      NAME     C*?   TV-DEVICE object already opened
C      X        I     BLC of 1st character in X
C      Y        I     BLC of 1st character in Y
C      ANGLE    I     angle
C      TYPE     I     Where to write the string
C                        < 0 => grey planes
C                          0 => single graphics plane
C                        > 0 => graphics + background in graphics chan 8
C      CHAN     I     Channel number: default GRCHNS(1), TVCHNS(1)
C      STRING   C*?   String to write - trailing blanks are written
C   Outputs:
C      IERR     I     Error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), STRING*(*)
      INTEGER   X, Y, ANGLE, TYPE, CHAN, IERR
C
      INTEGER   IC
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
C                                       channel
      IC = CHAN
      IF (TYPE.EQ.0) THEN
         IF (IC.EQ.0) IC = MAX (1, MIN (NGRAPH, GRCHNS(1)))
         GRSTAT(IC) = 1
         IC = IC + NGRAY
      ELSE IF (TYPE.LT.0) THEN
         IF (IC.EQ.0) IC = MAX (1, MIN (NGRAY, TVCHNS(1)))
         TVSTAT(IC) = 1
      ELSE
         IF (IC.EQ.0) IC = MAX (1, MIN (NGRAPH, GRCHNS(1)))
         IF (IC.EQ.NGRAPH) IC = IC - 1
         GRSTAT(NGRAPH) = 1
         GRSTAT(IC) = 1
         END IF
C                                       no background
      IF (TYPE.LE.0) THEN
         CALL IMCHAR (IC, X, Y, ANGLE, 0, STRING, TVSCRB, IERR)
C                                       background
      ELSE
         CALL IMANOT ('WRIT', IC, X, Y, ANGLE, 0, STRING, TVSCRB, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE TVDCRN (NAME, CORN, IERR)
C-----------------------------------------------------------------------
C   return TV coordinates of top left corner that is visible
C   Inputs:
C      NAME     C*?    TV device name
C   Outputs:
C      CORN     I(4)   TV coordinates of bottom left and top right
C                      visible pixel
C      IERR     I      Error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   CORN(4), IERR
C
      INTEGER   MAG, IX, IY
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
C                                       compute corner: X axis
      MAG = 1 + TVZOOM(1)
      IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
      IX = WINDTV(1) - (MAG-1)/2
      IX = (IX - TVZOOM(2)) / MAG + TVZOOM(2)
      IX = MOD (IX - 1 - TVSCGX + 3*MAXXTV(1), MAXXTV(1)) + 1
      CORN(1) = MAX (1, IX)
      IX = WINDTV(3) - (MAG-1)/2
      IX = (IX - TVZOOM(2)) / MAG + TVZOOM(2)
      IX = MOD (IX - 1 - TVSCGX + 3*MAXXTV(1), MAXXTV(1)) + 1
      CORN(3) = MAX (1, IX)
C                                       Y axis
      IY = WINDTV(2) - MAG + 1 - (MAG-1)/2
      IF (MAG.GT.1) IY = IY + MAG
      IY = (IY - TVZOOM(3)) / MAG + TVZOOM(3)
      IY = MOD (IY - 1 - TVSCGY + 3*MAXXTV(2), MAXXTV(2)) + 1
      CORN(2) = MAX (1, IY)
      IY = WINDTV(4) - MAG + 1 - (MAG-1)/2
      IF (MAG.GT.1) IY = IY + MAG
      IY = (IY - TVZOOM(3)) / MAG + TVZOOM(3)
      IY = MOD (IY - 1 - TVSCGY + 3*MAXXTV(2), MAXXTV(2)) + 1
      CORN(4) = MAX (1, IY)
C
 999  RETURN
      END
      SUBROUTINE TVDFUN (NAME, OPCODE, CHANS, IERR)
C-----------------------------------------------------------------------
C   Does various zoom, scroll, coloring, and enhancement functions
C   Input:
C      NAME     C*?   Object name
C      OPCODE   C*?   FIDD => TVFIDDLE current on channels
C                     PSEU => TVPSEUDO
C                     FLAM => TVPHLAME
C                     ZOOM => TVZOOM
C                     TRAN => TVTRANSF on CHANS
C                     SCRO => TVSCROLL
C                     OFFC => init color transfer
C                     OFFZ => init zoom
C                     OFFT => init B&W transfer on CHANS
C                     OFFS => init scroll
C      CHANS    I(16) List of channels for TRAN, OFFT: default those on
C   Output:
C      IERR     I     Error code: > 10 => device not open, else TV ierr
C                        0 is returned when OPCODE or CHANS are not
C                        correct
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), OPCODE*(*)
      INTEGER   CHANS(16), IERR
C
      INTEGER   I, NLEVS, J, IC, ZOR, SX, SY, JJ
      REAL      SLOPE, OFFS, RPOS(2,2)
      CHARACTER OPTYPE*4
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
      OPTYPE = OPCODE(:4)
      CALL RFILL (4, 0.0, RPOS)
      IC = 0
      DO 10 I = 1,16
         IF (CHANS(I).GT.0) THEN
            J = 2 ** (CHANS(I) - 1)
            IC = ZOR (IC, J)
            END IF
 10      CONTINUE
      IF (IC.EQ.0) THEN
         IC = ZOR (TVLIMG(1), TVLIMG(2))
         IC = ZOR (IC, TVLIMG(3))
         IC = ZOR (IC, TVLIMG(4))
         END IF
C                                       FIDDle
      NLEVS = LUTOUT + 1
      IF (OPTYPE.EQ.'FIDD') THEN
         CALL TVFIDL (IC, NLEVS, TVSCRB, IERR)
C                                       TVPSEUDO
      ELSE IF (OPTYPE.EQ.'PSEU') THEN
         CALL TVPSUD (NLEVS, TVOFM, IERR)
C                                       TVPHLAME
      ELSE IF (OPTYPE.EQ.'FLAM') THEN
         CALL TVFLAM (NLEVS, TVSCRB, IERR)
C                                       TVZOOM
      ELSE IF (OPTYPE.EQ.'ZOOM') THEN
         CALL TVZOME (IERR)
C                                       TVTRANSF on CHANS
      ELSE IF (OPTYPE.EQ.'TRAN') THEN
         J = 2 ** NGRAY
         IC = MOD (IC, J)
C                                       instructions
         MSGTXT = 'Cursor X position controls intercept'
         CALL MSGWRT (1)
         MSGTXT = 'Cursor Y position controls slope'
         CALL MSGWRT (1)
         MSGTXT = 'Hit buttons A or B to turn plot off or back on'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to reverse sign of slope'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to exit'
         CALL MSGWRT (1)
C                                        hide this mess in subroutine
         I = 1
         CALL IENHNS (IC, 7, I, RPOS, TVSCRB, IERR)
         GRSTAT(NGRAPH-1) = -1
C                                       OFFColor
      ELSE IF (OPTYPE.EQ.'OFFC') THEN
         I = OFMINP + 1
         CALL RFILL (I, 0.0, TVOFM)
         NLEVS = LUTOUT + 1
         IF (I.LT.NLEVS) NLEVS = I
         OFFS = 1.0 / OFMOUT
         SLOPE = OFFS * (OFMOUT + 1.0) / NLEVS
         DO 510 I = 1,NLEVS
            TVOFM(I) = I * SLOPE - OFFS
 510        CONTINUE
         I = OFMINP + 1
         I = I / NLEVS
         JJ = NLEVS
         DO 511 J = 2,I
            CALL RCOPY (NLEVS, TVOFM, TVOFM(JJ+1))
            JJ = JJ + NLEVS
 511        CONTINUE
         CALL YOFM ('WRIT', 7, .FALSE., TVOFM, IERR)
C                                       OFFZOOM
      ELSE IF (OPTYPE.EQ.'OFFZ') THEN
         TVZOOM(1) = 0
         TVZOOM(2) = MAXXTV(1) / 2
         TVZOOM(3) = MAXXTV(2) / 2
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .FALSE., IERR)
C                                       OFFT => init transfer on CHANS
      ELSE IF (OPTYPE.EQ.'OFFT') THEN
         J = 2 ** NGRAY
         IC = MOD (IC, J)
         J = MAXINT + 1
         SLOPE = (LUTOUT + 1.0) / (MAXINT + 1.0)
         DO 720 I = 1,J
            TVSCRB(I) = I * SLOPE - 0.5
 720        CONTINUE
         CALL YLUT ('WRIT', IC, 7, .FALSE., TVSCRB, IERR)
C                                       OFFS => init scroll in CHANS
      ELSE IF ((OPTYPE.EQ.'OFFS') .OR. (OPTYPE.EQ.'SCRO')) THEN
         IC = 0
         DO 810 I = 1,16
            IF (CHANS(I).GT.0) THEN
               J = 2 ** (MIN (CHANS(I), NGRAY+1) - 1)
               IC = ZOR (IC, J)
               END IF
 810        CONTINUE
         IF (IC.EQ.0) THEN
            IC = ZOR (TVLIMG(1), TVLIMG(2))
            IC = ZOR (IC, TVLIMG(3))
            IC = ZOR (IC, TVLIMG(4))
            END IF
         IF (OPTYPE.EQ.'OFFS') THEN
            SX = 0
            SY = 0
            CALL YSCROL (IC, SX, SY, .TRUE., IERR)
         ELSE
            CALL TVSCRL (IC, IERR)
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE TVDGRC (NAME, OPCODE, CHAN, RGB, IERR)
C-----------------------------------------------------------------------
C   read or write graphics plane colors
C   Inputs:
C      NAME     C*?    TV Device object name
C      OPCODE   C*?    READ or WRIT
C      CHAN     I      Graphics channel (1-NGRAPH, 0 -> cursor,
C                         <0 or > NGRAPH -> GRCHNS(1)
C   In/Out:
C      RGB      R(3)   Red green blue colors (0.0-1.0)
C   Output:
C      IERR     I      Error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), OPCODE*(*)
      INTEGER   CHAN, IERR
      REAL      RGB(3)
C
      INTEGER   IC
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
C                                       check parms
      IERR = 2
      IC = CHAN
      IF ((IC.LT.0) .OR. (IC.GT.NGRAPH)) IC = GRCHNS(1)
      IF ((IC.LT.0) .OR. (IC.GT.NGRAPH)) GO TO 999
      IF ((OPCODE(:4).NE.'READ') .AND. (OPCODE(:4).NE.'WRIT')) GO TO 999
C                                       do it
      CALL YGRAFX (OPCODE(:4), IC, RGB(1), RGB(2), RGB(3), IERR)
C
 999  RETURN
      END
      SUBROUTINE TVDINT (NAME, CORN, FIRST, TVXY, TVBUTT, IERR)
C-----------------------------------------------------------------------
C   Interact with the TV by standard means.  The calling program must
C   issue the user instructions and carry out any requested operations.
C   Inputs:
C      NAME     C*?      Open TV-DEVICE object
C      CORN     I(4)     Keep cursor within these bounds: NO DEFAULT
C   In/out:
C      FIRST    L        This is the first in a series of calls
C      TVXY     R(2)     TV cursor position corrected for scroll
C   Output:
C      TVBUTT   I        TV button setting
C      IERR     I        Error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   CORN(4), TVBUTT, IERR
      LOGICAL   FIRST
      REAL      TVXY(2)
C
      INTEGER   ITW(3), QUAD, IBUT
      REAL      PPOS(2)
      LOGICAL   DOIT, T, F
      INCLUDE 'TVDEV.INC'
      SAVE ITW, PPOS
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
C
      QUAD = -1
      IF (FIRST) THEN
         IF (TVXY(1).LT.CORN(1)) TVXY(1) = (CORN(1) + CORN(3)) / 2.0
         IF (TVXY(2).LT.CORN(2)) TVXY(2) = (CORN(2) + CORN(4)) / 2.0
         IF (TVXY(1).GT.CORN(3)) TVXY(1) = (CORN(3) + CORN(1)) / 2.0
         IF (TVXY(2).GT.CORN(4)) TVXY(2) = (CORN(4) + CORN(2)) / 2.0
         CALL ZTIME (ITW)
         FIRST = .FALSE.
         CALL YCURSE ('ONNN', F, T, TVXY, QUAD, IBUT, IERR)
         IF (IERR.NE.0) GO TO 999
         PPOS(1) = 0.0
         PPOS(2) = 0.0
         END IF
C                                       read until moves
 20   CALL YCURSE ('READ', F, T, TVXY, QUAD, TVBUTT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       under control
      DOIT = (TVXY(1).LT.CORN(1)) .OR. (TVXY(1).GT.CORN(3)) .OR.
     *   (TVXY(2).LT.CORN(2)) .OR. (TVXY(2).GT.CORN(4))
      IF (DOIT) THEN
         IF (TVXY(1).LT.CORN(1)) TVXY(1) = CORN(1)
         IF (TVXY(2).LT.CORN(2)) TVXY(2) = CORN(2)
         IF (TVXY(1).GT.CORN(3)) TVXY(1) = CORN(3)
         IF (TVXY(2).GT.CORN(4)) TVXY(2) = CORN(4)
         CALL YCURSE ('ONNN', F, T, TVXY, QUAD, IBUT, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      IF (TVBUTT.GT.0) THEN
         CALL YWINDO ('READ', WINDTV, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZTIME (ITW)
      ELSE
         CALL DLINTR (TVXY, TVBUTT, PPOS, ITW, DOIT)
         IF (.NOT.DOIT) GO TO 20
         END IF
C
 999  RETURN
      END
      SUBROUTINE TVDLAB (NAME, TYPE, CHAN, LTYPE, DOACRO, IERR)
C-----------------------------------------------------------------------
C   label an image
C   Inputs:
C      NAME     C*?      Open TV-DEVICE object
C      TYPE     C*2      'WE'dge, 'MA' image
C      CHAN     I        Graphics channel number: default GRCHNS(1)
C      LTYPE    I        Type of labeling to use: see help LTYPE
C      DOACRO   L        Ticks all the way across?
C   Output:
C      IERR     I        Error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), TYPE*2
      INTEGER   CHAN, LTYPE, IERR
      LOGICAL   DOACRO
C
      INTEGER   I, DOZERO
      LOGICAL   UNIQUE
      CHARACTER TTYPE*2
      INCLUDE 'TVDEV.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
C                                       find the image catalog block
      CALL COPY (256, CATBLK, CATSAV)
      TTYPE = TYPE
      IF (TTYPE.NE.'WE') TTYPE = 'MA'
      CALL TVFIND (NGRAY, TTYPE, I, UNIQUE, CATBLK, TVSCRB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       do it
      I = CHAN
      IF (I.EQ.0) I = MAX (1, MIN (GRCHNS(1), NGRAPH))
      IF (I.EQ.0) I = MAX (1, MIN (2, NGRAPH - 1))
      GRSTAT(I) = 1
      GRSTAT(NGRAPH) = 1
      IF (LOCNUM.LE.0) LOCNUM = 1
      LABTYP(LOCNUM) = 0
      DOZERO = -1
      IF (UNIQUE) DOZERO = 1
      CALL IAXIS1 (TVSCRB, LTYPE, I, DOZERO, DOACRO, IERR)
C
 990  CALL COPY (256, CATSAV, CATBLK)
C
 999  RETURN
      END
      SUBROUTINE TVDLIN (NAME, TYPE, CHAN, NPOINT, X, Y, IERR)
C-----------------------------------------------------------------------
C   connect npoint vertices with lines
C   Inputs:
C      NAME     C*?    Open TV device object
C      TYPE     I      TYpe of op
C                         1 draw on graphics, 2 draw on grey channel
C                         3 erase on graphics, 4 erase on gray
C      CHAN     I      channel number: default GRCHNS(1), TVCHNS(1)
C      NPOINT   I      number of points
C      X        I(*)   vertex X values
C      Y        I(*)   vertex Y values
C   Outputs
C      IERR     I      error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   TYPE, CHAN, NPOINT, X(*), Y(*), IERR
C
      INTEGER   IC
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
C                                       channel
      IC = CHAN
      IF (TYPE.EQ.1) THEN
         IF (IC.EQ.0) IC = MAX (1, MIN (NGRAPH, GRCHNS(1)))
         GRSTAT(IC) = 1
         IC = IC + NGRAY
      ELSE IF (TYPE.EQ.2) THEN
         IF (IC.EQ.0) IC = MAX (1, MIN (NGRAY, TVCHNS(1)))
         TVSTAT(IC) = 1
      ELSE IF (TYPE.EQ.3) THEN
         IF (IC.EQ.0) IC = MAX (1, MIN (NGRAPH, GRCHNS(1)))
         IC = IC + NGRAY
      ELSE
         IF (IC.EQ.0) IC = MAX (1, MIN (NGRAY, TVCHNS(1)))
         END IF
C                                       do it
      IF (TYPE.LE.2) THEN
         CALL IMVECT ('ONNN', IC, NPOINT, X, Y, TVSCRB, IERR)
      ELSE
         CALL IMVECT ('OFFF', IC, NPOINT, X, Y, TVSCRB, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE TVDLN3 (NAME, TYPE, CHAN, COLOR, NPOINT, X, Y, IERR)
C-----------------------------------------------------------------------
C   connect npoint vertices with 3-color lines
C   Inputs:
C      NAME     C*?    Open TV device object
C      TYPE     I      TYpe of op
C                         2 draw on grey channels
C                         4 erase on gray channels
C      CHAN     I      channel number: default TVCHNS(1), chan+1,chan+2
C      COLOR    R(3)   R,G,B color to use (0->1)
C      NPOINT   I      number of points
C      X        I(*)   vertex X values
C      Y        I(*)   vertex Y values
C   Outputs
C      IERR     I      error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   TYPE, CHAN, NPOINT, X(*), Y(*), IERR
      REAL      COLOR(3)
C
      INTEGER   IC, INT
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
C                                       channel
      IC = CHAN
      IF (IC.EQ.0) IC = MAX (1, MIN (NGRAY-2, TVCHNS(1)))
C                                       on
      IF (TYPE.LE.2) THEN
         TVSTAT(IC) = 1
         TVSTAT(IC+1) = 1
         TVSTAT(IC+2) = 1
         INT = COLOR(1) * MAXINT + 0.5
         CALL TVVECT (INT, IC, NPOINT, X, Y, TVSCRB, IERR)
         INT = COLOR(2) * MAXINT + 0.5
         IF (IERR.EQ.0) CALL TVVECT (INT, IC+1, NPOINT, X, Y, TVSCRB,
     *      IERR)
         INT = COLOR(3) * MAXINT + 0.5
         IF (IERR.EQ.0) CALL TVVECT (INT, IC+2, NPOINT, X, Y, TVSCRB,
     *      IERR)
C                                       off
      ELSE
         INT = 0
         CALL TVVECT (INT, IC, NPOINT, X, Y, TVSCRB, IERR)
         INT = 0
         IF (IERR.EQ.0) CALL TVVECT (INT, IC+1, NPOINT, X, Y, TVSCRB,
     *      IERR)
         INT = 0
         IF (IERR.EQ.0) CALL TVVECT (INT, IC+2, NPOINT, X, Y, TVSCRB,
     *      IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE TVDLOD (NAME, DISK, CNO, CHAN, PIXRNG, BLC, TRC, PINC,
     *   FUNTYP, TVCORN, IERR)
C-----------------------------------------------------------------------
C   loads image from disk, cno to TV chan
C   TVDLOD takes care of getting an LUN and buffer, filling in the image
C   catalog header, and defaulting the arguments.
C   Inputs:
C      NAME     C*?    Open TV device object name
C      DISK     I      Disk number
C      CNO      I      Catalog number for MA file
C      CHAN     I      Grey channel number: default TVCHNS(1)
C      PIXRNG   R(2)   Grey range - default max in data
C      BLC      I(7)   Bottom left corner
C      TRC      I(7)   Top right corner: 3-5 ignored
C      PINC     I(2)   X, Y pixel increments < 0 => interpolate
C      FUNTYP   C*2    Function applied:
C      TVCORN   I(2)   Force specific TV BLC
C   Output:
C      IERR     I      Error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), FUNTYP*2
      INTEGER   DISK, CNO, CHAN, BLC(7), TRC(7), PINC(2), TVCORN(2),
     *   IERR
      REAL      PIXRNG(2)
C
      INTEGER   BUFNO, LUN, IND, TVWIN(4), IMWIN(4), LINC(2), TYPE, IC,
     *   JERR, I, JBUFSZ
      REAL      LBLC(7), LTRC(7)
      CHARACTER TEMPIM*32, PHNAME*48
      INCLUDE 'TVDEV.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:CLASSIO.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
C                                       save the image catalog block
      CALL COPY (256, CATBLK, CATSAV)
C                                       get the catblk
      CALL CATIO ('READ', DISK, CNO, CATBLK, 'REST', TVSCRB, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.6)) GO TO 990
C                                       Make temporary object for buffer
      TEMPIM = 'Temporary image for TVDLOD'
      CALL OBCREA (TEMPIM, 'IMAGE   ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open for buffers
      CALL OBOPEN (TEMPIM, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       Get buffer numbers
      CALL OBINFO (TEMPIM, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       other parms
      IC = CHAN
      IF ((IC.LE.0) .OR. (IC.GT.NGRAY)) IC = MAX (1, MIN (NGRAY,
     *   TVCHNS(1)))
      CALL RNGSET (PIXRNG, CATR(KRDMX), CATR(KRDMN), CATR(IRRAN))
      CATBLK(IIVOL) = DISK
      CATBLK(IICNO) = CNO
      CALL CHR2H (2, FUNTYP, 1, CATH(IITRA))
      TVWIN(1) = TVCORN(1)
      TVWIN(2) = TVCORN(2)
      TYPE = -1
      CALL COPY (2, PINC, LINC)
      DO 10 I = 1,7
         LBLC(I) = BLC(I)
         LTRC(I) = TRC(I)
 10      CONTINUE
      CALL TVWIND (TYPE, LINC, LBLC, LTRC, IC, TVWIN, IMWIN, IERR)
      CALL MOVIST ('OFFF', IC, 0, 0, 0, IERR)
      TVSTAT(IC) = 1
C                                       open and load
      CALL ZPHFIL ('MA', DISK, CNO, 1, PHNAME, IERR)
      CALL ZOPEN (LUN, IND, DISK, PHNAME, .TRUE., .FALSE., .TRUE., IERR)
      IF (IERR.NE.0) GO TO 980
      JBUFSZ = 2 * BUFSIZ
      CALL TVLOAD (LUN, IND, IC, LINC, TVWIN, IMWIN, JBUFSZ,
     *   OBUFFR(1,BUFNO), IERR)
      CALL ZCLOSE (LUN, IND, JERR)
      IF (IERR.EQ.0) IERR = JERR
C
 980  CALL OBCLOS (TEMPIM, JERR)
      CALL OBLUFR (LUN)
 985  CALL OBFREE (TEMPIM, JERR)
C
 990  CALL COPY (256, CATSAV, CATBLK)
C
 999  RETURN
      END
      SUBROUTINE TVDMEN (NAME, TYPE, NCOL, NROWS, GRCHS, TOPSEP, SIDSEP,
     *   ISHELP, CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHOICE, TVBUTT,
     *   IERR)
C-----------------------------------------------------------------------
C   Displays a menu on a graphics channel, interactively highlights the
C   currently selected item, provides help, returns if time limit
C   exceeded or a menu selection was made.  It was intended to do
C   call-back functions, but one cannot pass an array of external names
C   in Fortran.  Therefore, the calling routine should handle this which
C   does give a lot more flexibility.
C   Inputs:
C      NAME     C*?    Open TVDEVICE object
C      TYPE     I      How to arrange columns: 0 left justified,
C                         1 right justified, 2 centered,
C                        -1 split to edges (NCOL=2)
C      NCOL     I      Number of columns
C      NROWS    I(*)   Number rows in each column
C      TOPSEP   I      Number pixels down from top for top of menu
C      SIDSEP   I      Number pixels down from left or right edge
C      CHOICS   C(*)*?  Strings of menu items
C      TIMLIM   I      Return after TIMLIM seconds even if no button
C                         0 => infinite time limit
C      LEAVE    L(*)   T => if the corresponding CHOICS is selected,
C                      leave the TV menu displayed, else turn off
C      NTITLE   I      Number of lines in TITLE
C      TITLE    C(*)*(*)  Title to menu ' ' => none
C   In/out:
C      ISHELP   C*6    Name of interactive help file = 'HLPishelp.HLP'
C                      ' '=> none, -> ' ' if error
C   Outputs:
C      GRCHS    I(2)   Graphics channels to use for (1) menu (default 1)
C                         (2) highlight (default omit)
C                         If GRCHS(1) < 0, use abs(GRCHS(1)) w no init
C                         i.e. this menu is already written there
C                         default GRCHANS(1) and (2)
C      CHOICE   I      Chosen item from list
C      TVBUTT   I      Selected button: 0 => timed out
C      IERR     I      Error code: 2 input error
C   Set GRCHS before the first call, then leave alone after that.
C   Limits: 10 columns, 40 rows/column
C-----------------------------------------------------------------------
      INTEGER   TYPE, NCOL, NROWS(*), GRCHS(2), TOPSEP, SIDSEP, TIMLIM,
     *   NTITLE, CHOICE, TVBUTT, IERR
      CHARACTER NAME*(*), CHOICS(*)*(*), ISHELP*(*), TITLE(*)*(*)
      LOGICAL   LEAVE(*)
C
      INTEGER   LGRCHS(2), AGS(2)
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
C                                       set graphics: force init
      AGS(1) = GRCHS(1)
      AGS(2) = GRCHS(2)
      IF (AGS(1).LE.0) AGS(1) = GRCHNS(1)
      IF (AGS(1).LE.0) AGS(1) = 2
      AGS(1) = MAX (1, MIN (NGRAPH, AGS(1)))
      IF (NGRAPH.EQ.1) THEN
         AGS(2) = 0
      ELSE
         AGS(2) = GRCHS(2)
         IF (AGS(2).EQ.0) AGS(2) = GRCHNS(2)
         AGS(2) = MAX (0, MIN (AGS(2), NGRAPH))
         IF (AGS(2).EQ.AGS(1)) AGS(2) = 1
         IF (AGS(2).EQ.AGS(1)) AGS(2) = 2
         END IF
      CALL COPY (2, AGS, LGRCHS)
      IF (GRSTAT(AGS(1)).EQ.2) LGRCHS(1) = -AGS(1)
      IF (AGS(2).GT.0) THEN
         IF (GRSTAT(AGS(2)).EQ.0) LGRCHS(2) = -AGS(2)
         END IF
C                                       do menu
         CALL TVMENU (TYPE, NCOL, NROWS, LGRCHS, TOPSEP, SIDSEP, ISHELP,
     *      CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHOICE, TVBUTT,
     *      TVSCRB, IERR)
C                                       force status
      IF (IERR.EQ.0) THEN
         CALL COPY (2, AGS, GRCHS)
         GRSTAT(AGS(1)) = 2
         IF (AGS(2).NE.0) GRSTAT(AGS(2)) = 0
         END IF
C
 999  RETURN
      END
      SUBROUTINE TVDOFM (NAME, OPER, CHAN, NCONT, OFMFIL, IERR)
C-----------------------------------------------------------------------
C   do various OFM read/write and modify operations
C   Inputs:
C      NAME     C*?   Name of open TV device object
C      OPER     C*4   Operation code as
C                     'GAMM'  interactively experiment with the gamma
C                             correction factor; saved only on user 1
C                             password.
C                     'CONT'  interactive setting of multiple contours
C                             in a new OFM
C                     'WEDG'  interactive setting of multiple wedges
C                             in a new OFM
C                     'DIR '  list OFMs in OFMFIL area for user &
C                             AIPSOFM area for user 001
C                     'GET '  get a specified OFM from OFMFIL or AIPSOFM
C                             area
C                     'PUT '  save current OFM in named file in OFMFIL
C                     'ZAP '  delete a named file in OFMFIL area
C                     'TWEK'  interactive adjustment of OFM
C                     'ADJU'  numeric readjustment of the OFM
C      CHAN     I     Graphics channel to use, default GRCHNS(1)
C      NCONT    I     Number of contours for CONT, WEDG
C      OFMFIL   C*48  full file name
C   Outputs:
C      IERR     I     error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), OPER*4, OFMFIL*48
      INTEGER   CHAN, NCONT, IERR
C
      INTEGER   QUAD, NLEVS, IMLEVS, JERR, IBUT, IG, OFMDIM
      REAL      RPOS(2), DOWEDG, DONEW, GAMMA
      LOGICAL   T, F, CURSON, DOGROF
C
      INCLUDE 'TVDEV.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
C                                        Common TV parms
      OFMDIM = TVMOFM
      NLEVS = LUTOUT + 1
      IMLEVS = MAX (LUTOUT+1, NLEVS)
      IF (IMLEVS.GT.OFMINP+1) IMLEVS = OFMINP + 1
C                                        open TV device
      CURSON = F
      DOGROF = F
C                                       read TV OFM
      IF (OPER.NE.'GET ') THEN
         CALL OFMIO ('READ', OFMDIM, IMLEVS, F, TVOFM, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       branch to the verb itself
      IG = CHAN
      IF (IG.LE.0) IG = GRCHNS(1)
      IF (IG.LE.0) IG = MIN (4, NGRAPH-1)
      IERR = 0
C                                       interactive setting of Gamma
      IF (OPER.EQ.'GAMM') THEN
         DOGROF = T
         CURSON = T
         CALL OFMGAM (IG, OFMDIM, TVOFM, RPOS, GAMMA, TVSCRB, IERR)
C                                       OFM as colored contours
      ELSE IF (OPER.EQ.'CONT') THEN
         DOGROF = T
         CURSON = T
         DOWEDG = - MAX (1, NCONT)
         DONEW = -1.0
         CALL OFMCON (DOWEDG, IG, IMLEVS, DONEW, RPOS, OFMDIM, TVOFM,
     *      TVSCRB, IERR)
C                                       OFM as colored wedges
      ELSE IF (OPER.EQ.'WEDG') THEN
         DOGROF = T
         CURSON = T
         DOWEDG = MAX (1, NCONT)
         DONEW = -1.0
         CALL OFMCON (DOWEDG, IG, IMLEVS, DONEW, RPOS, OFMDIM, TVOFM,
     *      TVSCRB, IERR)
C                                       list AIPSOFM/OFMFIL area's files
      ELSE IF (OPER.EQ.'DIR ') THEN
         CALL OFMDIR ('DIR', OFMFIL, OFMDIM, TVOFM, IERR)
C                                       get named OFM to TV from AIPSOFM
C                                       or OFMFIL
      ELSE IF (OPER.EQ.'GET ') THEN
         CALL OFMDIR ('GET', OFMFIL, OFMDIM, TVOFM, IERR)
C                                       send to the TV
         IF (IERR.EQ.0) CALL OFMIO ('WRIT', OFMDIM, IMLEVS, F, TVOFM,
     *      IERR)
C                                       create OFM file in OFMFIL w OFM
      ELSE IF (OPER.EQ.'SAVE') THEN
         CALL OFMDIR ('SAVE', OFMFIL, OFMDIM, TVOFM, IERR)
C                                       kill named OFM file from OFMFIL
      ELSE IF (OPER.EQ.'ZAP ') THEN
         CALL OFMDIR ('ZAP', OFMFIL, OFMDIM, TVOFM, IERR)
C                                       interactive OFM adjustment
      ELSE IF (OPER.EQ.'TWEK') THEN
         CALL OFMMOD ('TWEAK', IG, OFMDIM, TVOFM, TVSCRB, IERR)
         CURSON = T
C                                       numeric linear OFM adjustment
      ELSE IF (OPER.EQ.'ADJU') THEN
         DOGROF = T
         CURSON = T
         CALL OFMMOD ('ADJUS', IG, OFMDIM, TVOFM, TVSCRB, IERR)
         END IF
C                                       close down
C                                       cursor off, TV closed
 980  IF (CURSON) CALL YCURSE ('OFFF', F, F, RPOS, QUAD, IBUT, JERR)
      IF (DOGROF) THEN
         GRSTAT(IG) = 1
         CALL TVDOPR (NAME, 'GRCL', IG, JERR)
         CALL TVDOPR (NAME, 'GROF', IG, JERR)
         IF (NGRAPH.GE.4) THEN
            GRSTAT(NGRAPH) = 1
            CALL TVDOPR (NAME, 'GRCL', NGRAPH, JERR)
            CALL TVDOPR (NAME, 'GROF', NGRAPH, JERR)
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE TVDOPR (NAME, OPCODE, CHAN, IERR)
C-----------------------------------------------------------------------
C   Does various full-channel, full TV operations
C   Also hold/resume updating display
C   Input:
C      NAME     C*?   Object name
C      OPCODE   C*?   INIT - init the full TV
C                     TVCL, GRCL - zero the specified TV, graphics chan
C                     TVON, GRON - turn on the spec. TV, graphics chan
C                     TVOF, GROF - turn iff the spec. TV, graphics chan
C                     HOLD, HOFF - hold screen updates, resume updates
C                     TV3C       - turn on CHAN->CHAN+2 in RGB
C      CHAN     I     Channel number: 1 - ngrey, 1 - ngraph
C                     default 0 => TVCHNS(1), GRCHNS(1)
C   Output:
C      IERR     I     Error code: > 10 => device not open, else TV ierr
C                        0 is returned when OPCODE or CHAN are not
C                        correct
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), OPCODE*(*)
      INTEGER   CHAN, IERR
C
      INTEGER   I, IC
      CHARACTER OPTYPE*4
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
      OPTYPE = OPCODE(:4)
      IC = CHAN
C                                       INIT
      IF (OPTYPE.EQ.'INIT') THEN
         CALL YINIT (TVSCRB, IERR)
         CALL FILL (16, 0, TVSTAT)
         CALL FILL (8, 0, GRSTAT)
C                                       TVCLear
      ELSE IF (OPTYPE.EQ.'TVCL') THEN
         IF (IC.EQ.0) IC = TVCHNS(1)
         IF ((IC.GE.1) .AND. (IC.LE.NGRAY) .AND. (TVSTAT(IC).NE.0)) THEN
            CALL YCINIT (IC, TVSCRB)
            CALL YZERO (IC, IERR)
            TVSTAT(IC) = 0
            END IF
C                                       GRCLear
      ELSE IF (OPTYPE.EQ.'GRCL') THEN
         IF (IC.EQ.0) IC = GRCHNS(1)
         IF ((IC.GE.1) .AND. (IC.LE.NGRAPH) .AND. (GRSTAT(IC).NE.0))
     *      THEN
            I = IC + NGRAY
            CALL YCINIT (I, TVSCRB)
            CALL YZERO (I, IERR)
            GRSTAT(IC) = 0
            END IF
C                                       TVON
      ELSE IF (OPTYPE.EQ.'TVON') THEN
         IF (IC.EQ.0) IC = TVCHNS(1)
         IF ((IC.GE.1) .AND. (IC.LE.NGRAY)) THEN
            CALL YSLECT ('ONNN', IC, 0, TVSCRB, IERR)
            END IF
C                                       GRON
      ELSE IF (OPTYPE.EQ.'GRON') THEN
         IF (IC.EQ.0) IC = GRCHNS(1)
         IF ((IC.GE.1) .AND. (IC.LE.NGRAPH)) THEN
            I = IC + NGRAY
            CALL YSLECT ('ONNN', I, 0, TVSCRB, IERR)
            END IF
C                                       TVOFf
      ELSE IF (OPTYPE.EQ.'TVOF') THEN
         IF (IC.EQ.0) IC = TVCHNS(1)
         IF ((IC.GE.1) .AND. (IC.LE.NGRAY)) THEN
            CALL YSLECT ('OFFF', IC, 0, TVSCRB, IERR)
            END IF
C                                       GROFf
      ELSE IF (OPTYPE.EQ.'GROF') THEN
         IF (IC.EQ.0) IC = GRCHNS(1)
         IF ((IC.GE.1) .AND. (IC.LE.NGRAPH)) THEN
            I = IC + NGRAY
            CALL YSLECT ('OFFF', I, 0, TVSCRB, IERR)
            END IF
C                                       HOLD
      ELSE IF ((OPTYPE.EQ.'HOLD') .OR. (OPTYPE.EQ.'HONN')) THEN
         CALL YHOLD ('ONNN', IERR)
C                                       HOFF
      ELSE IF (OPTYPE.EQ.'HOFF') THEN
         CALL YHOLD ('OFFF', IERR)
C                                       TV3C
      ELSE IF (OPTYPE.EQ.'TV3C') THEN
         IF (IC.EQ.0) IC = TVCHNS(1)
         IF ((IC.GE.1) .AND. (IC.LE.NGRAY-2)) THEN
            CALL YSLECT ('ONNN', IC, 1, TVSCRB, IERR)
            IF (IERR.EQ.0) CALL YSLECT ('ONNN', IC+1, 2, TVSCRB, IERR)
            IF (IERR.EQ.0) CALL YSLECT ('ONNN', IC+2, 3, TVSCRB, IERR)
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE TVDPOS (NAME, DOMENU, TIMLIM, TVXY, TVBUTT, IERR)
C-----------------------------------------------------------------------
C   get tv position within a time limit.  The TV button buffer is
C   cleared on entry if TIMLIM >= 0.  The TV cursor is turned on if
C   needed, but it is not moved to do so.
C   Inputs:
C      NAME     C*?    TV device object name - must be open
C      DOMENU   I      Prompt message control:
C                        -1 => cursor position unimportant
C                         0 => picking an image pixel
C                         1 => picking a menu item
C                         2 => menu help avalable
C      TIMLIM   I      Return after TIMLIM seconds even if no button
C                         0 => infinite time limit
C                        <= -1 => no message and no wait
C   Outputs:
C      TVXY     R(2)   TV x,y pixel position: corrected for zoom, not
C                      scroll
C      TVBUTT   I      Selected button: 0 => timed out
C      IERR     I      Error code: > 10 => object not open properly
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   DOMENU, TIMLIM, TVBUTT, IERR
      REAL      TVXY(2)
C
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
C                                       Do it
      CALL TVTPOS (DOMENU, TIMLIM, TVXY, TVBUTT, IERR)
C
 999  RETURN
      END
      SUBROUTINE TVDROM (NAME, TYPE, CHANS, IERR)
C-----------------------------------------------------------------------
C   do an interactive roam display, images already loaded
C   Inputs:
C      NAME    C*?     TV device object name
C      TYPE    I       100 * (#planes in X) + (#planes in Y)
C      CHANS   I(16)   list of channel numbers
C   Outputs:
C      IERR    I       Error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   TYPE, CHANS(*), IERR
C
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
C                                       Do it
      CALL TVROAM (TYPE, CHANS, TVSCRB, IERR)
C
 999  RETURN
      END
      SUBROUTINE TVDTVW (NAME, OPCODE, WIN, IERR)
C-----------------------------------------------------------------------
C   read or write the TV display window corners
C   Inputs:
C      NAME     C*?    TV Device object name
C      OPCODE   C*?    READ or WRIT
C   In/Out:
C      WIN      I(4)   TV display window corners: on WRIT in ->
C                      requested , out is actual on both cases
C   Output:
C      IERR     I      Error code
C   DTVC.INC parameter WINDTV also updated
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), OPCODE*(*)
      INTEGER   WIN(4), IERR
C
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
C                                       check parms
      IERR = 2
      IF ((OPCODE(:4).NE.'READ') .AND. (OPCODE(:4).NE.'WRIT')) GO TO 999
      IF (OPCODE(:4).EQ.'WRIT') THEN
         IF (WIN(1).LT.1) GO TO 999
         IF (WIN(2).LT.1) GO TO 999
         IF (WIN(3).GT.MAXXTV(1)) GO TO 999
         IF (WIN(4).GT.MAXXTV(2)) GO TO 999
         END IF
C                                       do it
      CALL YWINDO (OPCODE(:4), WIN, IERR)
      IF (IERR.EQ.0) THEN
         IF ((WIN(1).NE.WINDTV(1)) .OR. (WIN(2).NE.WINDTV(2)) .OR.
     *      (WIN(3).NE.WINDTV(3)) .OR. (WIN(4).NE.WINDTV(4))) THEN
            CALL FILL (8, -1, GRSTAT)
            CALL FILL (16, -1, TVSTAT)
            CALL COPY (4, WIN, WINDTV)
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE TVDSTR (NAME, TYPE, CHAN, NSTARS, STPOS, STPARM, IERR)
C-----------------------------------------------------------------------
C   write "star" symbols on the TV
C   Inputs
C      NAME     C*(*)    Open TV device
C      TYPE     C*2      Display type
C      CHAN     I        Graphics channel to use
C      NSTARS   I        Number of stars to plot
C      STPOS    D(2,*)   Star coordinates (RA/DEC, NSTARS)
C      STPARM   R(4,*)   Star major, minor, PA, symbol type
C   Outputs
C      IERR     I        error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), TYPE*2
      INTEGER   CHAN, NSTARS, IERR
      DOUBLE PRECISION STPOS(2,*)
      REAL      STPARM(4,*)
C
      INTEGER   I, IER, STTYPE, PLBUF(256)
      CHARACTER TTYPE*2
      DOUBLE PRECISION DX, DY, COSPA, SINPA, COSDEC
      REAL      BLC(2), TRC(2), DELJ, DELN, AX(5), AY(5), POSANG,
     *   STP(3)
      LOGICAL   UNIQUE
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'TVDEV.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DGPH.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
C                                       find the image catalog block
      CALL COPY (256, CATBLK, CATSAV)
      TTYPE = TYPE
      IF (TTYPE.NE.'WE') TTYPE = 'MA'
      CALL TVFIND (NGRAY, TTYPE, I, UNIQUE, CATBLK, TVSCRB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       do it
      I = CHAN
      IF (I.EQ.0) I = MAX (1, MIN (GRCHNS(1), NGRAPH))
      IF (I.EQ.0) I = MAX (1, MIN (2, NGRAPH - 1))
      GRSTAT(I) = 1
      GRSTAT(NGRAPH) = 1
      IF (LOCNUM.LE.0) LOCNUM = 1
C                                       set Graphics common
      CALL COPY (256, CATBLK, GPHCAT)
      GPHIX0 = GPHCAT(IICOR)
      GPHIY0 = GPHCAT(IICOR+1)
      GPHSCX = GPHCAT(IICOR+2) - GPHIX0
      GPHSCY = GPHCAT(IICOR+3) - GPHIY0
      GPHIX1 = GPHCAT(IIWIN)
      GPHIY1 = GPHCAT(IIWIN+1)
      GPHIX2 = GPHCAT(IIWIN+2)
      GPHIY2 = GPHCAT(IIWIN+3)
      GPHX1 = GPHIX1
      GPHX2 = GPHIX2
      GPHY1 = GPHIY1
      GPHY2 = GPHIY2
      GPHDOT = .TRUE.
      GPHDOD = .FALSE.
      GPHLTY = 1
      BLC(1) = GPHX1
      BLC(2) = GPHY1
      TRC(1) = GPHX2
      TRC(2) = GPHY2
      GPHTVG(1) = CHAN + NGRAY
      CALL YSLECT ('ONNN', GPHTVG(1), 0, PLBUF, IER)
      CALL YZERO (GPHTVG(1), IER)
      CALL SETLOC (GPHCAT(IIDEP), .FALSE.)
C                                       loop over stars
      DO 100 I = 1,NSTARS
         CALL XYPIX (STPOS(1,I), STPOS(2,I), AX(1), AY(1), IER)
         IF ((IER.EQ.0) .AND. (AX(1).GE.BLC(1)) .AND. (AX(1).LE.TRC(1))
     *      .AND. (AY(1).GE.BLC(2)) .AND. (AY(1).LE.TRC(2))) THEN
            STP(1) = STPARM(1,I) / 3600.0
            STP(2) = STPARM(2,I) / 3600.0
            STP(3) = STPARM(3,I)
            IF (STP(1).LE.0.0) THEN
               STP(1) = 3.0 * ABS (CATR(KRCIC+1))
               STP(2) = 3.0 * ABS (CATR(KRCIC))
               STP(3) = 0.0
            ELSE IF (STP(2).LE.0.0) THEN
               STP(2) = 3.0 * ABS (CATR(KRCIC))
            END IF
            POSANG = STP(3) * DG2RAD
            COSPA = COS (POSANG)
            SINPA = SIN (POSANG)
            COSDEC = ABS (COS (STPOS(2,I)*DG2RAD))
C                                       Calculate ends of star mark
            DELJ = 0.5 * STP(1)
            DELN = 0.5 * STP(2)
            IF (CORTYP(LOCNUM).EQ.1) THEN
               COSDEC = ABS (COS (STPOS(2,I)*DG2RAD))
               DX = STPOS(1,I) + DELJ*SINPA/COSDEC
               DY = STPOS(2,I) + DELJ*COSPA
            ELSE IF (CORTYP(LOCNUM).EQ.2) THEN
               COSDEC = ABS (COS (STPOS(1,I)*DG2RAD))
               DX = STPOS(1,I) + DELJ*COSPA
               DY = STPOS(2,I) + DELJ*SINPA/COSDEC
            ELSE
               DX = STPOS(1,I)
               DY = STPOS(2,I) + DELJ
               END IF
            CALL XYPIX (DX, DY, AX(2), AY(2), IER)
            IF (IER.NE.0) GO TO 100
            DX = 2.0 * STPOS(1,I) - DX
            DY = 2.0 * STPOS(2,I) - DY
            CALL XYPIX (DX, DY, AX(3), AY(3), IER)
            IF (IER.NE.0) GO TO 100
            IF (CORTYP(LOCNUM).EQ.1) THEN
               DX = STPOS(1,I) - DELN*COSPA/COSDEC
               DY = STPOS(2,I) + DELN*SINPA
            ELSE IF (CORTYP(LOCNUM).EQ.2) THEN
               DX = STPOS(1,I) + DELN*SINPA
               DY = STPOS(2,I) - DELN*COSPA/COSDEC
            ELSE
               DX = STPOS(1,I) - DELN
               DY = STPOS(2,I)
               END IF
            CALL XYPIX (DX, DY, AX(4), AY(4), IER)
            IF (IER.NE.0) GO TO 100
            DX = 2.0 * STPOS(1,I) - DX
            DY = 2.0 * STPOS(2,I) - DY
            CALL XYPIX (DX, DY, AX(5), AY(5), IER)
            IF (IER.NE.0) GO TO 100
            STTYPE = STPARM(4,I) + 0.1
            CALL PNTPLT (STTYPE, AX, AY, BLC, TRC, .FALSE., .FALSE.,
     *         PLBUF, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
 100     CONTINUE
C
 990  CALL COPY (256, CATSAV, CATBLK)
C
 999  RETURN
      END
      SUBROUTINE TVDVAL (NAME, CHAN, IERR)
C-----------------------------------------------------------------------
C   do interactive display of image value under cursor
C   Inputs:
C      NAME     C*?    Open TV device
C      CHAN     I      Graphics channel for lettering: default
C                      GRCHANS(1)
C   Output:
C      IERR     I      Error code: > 10 => object not open properly
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   CHAN, IERR
C
      INTEGER   GR, LUN, POTERR, CLIMIT
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
C                                       channel
      CLIMIT = 0
      GR = CHAN
      IF ((GR.LT.1) .OR. (GR.GT.NGRAPH)) GR = GRCHNS(1)
      IF ((GR.EQ.NGRAPH) .OR. (GR.LT.1)) GR = MIN (2, NGRAPH)
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       clear the graphics
      CALL TVDOPR (NAME, 'GRCL', GR, IERR)
      IF (IERR.EQ.0) CALL TVDOPR (NAME, 'GRCL', NGRAPH, IERR)
C                                       do it
      IF (IERR.EQ.0) THEN
         CALL TVALUE (LUN, GR, CLIMIT, TVSCRB, TVOFM, POTERR, IERR)
         GRSTAT(GR) = 1
         GRSTAT(NGRAPH) = 1
         END IF
C                                       turn them off
      IF (IERR.EQ.0) CALL TVDOPR (NAME, 'GROF', GR, IERR)
      IF (IERR.EQ.0) CALL TVDOPR (NAME, 'GROF', NGRAPH, IERR)
C                                       free lun
      CALL OBLUFR (LUN)
C
 999  RETURN
      END
      SUBROUTINE TVDWED (NAME, TYPE, CHAN, BLC, LENGTH, WIDTH, TVCAT,
     *   IERR)
C-----------------------------------------------------------------------
C   Do an intensity wedge on the TV
C   Inputs:
C      NAME     C*?      Open TV-DEVICE object
C      TYPE     I        Type of wedge: 0 -> zero, else 10*i + j, where
C                        i = 1 or 2 => use full or clipped range inten.
C                        j = 0,1,2,3 left, bot, right, top
C      CHAN     I        TV grey-scale channel number
C      BLC      I(2)     BLC on TV screen
C      LENGTH   I        length of wedge
C      WIDTH    I        width of wedge
C      CATH     I(256)   TV image header to be used
C   Output:
C      IERR     I        Error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   TYPE, CHAN, BLC(2), LENGTH, WIDTH, TVCAT(256), IERR
C
      INTEGER   I, IT, JT
      REAL      RTEMP(2)
      INCLUDE 'TVDEV.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
C                                       find the image catalog block
      CALL COPY (256, CATBLK, CATSAV)
      CALL COPY (256, TVCAT, CATBLK)
C                                       do it
      I = CHAN
      IF (I.EQ.0) I = MAX (1, MIN (TVCHNS(1), NGRAY))
      IF (I.EQ.0) I = 1
      TVSTAT(I) = 1
      IT = TYPE / 10
      JT = MOD (TYPE, 10)
      RTEMP(1) = 0.0
      RTEMP(2) = 0.0
      IF ((JT.EQ.1) .OR. (JT.EQ.3)) THEN
         CALL IWEDGE (IT, JT, I, BLC(1), BLC(2), LENGTH, WIDTH, RTEMP,
     *      TVOFM, TVSCRB, IERR)
      ELSE
         CALL IWEDGE (IT, JT, I, BLC(1), BLC(2), WIDTH, LENGTH, RTEMP,
     *      TVOFM, TVSCRB, IERR)
         END IF
C
      CALL COPY (256, CATSAV, CATBLK)
C
 999  RETURN
      END
      SUBROUTINE TVDWIN (NAME, MSLEV, CHAN, BLC, TRC, IERR)
C-----------------------------------------------------------------------
C   make 1 window into the current TV image
C   Inputs:
C      NAME     C*?    Open TV-DEVICE object
C      MSLEV    I      message level to use after boxes set: 0 none
C      CHAN     I      Graphics channel number: default GRCHNS(1)
C   In/Out:
C      BLC      R(7)   Bottom left corner in imge pixels
C      TRC      R(7)   Top right corner in image pixels
C   Output:
C      IERR     I      Error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   MSLEV, CHAN, IERR
      REAL      BLC(7), TRC(7)
C
      INTEGER   I, MAXBOX, NBOX
      LOGICAL   UNIQUE
      INCLUDE 'TVDEV.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
C                                       find the image catalog block
      CALL COPY (256, CATBLK, CATSAV)
      CALL TVFIND (NGRAY, 'MA', I, UNIQUE, CATBLK, TVSCRB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       do it
      I = CHAN
      IF (I.EQ.0) I = MAX (1, MIN (GRCHNS(1), NGRAPH))
      IF (I.EQ.0) I = MAX (1, MIN (3, NGRAPH - 1))
      GRSTAT(I) = 1
      MAXBOX = 1
      NBOX = 0
      CALL GRBOXS (I, MAXBOX, NBOX, BLC, TRC, TVSCRB, IERR)
      IF ((IERR.EQ.0) .AND. (MSLEV.GT.0)) THEN
         DO 10 I = 1,NBOX
            WRITE (MSGTXT,1000) BLC(1), BLC(2), TRC(1), TRC(2)
            CALL MSGWRT (MSLEV)
 10         CONTINUE
         END IF
C
 990  CALL COPY (256, CATSAV, CATBLK)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TVDWIN: WINDOW = ',4F8.2)
      END
      SUBROUTINE TVDXPR (NAME, TYPE, NCOL, NROWS, GRCHS, TOPSEP,
     *   CHOICS, LEAVE, TTY, N, CHOICX, SHORTX, ALPHAX, OPTOKX,
     *   CHOICE, IERR)
C-----------------------------------------------------------------------
C   Displays a menu on a graphics channel, highlights the expert codes,
C   and reads the terminal to determine the desired operation.
C   Inputs:
C      NAME     C*?    Open TVDEVICE object
C      TYPE     I      How to arrange columns: 0 left justified,
C                         1 right justified, 2 centered,
C                        -1 split to edges (NCOL=2)
C      NCOL     I      Number of columns
C      NROWS    I(*)   Number rows in each column
C      TOPSEP   I      Number pixels down from top for top of menu
C      CHOICS   C(*)*?  Strings of menu items
C      LEAVE    L(*)   T => if the corresponding CHOICS is selected,
C                      leave the TV menu displayed, else turn off
C      TTY      I(2)       TTY LUN and Find
C      N        I          Number of choices
C      CHOICX   C(N)*(*)   Long string list of choices
C      SHORTX   C(N)*4     Short string list of choices in the form of 1
C                          uppercase letter and 1-3 lowercase ones.  The
C                          single uppercase letter will be recognized.
C      ALPHAX   C(N)*1     List of upper case letters to recognize.
C                          Letters should not repeat and must not
C                          contain X (exit expert mode) or H (help).
C      OPTOKX   I(N)       Flag > 0 => option is currently allowed
C   Outputs:
C      GRCHS    I(2)   Graphics channels to use for (1) menu (default 1)
C                         (2) highlight (default omit)
C                         If GRCHS(1) < 0, use abs(GRCHS(1)) w no init
C                         i.e. this menu is already written there
C                         default GRCHANS(1) and (2)
C      CHOICE   C*(*)  Chosen item from list
C      TVBUTT   I      Selected button: 0 => timed out
C      IERR     I      Error code: 2 input error
C   Set GRCHS before the first call, then leave alone after that.
C   Limits: 10 columns, 40 rows/column
C-----------------------------------------------------------------------
      INTEGER   TYPE, NCOL, NROWS(*), GRCHS(2), TOPSEP, TTY(2), N,
     *   OPTOKX(N), IERR
      CHARACTER NAME*(*), CHOICS(*)*(*), CHOICX(N)*(*), SHORTX(N)*(*),
     *   ALPHAX(N)*1, CHOICE*(*)
      LOGICAL   LEAVE(*)
C
      INTEGER   LGRCHS(2), AGS(2)
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
      IERR = 0
C                                       set graphics: force init
      AGS(1) = GRCHS(1)
      AGS(2) = GRCHS(2)
      IF (AGS(1).LE.0) AGS(1) = GRCHNS(1)
      IF (AGS(1).LE.0) AGS(1) = 2
      AGS(1) = MAX (1, MIN (NGRAPH, AGS(1)))
      IF (NGRAPH.EQ.1) THEN
         AGS(2) = 0
      ELSE
         AGS(2) = GRCHS(2)
         IF (AGS(2).EQ.0) AGS(2) = GRCHNS(2)
         AGS(2) = MAX (0, MIN (AGS(2), NGRAPH))
         IF (AGS(2).EQ.AGS(1)) AGS(2) = 1
         IF (AGS(2).EQ.AGS(1)) AGS(2) = 2
         END IF
      CALL COPY (2, AGS, LGRCHS)
      IF (GRSTAT(AGS(1)).EQ.2) LGRCHS(1) = -AGS(1)
      IF (AGS(2).GT.0) THEN
         IF (GRSTAT(AGS(2)).EQ.0) LGRCHS(2) = -AGS(2)
         END IF
C                                       do menu
      CALL TVXPRT (TYPE, NCOL, NROWS, LGRCHS, TOPSEP, CHOICS, LEAVE,
     *   TTY, N, CHOICX, SHORTX, ALPHAX, OPTOKX, CHOICE, TVSCRB, IERR)
C                                       force status
      IF (IERR.EQ.0) THEN
         CALL COPY (2, AGS, GRCHS)
         GRSTAT(AGS(1)) = 2
         IF (AGS(2).NE.0) GRSTAT(AGS(2)) = 0
         END IF
C
 999  RETURN
      END
      SUBROUTINE TVDZOM (NAME, OPCODE, ZOOM, IERR)
C-----------------------------------------------------------------------
C   read or write the TV display zoom
C   Inputs:
C      NAME     C*?    TV Device object name
C      OPCODE   C*?    READ or WRIT
C   In/Out:
C      ZOOM     I(3)   TV display zoom parameters: on WRIT in ->
C                      requested , out is actual on both cases
C   Output:
C      IERR     I      Error code
C   DTVC.INC parameter TVZOOM also updated
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), OPCODE*(*)
      INTEGER   ZOOM(3), IERR
C
      INCLUDE 'TVDEV.INC'
C-----------------------------------------------------------------------
      IERR = 11
      IF (.NOT.ACTIVE) GO TO 999
      IERR = 12
      IF (NAME.NE.MYNAME) GO TO 999
C                                       check parms
      IERR = 2
      IF ((OPCODE(:4).NE.'READ') .AND. (OPCODE(:4).NE.'WRIT')) GO TO 999
      IERR = 0
      IF (OPCODE(:4).EQ.'WRIT') CALL YZOOMC (ZOOM(1), ZOOM(2), ZOOM(3),
     *   .TRUE., IERR)
      IF (IERR.EQ.0) CALL COPY (3, TVZOOM, ZOOM)
C
 999  RETURN
      END
