C    Printer Class Module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "Printer" class library
C# Map-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999, 2004, 2007, 2015
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
C    A Printer object is an interface to the line printer, or, as a user
C    option, printer line output on the terminal.  Only one printer
C    object may be active (open) at a  time.
C       Usage notes:
C    1) A printer object may be associated with an image or uvdata
C    object before opening by setting the value of 'PRINTOBJ' before
C    opening.  When the printer object is then opened a header is
C    written describing that object.
C    Class public members:
C      PRINTOBJ  C*32  Name of associated uvdata or image object.
C      LPFILE    C*48  Name of printer file.
C      DOCRT     I     If >0 use terminal, else printer.
C    Following only after opening:
C      TITLE1    C*132 First page title
C      TITLE2    C*132 Second page title
C
C   Class  private data:
C      MYNAME    C*32  Name of open printer
C      POBJ      C*32  Name of associated object
C      DOCRT     R     Interactive or batch printer
C      NACROS    I     Number of columns in output
C      NLINE     I     Line number on page
C      IPAGE     I     Page number.
C      PRTLUN    I     Printer LUN (1)
C      PRTIND    I     Printer FTAB pointer.
C
C   Public functions:
C     PRTCRE (name, ierr)
C        Creates a printer object with name "name".
C     PRTDES (name, ierr)
C        Destroys the printer object with name "name";
C     PRTZAP (name, ierr)
C        Destroys the printer object with name "name";
C        (Same as PRTDES)
C     PRTOPN (name, status, ierr)
C        Opens a printer object, writing header if appropriate
C     PRTCLO (name, ierr)
C        Closes a printer object, spooling output if appropriate
C     PRTGET (name, keywrd, type, dim, value, valuec, ierr)
C        Return keyword value.
C     PRTPUT (name, keywrd, type, dim, value, valuec, ierr)
C        Store keyword value.
C     PRTWRI (name, line, quit, ierr)
C        Send a line to the printer.
C     PRTNUP (name, ierr)
C        Force new page on next write.
C     PRTCNT (name, quit, ierr)
C        For interactive use ask user if he/she wishes to continue.
C
C   Private functions:
C      PRBGET (name, keywrd, type, dim, value, valuec, ierr)
C         Fetches private member.
C      PRBPUT (name, keywrd, type, dim, value, valuec, ierr)
C         Stores private member.
C      PRTUVH (name, ierr)
C         Print uvdata header
C      PRTIMH (name, ierr)
C         Print image header
C      PRTTBH (name, ierr)
C         Print table header
C-----------------------------------------------------------------------
LOCAL INCLUDE 'PRINT.INC'
C                                       Include for CLEAN class.
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER   NACROS, NLINE, IPAGE, PRTLUN, PRTIND
      LOGICAL   ACTIVE
      REAL      DOCRT
      CHARACTER MYNAME*32, POBJ*32, TITLE1*132, TITLE2*132, SCRAT*132
      COMMON /PRTCCM/ DOCRT, ACTIVE, NACROS, NLINE, IPAGE, PRTLUN,
     *   PRTIND
      COMMON /PRTMCC/ MYNAME, POBJ, TITLE1, TITLE2, SCRAT
C                                                          End PRINT.INC
LOCAL END
      SUBROUTINE PRTCRE (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Creates an PRINTER object with name "name"
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'PRINT.INC'
      INTEGER  IDIM(7), DUMMY(1)
      CHARACTER BNAME*32, LPFILE*48, CDUMMY*1
      DATA BNAME /'    '/
      DATA LPFILE /'    '/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Create AIPS object
      CALL OBCREA (NAME, 'PRINTER ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Initilize - ass. obj.
      IDIM(1) = LEN (BNAME)
      IDIM(2) = 1
      IDIM(3) = 0
      CALL PRTPUT (NAME, 'PRINTOBJ', OOACAR, IDIM, DUMMY, BNAME, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Printer file
      IDIM(1) = LEN (LPFILE)
      IDIM(2) = 1
      IDIM(3) = 0
      CALL PRTPUT (NAME, 'LPFILE', OOACAR, IDIM, DUMMY, LPFILE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       printer
      IDIM(1) = 1
      DUMMY(1) = -1
      CALL PRTPUT (NAME, 'DOCRT', OOAINT, IDIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'PRTCRE: ERROR CREATING ' // NAME
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE PRTDES (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the PRINTER object with name "name";
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
      CALL PRTCLO (NAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Delete object
      CALL OBFREE (NAME, IERR)
C
 999  RETURN
      END
      SUBROUTINE PRTZAP (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the PRINTER object with name "name"; same as PRTDES.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'PRINT.INC'
C-----------------------------------------------------------------------
      CALL PRTDES (NAME,IERR)
C
 999  RETURN
      END
      SUBROUTINE PRTOPN (NAME, STATUS, IERR)
C-----------------------------------------------------------------------
C   Public
C   Opens a PRINTER file.  Marks PRINTER common as active.
C   Writes a header for associated object if appropriate.
C   Inputs:
C      NAME   C*?   The name of the object.
C      STATUS C*4   Not used.
C   Output:
C      IERR  I     Error return code, 0=OK, 5=data invalid
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), STATUS*4
      INTEGER   IERR
C
      INCLUDE 'PRINT.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DDCH.INC'
      INTEGER   DIM(7), TYPE, OBJNUM, CLASNO, IDOCRT, DUMMY(1), IROUND
      CHARACTER LPNAME*48, CNAME*8, CDUMMY*1
C-----------------------------------------------------------------------
      IERR = 0
C                                       PRINTER common must be inactive
      IF (ACTIVE) THEN
         IERR = 1
         MSGTXT = 'PRTOPN: ATTEMPT TO ACTIVATE SECOND PRINTER OBJECT'
         GO TO 990
         END IF
      ACTIVE = .TRUE.
C                                       Save name
      MYNAME = NAME
C                                       Open printer
      CALL PRTGET (NAME, 'PRINTOBJ', TYPE, DIM, DUMMY, POBJ, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL PRTGET (NAME, 'LPFILE', TYPE, DIM, DUMMY, LPNAME, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL PRTGET (NAME, 'DOCRT', TYPE, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      IDOCRT = DUMMY(1)
      DOCRT = IDOCRT
C                                       open printer excl
      CALL LPOPEN (LPNAME, DOCRT, PRTLUN, PRTIND, NACROS, SBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) PRTLUN, IERR
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Init page, line count, titles
      NLINE = 990
      IPAGE = 0
      TITLE1 = ' '
      TITLE2 = ' '
C                                       How many columns?
      IDOCRT = IROUND (DOCRT)
      DIM(1) = 1
      DIM(2) = 1
      DUMMY(1) = IDOCRT
      CALL PRTPUT (NAME, 'DOCRT', OOAINT, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      DUMMY(1) = NACROS
      CALL PRTPUT (NAME, 'NACROS', OOAINT, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Print header?
      IF (POBJ.NE.'    ') THEN
C                                       Get object class.
         CALL OBNAME (POBJ, OBJNUM, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL OBCLAS (OBJNUM, CLASNO, CNAME, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         CNAME = '      '
         END IF
      IF (CNAME.EQ.'UVDATA') THEN
C                                       UV data
         CALL PRTUVH (NAME, IERR)
      ELSE IF (CNAME.EQ.'IMAGE') THEN
C                                       Image
         CALL PRTIMH (NAME, IERR)
      ELSE IF (CNAME.EQ.'TABLE') THEN
C                                       Table
         CALL PRTTBH (NAME, IERR)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  MSGTXT = 'PRTOPN: ERROR OPENING ' // NAME
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PRINTER LUN',I3,' OPEN ERR =',I6)
      END
      SUBROUTINE PRTCLO (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Closes PRINTER spooling output to printer if necessary.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'PRINT.INC'
      INCLUDE 'INCS:CLASSIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       No action if inactive
      IF (.NOT.ACTIVE) GO TO 999
C                                       Make sure right one
      IF (NAME.NE.MYNAME) THEN
         IERR = 2
         MSGTXT = 'ATTEMPT TO ACCESS CLOSED PRINT OBJECT'
         GO TO 990
         END IF
C                                       Close print.
      CALL LPCLOS (PRTLUN, PRTIND, NLINE, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Mark as inactive
      ACTIVE = .FALSE.
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  MSGTXT = 'PRTCLO: ERROR CLOSING ' // NAME
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE PRTGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
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
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   OBJNUM
C-----------------------------------------------------------------------
      IERR = 0
C                                       Is this a base class?
      CALL PRBGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       IERR = 1 means not recognized.
      IF (IERR.EQ.1) THEN
C                                       Lookup NAME
         CALL OBNAME (NAME, OBJNUM, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL  OBGET (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
      END
      SUBROUTINE PRTPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
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
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   OBJNUM
C-----------------------------------------------------------------------
      IERR = 0
C                                       Is this a base class?
      CALL PRBPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       IERR = 1 means not recognized.
      IF (IERR.EQ.1) THEN
C                                       Lookup NAME
         CALL OBNAME (NAME, OBJNUM, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OBPUT (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
      END
      SUBROUTINE PRTWRI (NAME, LINE, QUIT, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Writes line to printer
C   Inputs:
C      NAME     C*?   The name of the object.
C      LINE     C*?   Line to be printed
C   Outputs:
C      QUIT     L     If true, user requests that task quit.
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), LINE*(*)
      LOGICAL   QUIT
      INTEGER   IERR
C
      INCLUDE 'PRINT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Check that it's active
      IF (.NOT.ACTIVE) THEN
         IERR = 2
         MSGTXT = 'PRINTER INACTIVE'
         GO TO 990
         END IF
C                                       Make sure right one
      IF (NAME.NE.MYNAME) THEN
         IERR = 2
         MSGTXT = 'ATTEMPT TO ACCESS CLOSED PRINT OBJECT'
         GO TO 990
         END IF
C                                       Print it.
      CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITLE1, TITLE2, LINE,
     *   NLINE, IPAGE, SCRAT, IERR)
C                                       User wants to quit?
      IF (IERR.LT.0) THEN
         QUIT = .TRUE.
         IERR = 0
      ELSE
         QUIT = .FALSE.
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      MSGTXT = 'PRTWRI: ERROR WRITING ' // NAME
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE PRTNUP (NAME, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Force new page on next write.
C   Inputs:
C      NAME     C*?   The name of the object.
C   Outputs:
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'PRINT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Make sure right one
      IF (NAME.NE.MYNAME) THEN
         IERR = 2
         MSGTXT = 'ATTEMPT TO ACCESS CLOSED PRINT OBJECT'
         GO TO 990
         END IF
C                                       Set line number
      NLINE = 999
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      MSGTXT = 'PRTNUP: ERROR WRITING ' // NAME
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE PRTCNT (NAME, QUIT, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Ask interactive user to quit or continue?
C   Inputs:
C      NAME     C*?   The name of the object.
C   Outputs:
C      QUIT     L     If true, user requests that task quit.
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      LOGICAL   QUIT
      INTEGER   IERR
C
      INCLUDE 'PRINT.INC'
C-----------------------------------------------------------------------
      IERR = 0
      NLINE = 1001
C                                       Make sure right one
      IF (NAME.NE.MYNAME) THEN
         IERR = 2
         MSGTXT = 'ATTEMPT TO ACCESS CLOSED PRINT OBJECT'
         GO TO 990
         END IF
C                                       ask
      CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITLE1, TITLE2,
     *   TITLE1, NLINE, IPAGE, SCRAT, IERR)
C                                       User wants to quit?
      IF (IERR.LT.0) THEN
         QUIT = .TRUE.
         IERR = 0
      ELSE
         QUIT = .FALSE.
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      MSGTXT = 'PRTCNT: ERROR WRITING ' // NAME
      CALL MSGWRT (8)
C
 999  RETURN
      END
C
C   Private functions:
C
      SUBROUTINE PRBGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
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
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
C
      INTEGER   POINT, NMEMS, IMEM, LOOP
      PARAMETER (NMEMS = 2)
      CHARACTER MEMBER*16, MEMS(NMEMS)*16
      INCLUDE 'PRINT.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA MEMS /'TITLE1', 'TITLE2'/
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
C                                       Find it?, if not, complain and
C                                       die.
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'PRBGET: UNKNOWN MEMBER: ' // MEMBER // ' IN ' // NAME
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Get from common
      GO TO (110,120), IMEM
C                                       TITLE1
 110     VALUEC(1) = TITLE1
         TYPE = OOACAR
         DIM(1) = LEN (TITLE1)
         DIM(2) = 1
         GO TO 900
C                                       TITLE2
 120     VALUEC(1) = TITLE2
         TYPE = OOACAR
         DIM(1) = LEN (TITLE2)
         DIM(2) = 1
         GO TO 900
 900  IERR = 0
C
 999  RETURN
      END
      SUBROUTINE PRBPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
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
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
C
      INTEGER   POINT, NMEMS, IMEM, LOOP
      PARAMETER (NMEMS = 2)
      CHARACTER MEMBER*16, MEMS(NMEMS)*16
      INCLUDE 'PRINT.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA MEMS /'TITLE1', 'TITLE2'/
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
C                                       Find it?, if not, complain and
C                                       die.
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'PRBPUT: UNKNOWN MEMBER: ' // MEMBER // ' IN ' // NAME
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Save in common
      GO TO (110,120), IMEM
C                                       TITLE1
 110     TITLE1 = VALUEC(1)
         GO TO 900
C                                       TITLE2
 120     TITLE2 = VALUEC(1)
         GO TO 900
 900  IERR = 0
C
 999  RETURN
      END
      SUBROUTINE PRTUVH (NAME, IERR)
C-----------------------------------------------------------------------
C   Private
C   Print uv data header.
C   Inputs:
C      NAME    C*?   The name of the uvdata object.
C   Outputs:
C      IERR    I     Error code, 0=OK.  1=> didn't find.
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER NAME*(*)
C
      INCLUDE 'PRINT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INTEGER   TYPE, DIM(3), SEQ, DISK, INDXF, INDXSU, INDXRA, INDXDC,
     *   LLHM(2), MMHM(2), NCOR, NVIS, BUFNO, DUMMY(1)
      LOGICAL   DOSOU
      REAL      LLSEC, MMSEC
      DOUBLE PRECISION CRVAL(7)
      CHARACTER LINE*132, ONAME*12, CLASS*6, OBJECT*8, LLCH*1, MMCH*1,
     *   CTYPE(7)*8, ISORT*2, OBSDAT*8, ADATE*12, CDUMMY*1
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open and close associated object
C                                       if not OPEN
      CALL OBINFO (POBJ, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (BUFNO.LE.0) THEN
         CALL OUVOPN (POBJ, 'READ', IERR)
         IF (IERR.NE.0) GO TO 995
         CALL OUVCLO (POBJ, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       File info
      CALL FNAGET (POBJ, 'NAME', TYPE, DIM, DUMMY, ONAME, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL FNAGET (POBJ, 'CLASS', TYPE, DIM, DUMMY, CLASS, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL FNAGET (POBJ, 'IMSEQ', TYPE, DIM, SEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL FNAGET (POBJ, 'DISK', TYPE, DIM, DISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      WRITE (LINE,1050) ONAME, CLASS, SEQ, DISK, NLUSER
      CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITLE1, TITLE2, LINE,
     *   NLINE, IPAGE, SCRAT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Coordinates
      CALL UVDGET (POBJ, 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDGET (POBJ, 'CTYPE', TYPE, DIM, DUMMY, CTYPE, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Is source random parameter
C                                       present?
      CALL UVDFND (POBJ, 1, 'SOURCE', INDXSU, IERR)
C                                       May not be there
      DOSOU = (IERR.EQ.0) .AND. (INDXSU.GT.0)
      IERR = 0
C                                       Single source
      IF (.NOT.DOSOU) THEN
C                                       Need RA axis
         CALL UVDFND (POBJ, 2, 'RA', INDXRA, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'TROUBLE FINDING RA AXIS'
            GO TO 990
            END IF
C                                       Need MM axis
         CALL UVDFND (POBJ, 2, 'DEC', INDXDC, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'TROUBLE FINDING DEC AXIS'
            GO TO 990
            END IF
C                                       Get various information
         CALL UVDGET (POBJ, 'OBJECT', TYPE, DIM, DUMMY, OBJECT, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Crack position
         CALL COORDD (1, CRVAL(INDXRA), LLCH, LLHM, LLSEC)
         CALL COORDD (2, CRVAL(INDXDC), MMCH, MMHM, MMSEC)
C                                       Source info
         WRITE (LINE,1055) OBJECT, CTYPE(INDXRA), LLCH, LLHM, LLSEC,
     *      CTYPE(INDXDC), MMCH, MMHM, MMSEC
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITLE1, TITLE2,
     *      LINE, NLINE, IPAGE, SCRAT, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
C                                       Multi source - give obs. date
         CALL UVDGET (POBJ, 'DATE-OBS', TYPE, DIM, DUMMY, OBSDAT, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL DATDAT (OBSDAT, ADATE)
         LINE = 'Multisource uv data file observed on ' // ADATE
         CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITLE1, TITLE2,
     *      LINE, NLINE, IPAGE, SCRAT, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Other info
C                                       Need FREQ axis
      CALL UVDFND (POBJ, 2, 'FREQ', INDXF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING FREQ AXIS'
         GO TO 990
         END IF
      CALL UVDGET (POBJ, 'NCORR', TYPE, DIM, NCOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDGET (POBJ, 'GCOUNT', TYPE, DIM, NVIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDGET (POBJ, 'SORTORD', TYPE, DIM, DUMMY, ISORT, IERR)
      IF (IERR.NE.0) GO TO 995
      WRITE (LINE,1060) CRVAL(INDXF)*1.0D-9, NCOR, NVIS, ISORT
      CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITLE1, TITLE2, LINE,
     *   NLINE, IPAGE, SCRAT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Blank line
      LINE = ' '
      CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITLE1, TITLE2, LINE,
     *   NLINE, IPAGE, SCRAT, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  MSGTXT = 'PRTUVH: ERROR WRITING HEADER FOR ' // POBJ
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('File = ',A12,'.',A6,'.',I4,'   Vol =',I2,4X,'Userid =',
     *   I5)
 1055 FORMAT ('Source= ',A,3X,A,' = ',A1,I2.2,I3.2,F6.2,3X,A,' = ',
     *   A1,I2.2,I3.2,F5.1)
 1060 FORMAT ('Freq=',F13.9,' GHz   Ncor=',I3,'   No. vis=',I10,
     *   '   Sort order= ',A2)
      END
      SUBROUTINE PRTIMH (NAME, IERR)
C-----------------------------------------------------------------------
C   Private
C   Print image header.
C   Inputs:
C      NAME    C*?   The name of the image object.
C   Outputs:
C      IERR    I     Error code, 0=OK.  1=> didn't find.
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER NAME*(*)
C
      INCLUDE 'PRINT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INTEGER   TYPE, DIM(3), SEQ, DISK, INDXF, INDXRA, INDXDC,
     *   LLHM(2), MMHM(2), BUFNO, DUMMY(1)
      REAL      LLSEC, MMSEC
      DOUBLE PRECISION CRVAL(7)
      CHARACTER LINE*132, ONAME*12, CLASS*6, OBJECT*8, LLCH*1, MMCH*1,
     *   CTYPE(7)*8, CDUMMY*1
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open and close associated object
C                                       if not OPEN
      CALL OBINFO (POBJ, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (BUFNO.LE.0) THEN
         CALL IMGOPN (POBJ, 'READ', IERR)
         IF (IERR.NE.0) GO TO 995
         CALL IMGCLO (POBJ, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       File info
      CALL FNAGET (POBJ, 'NAME', TYPE, DIM, DUMMY, ONAME, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL FNAGET (POBJ, 'CLASS', TYPE, DIM, DUMMY, CLASS, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL FNAGET (POBJ, 'IMSEQ', TYPE, DIM, SEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL FNAGET (POBJ, 'DISK', TYPE, DIM, DISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      WRITE (LINE,1050) ONAME, CLASS, SEQ, DISK, NLUSER
      CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITLE1, TITLE2, LINE,
     *   NLINE, IPAGE, SCRAT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Coordinates
      CALL IMGET (POBJ, 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL IMGET (POBJ, 'CTYPE', TYPE, DIM, DUMMY, CTYPE, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Need RA axis
      CALL UVDFND (POBJ, 2, 'RA--', INDXRA, IERR)
      IF ((IERR.EQ.0) .AND. (INDXRA.GT.0)) THEN
         CALL COORDD (1, CRVAL(INDXRA), LLCH, LLHM, LLSEC)
      ELSE
         CALL COORDD (1, 0.0D0, LLCH, LLHM, LLSEC)
         END IF
C                                       Need Dec axis
      CALL UVDFND (POBJ, 2, 'DEC-', INDXDC, IERR)
      IF ((IERR.EQ.0) .AND. (INDXDC.GT.0)) THEN
         CALL COORDD (2, CRVAL(INDXDC), MMCH, MMHM, MMSEC)
      ELSE
         CALL COORDD (2, 0.0D0, MMCH, MMHM, MMSEC)
         END IF
C                                       Get various information
      CALL IMGET (POBJ, 'OBJECT', TYPE, DIM, DUMMY, OBJECT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Source info
      WRITE (LINE,1055) OBJECT, CTYPE(INDXRA), LLCH, LLHM, LLSEC,
     *   CTYPE(INDXDC), MMCH, MMHM, MMSEC
      CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITLE1, TITLE2, LINE,
     *   NLINE, IPAGE, SCRAT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Other info
C                                       Need FREQ axis
      CALL UVDFND (POBJ, 2, 'FREQ', INDXF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING FREQ AXIS'
         GO TO 990
         END IF
      WRITE (LINE,1060) CRVAL(INDXF)*1.0D-9
      CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITLE1, TITLE2, LINE,
     *   NLINE, IPAGE, SCRAT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Blank line
      LINE = ' '
      CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITLE1, TITLE2, LINE,
     *   NLINE, IPAGE, SCRAT, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  MSGTXT = 'PRTIMH: ERROR WRITING HEADER FOR ' // POBJ
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('File = ',A12,'.',A6,'.',I4,'   Vol =',I2,4X,'Userid =',
     *   I5)
 1055 FORMAT ('Source= ',A,3X,A,' = ',A1,I2.2,I3.2,F6.2,3X,A,' = ',
     *   A1,I2.2,I3.2,F5.1)
 1060 FORMAT ('Freq=',F13.9,' GHz ')
      END
      SUBROUTINE PRTTBH (NAME, IERR)
C-----------------------------------------------------------------------
C   Private
C   Print table header.
C   Inputs:
C      NAME    C*?   The name of the table object.
C   Outputs:
C      IERR    I     Error code, 0=OK.  1=> didn't find.
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER NAME*(*)
C
      INCLUDE 'PRINT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INTEGER   TYPE, DIM(3), SEQ, DISK, BUFNO, TDISK, TCNO, TVER, NROW,
     *   DUMMY(1)
      CHARACTER LINE*132, ONAME*12, CLASS*6, TBTYPE*2, TLABEL*56,
     *   CDUMMY*1
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open and close associated object
C                                       if not OPEN
      CALL OBINFO (POBJ, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (BUFNO.LE.0) THEN
         CALL IMGOPN (POBJ, 'READ', IERR)
         IF (IERR.NE.0) GO TO 995
         CALL IMGCLO (POBJ, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       File info
      CALL FNAGET (POBJ, 'NAME', TYPE, DIM, DUMMY, ONAME, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL FNAGET (POBJ, 'CLASS', TYPE, DIM, DUMMY, CLASS, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL FNAGET (POBJ, 'IMSEQ', TYPE, DIM, SEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL FNAGET (POBJ, 'DISK', TYPE, DIM, DISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      WRITE (LINE,1050) ONAME, CLASS, SEQ, DISK, NLUSER
      CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITLE1, TITLE2, LINE,
     *   NLINE, IPAGE, SCRAT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Table label
      CALL TABGET (POBJ, 'LABEL', TYPE, DIM, DUMMY, TLABEL, IERR)
      IF (IERR.NE.0) GO TO 995
      LINE = TLABEL
      CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITLE1, TITLE2, LINE,
     *   NLINE, IPAGE, SCRAT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Table type
      CALL TBLKUP (POBJ, TDISK, TCNO, TBTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get number of rows
      CALL TABGET (POBJ, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (LINE,1065) TBTYPE, TVER, NROW
      CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITLE1, TITLE2, LINE,
     *   NLINE, IPAGE, SCRAT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Blank line
      LINE = ' '
      CALL PRTLIN (PRTLUN, PRTIND, DOCRT, NACROS, TITLE1, TITLE2, LINE,
     *   NLINE, IPAGE, SCRAT, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  MSGTXT = 'PRTTBH: ERROR WRITING HEADER FOR ' // POBJ
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('File = ',A12,'.',A6,'.',I4,'   Vol =',I2,4X,'Userid =',
     *   I5)
 1065 FORMAT ('Table of type ', A2, ', version ',I3,' contains ',I8,
     *   ' rows')
      END
