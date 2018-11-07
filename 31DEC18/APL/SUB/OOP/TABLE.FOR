C   Table Class module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "TABLE" class library
C# Ext-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999, 2011, 2015
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
C   Most table access is through TABGET and TABPUT  but rapid access to
C   row data is possible through TABDGT and TABDPT.
C
C   Class public members:
C     NAME      C*12     Catalog file name
C     CLASS     C*6      Catalog file class
C     SEQ       I        Catalog file sequence number
C     DISK      I        Disk number
C     TBLTYPE   C*2      Table type
C     VER       I        Version number
C   The following must be set before a new table is opened and are
C   unavailable before an existing table is opened.
C   state:
C     LABEL     C*56     Table label
C     NCOL      I        Number of columns
C     COLABEL   C(*)*24  Column labels
C     COLUNIT   C(*)*8   Column units
C     COLTYPE   I(*)     Column data type: 1=double, 2=real,
C                        3=character, 4=integer, 5=logical, 7=bit
C                        arrays.
C     COLDIM    I(*)     Column dimension.
C   The following are available only when the table is open.
C     NROW      I        Number of rows
C     SORT      I(2)     Sort order
C     CURROW    I        Current row number, if negative it has not yet
C                        been read.
C     ENTRY.nn  ?(?)     table entry for column number nn
C     KEY.xxxx  ?        table keyword/value pair for keyword xxxx
C
C   Class private members:
C     TBNCOL    I(MAXIO)      Number of columns, per I/O stream.
C     TBCROW    I(MAXIO)      Current row number, per I/O stream.
C     TBTYPE    I(128,MAXIO)  Column type codes, one set per I/O stream
C     TBDIM     I(128,MAXIO)  Column element count
C     TBPTR     I(128,MAXIO)  Column pointer to first element in array
C                             of type.
C     RECORD    I(2048,MAXIO) Record buffer per I/O stream.
C                             Equivalenced to RECR, RECD, RECH and, RECL
C                             for real, double, Hollerith and logical.
C
C   Public functions:
C     TABCRE (name, iret)
C        Creates a table object.
C     TABDES (name, ierr)
C        Destroys the table object with name "name"; quasi-permanent
C        forms are unaffected.
C     TABZAP (name, ierr)
C        Destroys the table object with name "name"; quasi-permanent
C        forms are deleted.
C     TABRMV (name, ierr)
C        Removes any underlying AIPS files leaves object intact.
C     TBCOPY (namein, namout, ierr)
C        Copys one object to another.  The same quasi permanent forms
C        are used for both.
C     TBLCOP (namein, namout, ierr)
C        Copys contents of one table object to another.  (Standard AIPS
C        TABCOP)
C     TABCLN (namein, namout, ierr)
C        CLONES an object.  A new table is created.
C     TABOPN (name, status, ierr)
C        Opens a table for access.
C     TABCLO (name, ierr)
C        Closes a table for access.
C     TABCOL (name, ncol, colab, colnum, ierr)
C        Returns column numbers for a list of column labels.
C     TABGET (name, keywrd, type, dim, value, valuec, ierr)
C        Fetches table member
C     TABPUT (name, keywrd, type, dim, value, valuec, ierr)
C        Stores table member
C     TABDGT (name, row, col, type, dim, value, valuec, ierr)
C        Fetches table row data
C     TABDPT (name, row, col, type, dim, value, valuec, ierr)
C        Stores table row data
C     TABKGT (name, keys, nkeys, klocs, kvals, ktype, ierr)
C        Fetches values of specified table keywords.  If keys(1) is
C        blank then all keywords up to a maximum of nkeys is read.  On
C        return nkeys is the number read.
C     TABKPT (name, keys, nkeys, klocs, kvals, ktype, ierr)
C        Stores values of specified table keywords.
C     TBLSRT (name, key1, key2, ierr)
C        Sorts a table using a 2 key sort.
C     TBLMRG (name, mkol, toler, skol, nskol, srtkol, ierr)
C        Merge a table.
C     TABEXI (name, exists, ierr)
C        Check whether the table exists.
C
C   Private functions:
C     TBLMEM (keywrd, mem, arg, local, ierr)
C        Parses keyword into components
C     TBLKUP (name, tdisk, tcno, ttype, tver, ierr)
C        Looks up information about table object.
C     TBLHIV (name, nver, ierr)
C        Returns highest table version number.
C-----------------------------------------------------------------------
LOCAL INCLUDE 'TABLE.INC'
C                                       Table class
      INTEGER   NMEM
C                                       NMEM = .no. base classes.
      PARAMETER (NMEM = 5)
      CHARACTER LMEM(NMEM)*8, THSCLS*16
      DATA LMEM /'CURROW', 'KEY', 'ENTRY', 'NROW', 'SORT'/
      DATA THSCLS /'TABLE'/
LOCAL END
LOCAL INCLUDE 'TABLEIO.INC'
C                                       Table class I/O
C                                       Requires OBJPARM.INC,CLASSIO.INC
C                                       table control block last 512 of
C                                       each OBUFFR
      INTEGER   TBNCOL(MAXIO), TBCROW(MAXIO), TBTYPE(128,MAXIO),
     *   TBDIM(128,MAXIO), TBPRT(128,MAXIO), RECORD(BUFSIZ,MAXIO),
     *   IBUFFR(BUFSIZ,MAXIO)
C                                       Have to address double in real
C                                       array and copy.
      REAL      RECR(BUFSIZ,MAXIO), RECD(BUFSIZ,MAXIO)
      HOLLERITH RECH(BUFSIZ,MAXIO)
      LOGICAL   RECL(BUFSIZ,MAXIO)
      CHARACTER TABSTR*(MAXSIZ/2)
      COMMON /TBLIOC/ TBNCOL, TBCROW, TBTYPE, TBDIM, TBPRT
      COMMON /TBLCOC/ TABSTR
      EQUIVALENCE (OBUFFR, IBUFFR, RECORD, RECR, RECD, RECH, RECL)
LOCAL END
      SUBROUTINE TABCRE (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Creates an table object with name "name"
C   Simple return if object exists.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   OBJNUM, MSGSAV
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Does it exist already?
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OBNAME (NAME, OBJNUM, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.0) GO TO 999
C                                       Create AIPS object
      CALL OBCREA (NAME, 'TABLE', IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE TABDES (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the table object with name "name"; quasi-permanent forms
C   are unaffected.
C   Inputs:
C      NAME  C*32  The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
C-----------------------------------------------------------------------
      IERR = 0
C                                       Destroy object
      CALL OBFREE (NAME, IERR)
C
 999  RETURN
      END
      SUBROUTINE TABZAP (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the table object with name "name"; quasi-permanent forms
C   are deleted.
C   Inputs:
C      NAME  C*32  The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   TCNO, TDISK, TVER
      CHARACTER TTYPE*2, FNAME*48, STAT*4
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Look up table
      CALL TBLKUP (NAME, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Destroy table
      CALL ZPHFIL (TTYPE, TDISK, TCNO, TVER, FNAME, IERR)
      CALL ZDESTR (TDISK, FNAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Remove from AIPS catalog entry
      STAT = 'WRWR'
      CALL DELEXT (TTYPE, TDISK, TCNO, STAT, SBUFF, SBUFF(257), TVER,
     *   IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.3)) GO TO 999
C                                       Destroy object
      CALL TABDES (NAME, IERR)
C
 999  RETURN
      END
      SUBROUTINE TABRMV (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Removed any underlying AIPS files but leaves the object intact.
C   If VER = 0 then the highest numbered table is deleted.
C   If VER < 0 then all versions are deleted.
C   Inputs:
C      NAME  C*32  The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   TCNO, TDISK, TVER, NVER, IVER, LOWVER, HIVER
      LOGICAL   EXIST
      CHARACTER TTYPE*2, FNAME*48, STAT*4
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Look up table
      CALL TBLKUP (NAME, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Highest version number?
      CALL TBLHIV (NAME, NVER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Specific table?
      IF (TVER.GT.0) THEN
C                                       Does it exist?
         CALL OBFEXS (NAME, EXIST, IERR)
         IERR = 0
         IF (.NOT.EXIST) GO TO 999
         LOWVER = TVER
         HIVER = TVER
C                                       Highest
      ELSE IF (TVER.EQ.0) THEN
         LOWVER = MAX (NVER, 1)
         HIVER = NVER
C                                       All
      ELSE
         LOWVER = 1
         HIVER = NVER
         END IF
C                                       Loop as necessary deleting tables
      DO 100 IVER = LOWVER, HIVER
C                                       Destroy table
         CALL ZPHFIL (TTYPE, TDISK, TCNO, IVER, FNAME, IERR)
         CALL ZDESTR (TDISK, FNAME, IERR)
         IF (IERR.GT.1) THEN
            IERR = 0
            GO TO 100
            END IF
         IERR = 0
C                                       Remove from AIPS catalog entry
         STAT = 'WRWR'
         CALL DELEXT (TTYPE, TDISK, TCNO, STAT, SBUFF, SBUFF(257), IVER,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE TBCOPY (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Copys one object to another.  The same quasi permanent forms are
C   used for both.  Only copies relevant naming info.
C   Inputs:
C      NAMEIN  C*?   The name of the input object.
C      NAMOUT  C*?   The name of the output object.
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAMEIN*(*), NAMOUT*(*)
      INTEGER   IERR
C
      INTEGER   OBJIN, OBJOUT, TYPE, DIM(3), TDISK, TSEQ, TVER, DUMMY
      CHARACTER TNAME*12, TCLASS*6, TTYPE*2, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Create output
      CALL TABCRE (NAMOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Find object numbers
      CALL OBNAME (NAMEIN, OBJIN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBNAME (NAMOUT, OBJOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy relevant info
      CALL OBGET (OBJIN, 'NAME', TYPE, DIM, DUMMY, TNAME, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBPUT (OBJOUT, 'NAME', OOACAR, DIM, DUMMY, TNAME, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBGET (OBJIN, 'CLASS', TYPE, DIM, DUMMY, TCLASS, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBPUT (OBJOUT, 'CLASS', OOACAR, DIM, DUMMY, TCLASS, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBGET (OBJIN, 'IMSEQ', TYPE, DIM, TSEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBPUT (OBJOUT, 'IMSEQ', OOAINT, DIM, TSEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBGET (OBJIN, 'DISK', TYPE, DIM, TDISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBPUT (OBJOUT, 'DISK', OOAINT, DIM, TDISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBGET (OBJIN, 'TBLTYPE', TYPE, DIM, DUMMY, TTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBPUT (OBJOUT, 'TBLTYPE', OOACAR, DIM, DUMMY, TTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBGET (OBJIN, 'VER', TYPE, DIM, TVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBPUT (OBJOUT, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE TBLCOP (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Copys the contents of one table object to another.
C   Destroys any previous version of the underlying files of NAMOUT.
C   Note: no error is returned if input table doesn't  exist.
C   Inputs:
C      NAMEIN  C*?   The name of the input object.
C      NAMOUT  C*?   The name of the output object.
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAMEIN*(*), NAMOUT*(*)
      INTEGER   IERR
C
      INTEGER   LUNI, LUNO, DISKI, DISKO, CNOI, CNOO, BUFIN, BUFOUT,
     *   VERI, VERO
      LOGICAL   EXIST
      CHARACTER TBTYPE*2
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open object for buffer
      CALL OBOPEN (NAMEIN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBOPEN (NAMOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (NAMEIN, BUFIN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBINFO (NAMOUT, BUFOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get other info
      CALL TBLKUP (NAMEIN, DISKI, CNOI, TBTYPE, VERI, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TBLKUP (NAMOUT, DISKO, CNOO, TBTYPE, VERO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Make sure output doesn't exist
      CALL OBFEXS (NAMOUT, EXIST, IERR)
      IERR = 0
      IF (EXIST) THEN
         CALL TABRMV (NAMOUT, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Catalog header
      CALL OBHGET (NAMOUT, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Assign LUNs
      CALL OBLUN (LUNI, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBLUN (LUNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy table
      CALL TABCOP (TBTYPE, VERI, VERO, LUNI, LUNO, DISKI, DISKO, CNOI,
     *   CNOO, CATBLK, OBUFFR(1,BUFIN), OBUFFR(1,BUFOUT), IERR)
C                                       OK if no input files
      IF (IERR.EQ.2) IERR = 0
      IF (IERR.NE.0) GO TO 999
C                                       Save catalog header
      CALL OBHPUT (NAMOUT, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close object
      CALL OBCLOS (NAMEIN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBCLOS (NAMOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUNI)
      IF (IERR.NE.0) GO TO 999
      CALL OBLUFR (LUNO)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE TABCLN (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Clones an table object.  The output name, class, seq, disk, ver are
C   given by the keywords OUTNAME, OUTCLASS, OUTSEQ, OUTDISK, OUTVER.
C   Inputs:
C      NAMEIN  C*?   The name of the input object.
C      NAMOUT  C*?   The name of the output object.
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAMEIN*(*), NAMOUT*(*)
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(7), TDISK, TSEQ, TVER, OBJNUM, DUMMY
      CHARACTER TNAME*12, TCLASS*6, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open and close input table to be
C                                       sure that all info there.
      CALL TABOPN (NAMEIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABCLO (NAMEIN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy Object
      CALL TBCOPY (NAMEIN, NAMOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Find object number
      CALL OBNAME (NAMOUT, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Update name etc.
      CALL OBGET (OBJNUM, 'OUTNAME', TYPE, DIM, DUMMY, TNAME, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBPUT (OBJNUM, 'NAME', OOACAR, DIM, DUMMY, TNAME, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBGET (OBJNUM, 'OUTCLASS', TYPE, DIM, DUMMY, TCLASS, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBPUT (OBJNUM, 'CLASS', OOACAR, DIM, DUMMY, TCLASS, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBGET (OBJNUM, 'OUTSEQ', TYPE, DIM, TSEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBPUT (OBJNUM, 'IMSEQ', OOAINT, DIM, TSEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBGET (OBJNUM, 'OUTDISK', TYPE, DIM, TDISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBPUT (OBJNUM, 'DISK', OOAINT, DIM, TDISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBGET (OBJNUM, 'OUTVER', TYPE, DIM, TVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBPUT (OBJNUM, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open and close input table to
C                                       force table creation.
      CALL TABOPN (NAMOUT, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABCLO (NAMOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy table
C
 999  RETURN
      END
      SUBROUTINE TABOPN (NAME, STATUS, IERR)
C-----------------------------------------------------------------------
C   Public
C   Sets up to open a table file.  Obtains header info etc.
C   Inputs:
C      NAME   C*32  The name of the object.
C      STATUS C*4   'READ', 'WRIT' (mixed reads and writes)
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), STATUS*4
      INTEGER   IERR
C
      INTEGER   TDISK, TCNO, TVER, LUN, BUFNO, DATP(128,2), LOOP, NKEY,
     *   NCOL, NREC, OFF, POINT, TYPE, DIM(7), DUMMY(1)
      LOGICAL   TABLE, EXIST, FITASC
      HOLLERITH HTEMP(6)
      CHARACTER TTYPE*2, STAT*4, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'TABLEIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table
      CALL TBLKUP (NAME, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Does it exist?
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ISTAB (TTYPE, TDISK, TCNO, TVER, LUN, SBUFF, TABLE, EXIST,
     *   FITASC, IERR)
      EXIST = EXIST .AND. (IERR.EQ.0)
      IERR = 0
C                                       If TVER=0 then hope for the best
      IF (TVER.LE.0) THEN
         EXIST = STATUS.EQ.'READ'
         TABLE = .TRUE.
         END IF
C                                       Table must exist for read
      IF ((.NOT.EXIST) .AND. (STATUS.EQ.'READ')) THEN
         IERR = 2
         WRITE (MSGTXT,1000) TTYPE, TVER
         GO TO 990
         END IF
C                                       Error if it's not a table file.
      IF (.NOT.TABLE) THEN
         IERR = 2
         WRITE (MSGTXT,1001) TTYPE, TVER
         GO TO 990
         END IF
C                                       Setup for TABINI call
C                                       Get CATBLK
      STAT = 'REST'
      CALL CATIO ('READ', TDISK, TCNO, CATBLK, STAT, SBUFF, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
         WRITE (MSGTXT,1002) IERR
         GO TO 990
         END IF
      IERR = 0
C                                       Open object
      CALL OBOPEN (NAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Writing new table?
      IF ((.NOT.EXIST) .AND. (STATUS.EQ.'WRIT')) THEN
C                                       Default 20 keywords
         NKEY = 20
         NREC = 100
C                                       Number of columns
         CALL TABGET (NAME, 'NCOL', TYPE, DIM, DUMMY, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         NCOL = DUMMY(1)
C                                       Element count
         CALL TABGET (NAME, 'COLTYPE', TYPE, DIM, TBTYPE(1,BUFNO),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Type
         CALL TABGET (NAME, 'COLDIM', TYPE, DIM, TBDIM(1,BUFNO), CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Fill DATP
         DO 100 LOOP = 1,NCOL
            DATP(LOOP,2) = TBTYPE(LOOP,BUFNO) + 10 * TBDIM(LOOP,BUFNO)
 100        CONTINUE
      ELSE
C                                       Existing table
         NKEY = 0
         NREC = 0
         NCOL = 0
         END IF
C                                       Call TABINI to create/open
      CALL TABINI (STATUS, TTYPE, TDISK, TCNO, TVER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.GT.0) GO TO 999
C                                       If just created fill in info.
      IF (IERR.LT.0) THEN
         IERR = 0
C                                       Table name
         CALL TABGET (NAME, 'LABEL', TYPE, DIM, DUMMY, TABSTR, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL CHR2H (56, TABSTR, 1, OBUFFR(OFF+100,BUFNO))
C                                       Column labels
         CALL TABGET (NAME, 'COLABEL', TYPE, DIM, DUMMY, TABSTR, IERR)
         IF (IERR.NE.0) GO TO 999
         DO 120 LOOP = 1,NCOL
            POINT = (LOOP-1) * DIM(1) + 1
            CALL CHR2H (DIM, TABSTR(POINT:POINT+DIM(1)-1), 1, HTEMP)
            CALL TABIO ('WRIT', 3, LOOP, HTEMP, OBUFFR(OFF,BUFNO), IERR)
            IF (IERR.NE.0) GO TO 990
 120        CONTINUE
C                                       Column Units
         CALL TABGET (NAME, 'COLUNIT', TYPE, DIM, DUMMY, TABSTR, IERR)
         IF (IERR.NE.0) GO TO 999
         DO 140 LOOP = 1,NCOL
            POINT = (LOOP-1) *DIM(1) + 1
            CALL CHR2H (DIM, TABSTR(POINT:POINT+DIM(1)-1), 1, HTEMP)
            CALL TABIO ('WRIT', 4, LOOP, HTEMP, OBUFFR(OFF,BUFNO), IERR)
            IF (IERR.NE.0) GO TO 990
 140        CONTINUE
         END IF
C                                       Save CATBLK
      CALL OBHPUT (NAME, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Save local information.
C                                       Save disk, ver in case defaults
C                                       passed.
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      DUMMY(1) = TDISK
      CALL TABPUT (NAME, 'DISK', OOAINT, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DUMMY(1) = TVER
      CALL TABPUT (NAME, 'VER', OOAINT, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Interprete DATP
      DO 200 LOOP = 1,NCOL
         TBPRT(LOOP,BUFNO) = DATP(LOOP,1)
         TBDIM(LOOP,BUFNO) = DATP(LOOP,2) / 10
         TBTYPE(LOOP,BUFNO) = DATP(LOOP,2) - TBDIM(LOOP,BUFNO) * 10
 200     CONTINUE
C                                       Number of columns
      DIM(1) = 1
      DIM(2) = 1
      DUMMY(1) = NCOL
      CALL TABPUT (NAME, 'NCOL', OOAINT, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      TBNCOL(BUFNO) = NCOL
C                                       Current row
      TBCROW(BUFNO) = -1
C                                       Element count
      DIM(1) = NCOL
      CALL TABPUT (NAME, 'COLTYPE', OOAINT, DIM, TBTYPE(1,BUFNO),
     *   CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Type
      CALL TABPUT (NAME, 'COLDIM', OOAINT, DIM, TBDIM(1,BUFNO), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Table name
      DIM(1) = 56
      TYPE = 3
      CALL H2CHR (DIM(1), 1, OBUFFR(OFF+100,BUFNO), TABSTR)
      CALL TABPUT (NAME, 'LABEL', OOACAR, DIM, DUMMY, TABSTR, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Column labels
      DIM(1) = 24
      DIM(2) = NCOL
      DIM(3) = 0
      TABSTR = '  '
      DO 220 LOOP = 1,NCOL
         POINT = (LOOP-1) * DIM(1) + 1
         CALL TABIO ('READ', 3, LOOP, HTEMP, OBUFFR(OFF,BUFNO), IERR)
         IF (IERR.NE.0) GO TO 990
         CALL H2CHR (DIM, 1, HTEMP, TABSTR(POINT:POINT+DIM(1)-1))
 220     CONTINUE
      CALL TABPUT (NAME, 'COLABEL', OOACAR, DIM, DUMMY, TABSTR, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Column Units
      DIM(1) = 8
      TABSTR = '  '
      DO 240 LOOP = 1,NCOL
         POINT = (LOOP-1) * DIM(1) + 1
         CALL TABIO ('READ', 4, LOOP, HTEMP, OBUFFR(OFF,BUFNO), IERR)
         IF (IERR.NE.0) GO TO 990
         CALL H2CHR (DIM, 1, HTEMP, TABSTR(POINT:POINT+DIM(1)-1))
 240     CONTINUE
      CALL TABPUT (NAME, 'COLUNIT', OOACAR, DIM, DUMMY, TABSTR, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Remove LUN from reserved list
      CALL OBLUFR (LUN)
      GO TO 999
C                                       Error
 990     CALL MSGWRT (7)
         MSGTXT = 'PROBLEM OPENING TABLE OBJECT: ' // NAME
         CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TABLE TYPE ',A,' VERSION ', I3,' DOES NOT EXIST')
 1001 FORMAT ('EXTENSION TYPE ',A,' VERSION ', I3,' IS NOT A TABLE')
 1002 FORMAT ('ERROR ',I3,' READING CATBLK')
      END
      SUBROUTINE TABCLO (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Closes table updating disk resident information.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   BUFNO, OFF
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TABLEIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Write old record if necessary
      IF ((IBUFFR(OFF+70,BUFNO).EQ.2) .AND. (TBCROW(BUFNO).GT.0)) THEN
         CALL TABIO ('WRIT', 0, TBCROW(BUFNO), RECORD(1,BUFNO),
     *      IBUFFR(OFF,BUFNO), IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Close table
      CALL TABIO ('CLOS', 0, TBCROW(BUFNO), RECORD(1,BUFNO),
     *   IBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object
      CALL OBCLOS (NAME, IERR)
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'TABCLO: ERROR CLOSING ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE TABCOL (NAME, NCOL, COLAB, COLNUM, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Looks up column numbers.  This routine calls FNDCOL which is rather
C   dangerous,  Do not call after starting I/O to the table.
C   Inputs:
C      NAME     C*?   The name of the object.
C      NCOL     I     The number of columns
C      COLAB    C(?)*? Column labels will check the number of characters
C                      passed.
C   Outputs:
C      COLNUM   I(*)  column numbers
C      IERR     I     Error return code, 0=OK, else FNDCOL errors
C-----------------------------------------------------------------------
      INTEGER   NCOL, COLNUM(NCOL), IERR
      CHARACTER NAME*(*), COLAB(NCOL)*(*)
C
      INTEGER   NCHK, OFF, BUFNO
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'TABLEIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Number of characters to check.
      NCHK = LEN (COLAB(1))
C                                       Get buffer number
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = NAME // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                       Look up numbers
      CALL FNDCOL (NCOL, COLAB, NCHK, .TRUE., IBUFFR(OFF,BUFNO), COLNUM,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE TABGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Returns a table member.
C   Inputs:
C      NAME     C*?   The name of the object.
C      KEYWRD   C*?   The name of the keyword in form 'MEM1.MEM2...'
C      TYPE     I     Data type of VALUE: 1=double, 2=real,
C                        3=character, 4=integer, 5=logical, 7=bit
C                        arrays.
C      DIM      I(*)  Dimensionality of value, an axis dimension of zero
C                     means that that dimension and higher are
C                     undefined.
C      VALUE    ?(*)  The value associated with keyword.
C      VALUEC   C*(*) Character value associated with keyword.
C   Outputs:
C      IERR     I     Error return code, 0=OK, -1 => table row
C                     deselected.
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   KEYVAL(2), NKEY, LOCS, COL, ROW, NCOL, OFF, BUFNO,
     *   OBJNUM
      CHARACTER MEM*8, ARG*8
      LOGICAL LOCAL, KEYVL(2)
      HOLLERITH KEYVH(2)
      REAL      KEYVR(2)
      DOUBLE PRECISION KEYVD
      EQUIVALENCE (KEYVAL, KEYVR, KEYVH, KEYVL, KEYVD)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'TABLEIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look for base class.member name
C                                       in KEYWRD.
      CALL TBLMEM (KEYWRD, MEM, ARG, LOCAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Deal with keywrd here?
      IF (LOCAL) THEN
C                                       Get buffer number
         CALL OBINFO (NAME, BUFNO, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Table open?
         IF (BUFNO.LE.0) THEN
            MSGTXT = NAME // ' TABLE IS NOT OPEN'
            CALL MSGWRT (6)
            IERR = 5
            GO TO 999
            END IF
         NCOL = TBNCOL(BUFNO)
         TYPE = 4
         DIM(2) = 1
         DIM(3) = 0
C                                       Current row number
         IF (MEM.EQ.'CURROW') THEN
            DIM(1) = 1
            VALUE(1) = ABS (TBCROW(BUFNO))
C                                       Number of rows
         ELSE IF (MEM.EQ.'NROW') THEN
            DIM(1) = 1
            VALUE(1) = IBUFFR(OFF+4,BUFNO)
C
         ELSE IF (MEM.EQ.'SORT') THEN
            DIM(1) = 2
            VALUE(1) = IBUFFR(OFF+42,BUFNO)
            VALUE(2) = IBUFFR(OFF+43,BUFNO)
C                                       Table Keyword
         ELSE IF (MEM.EQ.'KEY') THEN
            DIM(1) = 1
            NKEY = 1
            CALL TABKEY ('READ', ARG, NKEY, IBUFFR(OFF,BUFNO), LOCS,
     *         KEYVAL, TYPE, IERR)
            IF (TYPE.EQ.1) THEN
               CALL COPY (NWDPDP, KEYVD, VALUE)
            ELSE IF (TYPE.EQ.2) THEN
               CALL COPY (DIM(1), KEYVR, VALUE)
            ELSE IF (TYPE.EQ.3) THEN
               DIM(1) = 8
               CALL H2CHR (DIM, 1, KEYVH, VALUEC)
            ELSE IF (TYPE.EQ.4) THEN
               VALUE(1) = KEYVAL(1)
            ELSE IF (TYPE.EQ.5) THEN
               CALL COPY (DIM(1), KEYVL, VALUE)
               END IF
C                                       Table data entry
         ELSE IF (MEM.EQ.'ENTRY') THEN
C                                       Decode column number from ARG
            READ (ARG,1000,ERR=500) COL
            ROW = ABS (TBCROW(BUFNO))
            CALL TABDGT (NAME, ROW, COL, TYPE, DIM, VALUE, VALUEC, IERR)
            GO TO 999
            END IF
      ELSE
C                                       Find object number
         CALL OBNAME (NAME, OBJNUM, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OBGET (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
         END IF
      GO TO 999
C                                       Error decoding column number
 500  WRITE(MSGTXT,1500) KEYWRD
      CALL MSGWRT (7)
      MSGTXT = 'ERROR FOR TABLE OBJECT ' // NAME
      CALL MSGWRT (7)
      IERR = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I8)
 1500 FORMAT ('ERROR PARSING COLUMN NUMBER FROM ',A)
      END
      SUBROUTINE TABPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Stores a table member.
C   Inputs:
C      NAME     C*?   The name of the object.
C      KEYWRD   C*?   The name of the keyword in form 'MEM1.MEM2...'
C      TYPE     I     Data type of VALUE: 1=double, 2=real,
C                        3=character, 4=integer, 5=logical, 7=bit
C                        arrays.
C      DIM      I(*)  Dimensionality of value, an axis dimension of zero
C                     means that that dimension and higher are
C                     undefined.
C      VALUE    ?(*)  The value associated with keyword.
C      VALUEC   C*(*) Character value associated with keyword.
C   Outputs:
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   OBJNUM, KEYVAL(2), NKEY, LOCS, COL, ROW, OFF, BUFNO
      CHARACTER MEM*8, ARG*8
      LOGICAL LOCAL, KEYVL(2)
      HOLLERITH KEYVH(2)
      REAL      KEYVR(2)
      DOUBLE PRECISION KEYVD
      EQUIVALENCE (KEYVAL, KEYVR, KEYVH, KEYVL, KEYVD)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'TABLEIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look for base class.member name
C                                       in KEYWRD.
      CALL TBLMEM (KEYWRD, MEM, ARG, LOCAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Deal with keywrd here?
      IF (LOCAL) THEN
C                                       Get buffer number
         CALL OBINFO (NAME, BUFNO, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Table open?
         IF (BUFNO.LE.0) THEN
            MSGTXT = NAME // ' TABLE IS NOT OPEN'
            CALL MSGWRT (6)
            IERR = 5
            GO TO 999
            END IF
C                                       Number of columns
C                                       Current row number
         IF (MEM.EQ.'CURROW') THEN
C                                       Write old record
            CALL TABIO ('WRIT', 0, TBCROW(BUFNO), RECORD(1,BUFNO),
     *         IBUFFR(OFF,BUFNO), IERR)
            IF (IERR.NE.0) GO TO 500
C                                       negate because not read yet
            TBCROW(BUFNO) = -VALUE(1)
C                                       Number of rows.
         ELSE IF (MEM.EQ.'NROW') THEN
            IBUFFR(OFF+4,BUFNO) = VALUE(1)
C                                       Sort order
         ELSE IF (MEM.EQ.'SORT') THEN
            IBUFFR(OFF+42,BUFNO) = VALUE(1)
            IBUFFR(OFF+43,BUFNO) = VALUE(2)
C                                       Table Keyword
         ELSE IF (MEM.EQ.'KEY') THEN
            NKEY = 1
            LOCS = 1
            IF (TYPE.EQ.1) THEN
               CALL COPY (NWDPDP, VALUE, KEYVD)
            ELSE IF (TYPE.EQ.2) THEN
               CALL COPY (1, VALUE, KEYVR)
            ELSE IF (TYPE.EQ.3) THEN
               CALL CHR2H (MAX (8, DIM(1)), VALUEC, 1, KEYVH)
            ELSE IF (TYPE.EQ.4) THEN
               KEYVAL(1) = VALUE(1)
            ELSE IF (TYPE.EQ.5) THEN
               CALL COPY (1, VALUE, KEYVL)
               END IF
            CALL TABKEY ('WRIT', ARG, NKEY, IBUFFR(OFF,BUFNO), LOCS,
     *         KEYVAL, TYPE, IERR)
C                                       Table data entry
         ELSE IF (MEM.EQ.'ENTRY') THEN
C                                       Decode column number from ARG
            READ (ARG,1000,ERR=500) COL
C                                       Return entry
            ROW = ABS (TBCROW(BUFNO))
            CALL TABDPT (NAME, ROW, COL, TYPE, DIM, VALUE, VALUEC, IERR)
            END IF
      ELSE
C                                       Find object number
         CALL OBNAME (NAME, OBJNUM, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OBPUT (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
         END IF
      GO TO 999
C                                       Error decoding column number
 500  WRITE (MSGTXT,1500) KEYWRD
      CALL MSGWRT (7)
      MSGTXT = 'ERROR FOR TABLE OBJECT ' // NAME
      CALL MSGWRT (7)
      IERR = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I8)
 1500 FORMAT ('ERROR PARSING COLUMN NUMBER FROM ',A)
      END
      SUBROUTINE TABDGT (NAME, ROW, COL, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Returns table row data.
C   Inputs:
C      NAME     C*?   The name of the object.
C      ROW      I     Desired row number.
C      COL      I     Desired column number
C   Outputs:
C      TYPE     I     Data type of VALUE: 1=double, 2=real,
C                        3=character, 4=integer, 5=logical, 7=bit
C                        arrays.
C      DIM      I(*)  Dimensionality of value, an axis dimension of zero
C                     means that that dimension and higher are
C                     undefined.
C      VALUE    ?(*)  The value associated with keyword.
C      VALUEC   C*(*) Character value associated with keyword.
C      IERR     I     Error return code, 0=OK, -1 => record deselected.
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), VALUEC(*)*(*)
      INTEGER   ROW, COL, TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   BUFNO, OFF, RPNT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'TABLEIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Validity checks
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = 'TABLE IS NOT OPEN'
         IERR = 5
         GO TO 510
         END IF
C                                       Check column number
      IF (COL.GT.TBNCOL(BUFNO)) THEN
         WRITE(MSGTXT,1000) COL, TBNCOL(BUFNO)
         IERR = 1
         GO TO 510
         END IF
C                                       Check row number
      IF (ROW.GT.IBUFFR(OFF+4,BUFNO)) THEN
         WRITE(MSGTXT,1001) ROW, IBUFFR(OFF+4,BUFNO)
         IERR = 1
         GO TO 510
         END IF
C                                       May need to change row
      IF (TBCROW(BUFNO).NE.ROW) THEN
C                                       Need to write?
         IF ((IBUFFR(OFF+70,BUFNO).EQ.2)  .AND. (TBCROW(BUFNO).GT.0))
     *      CALL TABIO ('WRIT', 0, TBCROW(BUFNO), RECORD(1,BUFNO),
     *      IBUFFR(OFF,BUFNO), IERR)
         TBCROW(BUFNO) = ROW
C                                       Don't read past end of table.
         IF (ROW.LE.IBUFFR(OFF+4,BUFNO)) CALL TABIO ('READ', 0,
     *      TBCROW(BUFNO), RECORD(1,BUFNO), IBUFFR(OFF,BUFNO), IERR)
C                                       IERR = -1 => deselected
         IF (IERR.GT.0) GO TO 500
         END IF
      TBCROW(BUFNO) = ROW
C                                       Return column entry
      DIM(1) = TBDIM(COL,BUFNO)
      DIM(2) = 1
      DIM(3) = 0
      TYPE  = TBTYPE(COL,BUFNO)
      RPNT = TBPRT(COL,BUFNO)
      IF (TYPE.EQ.1) THEN
         RPNT = (RPNT-1) * NWDPDP + 1
         CALL COPY (DIM(1)*NWDPDP, RECD(RPNT,BUFNO), VALUE)
      ELSE IF (TYPE.EQ.2) THEN
         CALL COPY (DIM(1), RECR(RPNT,BUFNO), VALUE)
      ELSE IF (TYPE.EQ.3) THEN
         CALL H2CHR (DIM, 1, RECH(RPNT,BUFNO), VALUEC)
      ELSE IF (TYPE.EQ.4) THEN
         CALL COPY (DIM(1), RECORD(RPNT,BUFNO), VALUE)
      ELSE IF (TYPE.EQ.5) THEN
         CALL COPY (DIM(1), RECL(RPNT,BUFNO), VALUE)
      ELSE IF (TYPE.EQ.7) THEN
         CALL LG2BIT (DIM(1), VALUE, RECORD(RPNT,BUFNO), -1)
         END IF
      GO TO 999
C                                       Error in TABIO
 500  WRITE(MSGTXT,1500) IERR
      IERR = 4
C                                       Other error?
 510  CALL MSGWRT (7)
      MSGTXT = 'ERROR FOR TABLE OBJECT: ' // NAME
      CALL MSGWRT (7)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REQUESTED COLUMN ',I4,' EXCEEDS MAX OF ',I4)
 1001 FORMAT ('REQUESTED ROW ',I4,' EXCEEDS MAX OF ',I4)
 1500 FORMAT ('ERROR ',I3,' IN TABIO ')
      END
      SUBROUTINE TABDPT (NAME, ROW, COL, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Stores table row data.
C   Inputs:
C      NAME     C*?   The name of the object.
C      ROW      I     Desired row number.
C      COL      I     Desired column number
C      TYPE     I     Data type of VALUE: 1=double, 2=real,
C                        3=character, 4=integer, 5=logical, 7=bit
C                        arrays.
C      DIM      I(*)  Dimensionality of value, an axis dimension of zero
C                     means that that dimension and higher are
C                     undefined.
C      VALUE    ?(*)  The value associated with keyword.
C      VALUEC   C*(*) Character value associated with keyword.
C   Outputs:
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), VALUEC(*)*(*)
      INTEGER   ROW, COL, TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   BUFNO, OFF, RPNT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'TABLEIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Validity checks
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = 'TABLE IS NOT OPEN'
         IERR = 5
         GO TO 510
         END IF
C                                       Check column number
      IF (COL.GT.TBNCOL(BUFNO)) THEN
         WRITE(MSGTXT,1000) COL, TBNCOL(BUFNO)
         IERR = 1
         GO TO 510
         END IF
C                                       Check type
      IF (TYPE.NE.TBTYPE(COL,BUFNO)) THEN
         WRITE(MSGTXT,1002) COL, TYPE, TBTYPE(COL,BUFNO)
         IERR = 1
         GO TO 510
         END IF
C                                       Check dim
      IF (DIM(1).NE.TBDIM(COL,BUFNO)) THEN
         WRITE(MSGTXT,1003) COL, DIM(1), TBDIM(COL,BUFNO)
         IERR = 1
         GO TO 510
         END IF
C                                       May need to write old and
C                                       read new row
      IF (TBCROW(BUFNO).NE.ROW) THEN
         IF (TBCROW(BUFNO).GT.0) CALL TABIO ('WRIT', 0, TBCROW(BUFNO),
     *      RECORD(1,BUFNO), IBUFFR(OFF,BUFNO), IERR)
         IF (IERR.NE.0) GO TO 500
         TBCROW(BUFNO) = ROW
C                                       Don't read past end of table.
         IF (ROW.LE.IBUFFR(OFF+4,BUFNO)) CALL TABIO ('READ', 0,
     *      TBCROW(BUFNO), RECORD(1,BUFNO), IBUFFR(OFF,BUFNO), IERR)
         IF (IERR.NE.0) GO TO 500
         END IF
C                                       Update no. rows
      IBUFFR(OFF+4,BUFNO) = MAX (IBUFFR(OFF+4,BUFNO), ROW)
      TBCROW(BUFNO) = ROW
      RPNT = TBPRT(COL,BUFNO)
C                                       Store column entry
      IF (TYPE.EQ.1) THEN
         RPNT = (RPNT-1) * NWDPDP + 1
         CALL COPY (DIM(1)*NWDPDP, VALUE, RECD(RPNT,BUFNO))
      ELSE IF (TYPE.EQ.2) THEN
         CALL COPY (DIM(1), VALUE, RECR(RPNT,BUFNO))
      ELSE IF (TYPE.EQ.3) THEN
         CALL CHR2H (DIM, VALUEC, 1, RECH(RPNT,BUFNO))
      ELSE IF (TYPE.EQ.4) THEN
         CALL COPY (DIM(1), VALUE, RECORD(RPNT,BUFNO))
      ELSE IF (TYPE.EQ.5) THEN
         CALL COPY (DIM(1), VALUE, RECL(RPNT,BUFNO))
      ELSE IF (TYPE.EQ.7) THEN
         CALL LG2BIT (DIM(1), VALUE, RECORD(RPNT,BUFNO), +1)
         END IF
         GO TO 999
C                                       Error in TABIO
 500  WRITE(MSGTXT,1500) IERR
C                                       Other error
 510  CALL MSGWRT (7)
      MSGTXT = 'ERROR FOR TABLE OBJECT: ' // NAME
      CALL MSGWRT (7)
      IERR = 4
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REQUESTED COLUMN ',I4,' EXCEEDS MAX OF ',I4)
 1002 FORMAT ('INCOMPATIBLE DATA TYPES FOR COL ',I3,I4,'<>',I4)
 1003 FORMAT ('INCOMPATIBLE DIMENSIONALITY FOR COL ',I3,I4,'<>',I4)
 1500 FORMAT ('ERROR ',I3,' IN TABIO ')
      END
      SUBROUTINE TABKGT (NAME, KEYS, NKEYS, KLOCS, KVALS, KTYPE, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Fetches values of specified table keywords.  If keys(1) is
C     blank then all keywords up to a maximum of nkeys is read.
C   Inputs:
C      NAME     C*?        The name of the object.
C      KEYS     C(*)*8     Keywords to read; first blank => read all.
C   In/out:
C      NKEYS   I           Max. Number of keywords to read; on return
C                          the number of keywords.
C   Output:
C      KLOCS    I(NKEYS)   The word offset of first integer
C                          word of keyword value in array KVALS.
C                          This value will be -1 for keywords not found.
C     KVALS     I(2*NKEYS) The array of keyword values.  This array may
C                          contain values of type real, double, integer,
C                          logical and hollerith.  Due to word
C                          alignment problems on some machines values
C                          longer than an integer should be copied,
C                          eg. if the  5th keyword (XXX) is a double:
C                               IPOINT = KLOCS(5)
C                               CALL COPY (NWDPDP, KVALS(IPOINT), XXX)
C     KTYPE     I(NKEYS)   The type code of the keywords:
C                             1 = Double precision floating
C                             2 = Single precision floating
C                             3 = Character string (8 HOLLERITH chars)
C                             4 = integer
C                             5 = Logical
C   Outputs:
C      IERR     I     Error return code, 0=OK
C                     Note: will return 0 if some keywords not found.
C-----------------------------------------------------------------------
      INTEGER   NKEYS
      CHARACTER NAME*(*), KEYS(NKEYS)*8
      INTEGER   KLOCS(NKEYS), KVALS(*), KTYPE(NKEYS), IERR
C
      CHARACTER OPCODE*4
      INTEGER   BUFNO, OFF, MSGSAV
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'TABLEIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = 'TABLE IS NOT OPEN'
         IERR = 5
         GO TO 510
         END IF
C                                       Set OPCODE
      OPCODE = 'READ'
      IF (KEYS(1) .EQ. '        ') OPCODE = 'ALL '
C                                       Call TABKEY (disable error
C                                       messages)
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TABKEY (OPCODE, KEYS, NKEYS, IBUFFR(OFF,BUFNO), KLOCS,
     *   KVALS, KTYPE, IERR)
      MSGSUP = MSGSAV
C                                       Some not found is OK.
      IF (IERR.GT.20) IERR = 0
      IF (IERR.NE.0) GO TO 500
      GO TO 999
C                                       Error in TABKEY
 500  WRITE(MSGTXT,1500) IERR
C                                       Other error
 510  CALL MSGWRT (7)
      MSGTXT = 'ERROR FOR TABLE OBJECT: ' // NAME
      CALL MSGWRT (7)
      IERR = 4
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1500 FORMAT ('ERROR ',I3,' IN TABKEY ')
      END
      SUBROUTINE TABKPT (NAME, KEYS, NKEYS, KLOCS, KVALS, KTYPE, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Stores values of specified table keywords.
C   Inputs:
C      NAME     C*?        The name of the object.
C      KEYS     C(*)*8     Keywords to store.
C      NKEYS   I           Number of keywords.
C      KLOCS    I(NKEYS)   The word offset of first integer
C                          word of keyword value in array KVALS.
C     KVALS     I(2*NKEYS) The array of keyword values.  This array may
C                          contain values of type real, double, integer,
C                          logical and hollerith.  Due to word
C                          alignment problems on some machines values
C                          longer than an integer should be copied,
C                          eg. if the  5th keyword (XXX) is a double
C                               KLOCS(5) = IPOINT
C                               CALL COPY (NWDPDP, XXX, KVALS(IPOINT))
C     KTYPE     I(NKEYS)   The type code of the keywords:
C                             1 = Double precision floating
C                             2 = Single precision floating
C                             3 = Character string (8 HOLLERITH chars)
C                             4 = integer
C                             5 = Logical
C   Outputs:
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      INTEGER   NKEYS
      CHARACTER NAME*(*), KEYS(NKEYS)*8
      INTEGER   KLOCS(NKEYS), KVALS(*), KTYPE(NKEYS), IERR
C
      CHARACTER OPCODE*4
      INTEGER   BUFNO, OFF
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'TABLEIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = 'TABLE IS NOT OPEN'
         IERR = 5
         GO TO 510
         END IF
C                                       Set OPCODE
      OPCODE = 'WRIT'
C                                       Call TABKEY
      CALL TABKEY (OPCODE, KEYS, NKEYS, IBUFFR(OFF,BUFNO), KLOCS,
     *   KVALS, KTYPE, IERR)
      IF (IERR.NE.0) GO TO 500
      GO TO 999
C                                       Error in TABKEY
 500  WRITE(MSGTXT,1500) IERR
C                                       Other error
 510  CALL MSGWRT (7)
      MSGTXT = 'ERROR FOR TABLE OBJECT: ' // NAME
      CALL MSGWRT (7)
      IERR = 4
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1500 FORMAT ('ERROR ',I3,' IN TABKEY ')
      END
      SUBROUTINE TBLSRT (NAME, KEY1, KEY2, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Sorts a table into an order specified by the values contained in
C   columns specified by KEY1 and KEY2.  If KEY1=KEY2 and is a character
C   column then an up to 8 character sort is done on that column.
C   The object should not be open when the call is made.
C      The default output ordering is in ascending order of the key
C   values. Descending order or ordering by absolute value can be
C   requested by an appropriate prefix to the column labels (KEY1,
C   KEY2).  If the column label begins with a minus sign "-" then the
C   sorting will be by descending value of that column.  After the minus
C   sign, if any, "ABS:" indicates that the absolute values of numeric
C   entries in that column be used in the sorting.  For example to sort
C   column labeled 'SOMETHING_OR_OTHER' by descending absolute order
C   then the corresponding KEY? would be: '-ABS:SOMETHING_OR_OTHER'
C     Note: This routine uses fixed LUNs and may not function when any
C   objects are open. This routine may create scratch files.
C   Inputs:
C      NAME     C*?    The name of the object.
C      KEY1     C*?    Label of the column containing the slowest
C                      varying keys.  May include abs. and/or descending
C                      order prefix.
C      KEY2     C*?    Label of the column containing the fastest
C                      varying keys.  May include abs. and/or descending
C                      order prefix.
C   Outputs:
C      IERR     I      Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), KEY1*(*), KEY2*(*)
      INTEGER   IERR
C
      INTEGER   BUFNO, BUFNO2, COL1, COL2, SORT(2), TYPE, DIM(3), TDISK,
     *   TCNO, TVER, KEY(2,2), IER, BS, C1TYPE, C1DIM, KEYSUB(2,2),
     *   DUMMY(1)
      REAL      FKEY(2,2)
      LOGICAL   ISOPEN, ISOP2, ABS1, ABS2, DESC1, DESC2
      CHARACTER TTYPE*2, STAT*4, TNAM*36, KKEY1*32, KKEY2*32, TKEY*32,
     *   CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'TABLEIO.INC'
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
      IERR = 0
      BS = BUFSIZ
      ISOPEN = .FALSE.
      ISOP2 = .FALSE.
C                                       Check for descending order.
      DESC1 = KEY1(1:1).EQ.'-'
      IF (DESC1) THEN
         KKEY1 = KEY1(2:)
      ELSE
         KKEY1 = KEY1
         END IF
      DESC2 = KEY2(1:1).EQ.'-'
      IF (DESC2) THEN
         KKEY2 = KEY2(2:)
      ELSE
         KKEY2 = KEY2
         END IF
C                                       Check for absolute values
      ABS1 = KKEY1(1:4).EQ.'ABS:'
      IF (ABS1) THEN
         TKEY = KKEY1(5:)
         KKEY1 = TKEY
         END IF
      ABS2 = KKEY2(1:4).EQ.'ABS:'
      IF (ABS2) THEN
         TKEY = KKEY2(5:)
         KKEY2 = TKEY
         END IF
C                                       Shallow copy table to get second
C                                       buffer.
      TNAM = 'Temporary shallow copy'
      CALL TBCOPY (NAME, TNAM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open
      CALL TABOPN (NAME, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      ISOPEN = .TRUE.
C                                       Look up table info
      CALL TBLKUP (NAME, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get CATBLK
      STAT = 'REST'
      CALL CATIO ('READ', TDISK, TCNO, CATBLK, STAT, SBUFF, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
         WRITE (MSGTXT,1002) IERR
         GO TO 990
         END IF
      IERR = 0
C                                       Get buffer number
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check sort order
      CALL TABCOL (NAME, 1, KKEY1, DUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      COL1 = DUMMY(1)
      CALL TABCOL (NAME, 1, KKEY2, DUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      COL2 = DUMMY(1)
      CALL TABGET (NAME, 'SORT', TYPE, DIM, SORT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get first column info
      C1TYPE = TBTYPE(COL1,BUFNO)
      C1DIM = TBDIM(COL1,BUFNO)
C                                       Open copy for second buffer
      CALL TABOPN (TNAM, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      ISOP2 = .TRUE.
C                                       Get second buffer number
      CALL OBINFO (TNAM, BUFNO2, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close tables; will use buffers
C                                       allocated for the table when it
C                                       was open.
      CALL TABCLO (NAME, IERR)
      IF (IERR.NE.0) GO TO 990
      ISOPEN = .FALSE.
      CALL TABCLO (TNAM, IERR)
      IF (IERR.NE.0) GO TO 990
      ISOP2 = .FALSE.
C                                       Delete temporary object.
      CALL DESTRY (TNAM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Is a sort needed?
      IF ((COL1.EQ.SORT(1)) .AND. (COL2.EQ.SORT(2))) GO TO 999
C                                       Sort by abs?
      IF (ABS1) COL1 = -COL1
      IF (ABS2) COL2 = -COL2
C                                       Sort table
      KEY(1,1) = COL1
      KEY(2,1) = COL1
      KEY(1,2) = COL2
      KEY(2,2) = COL2
      FKEY(1,1) = 1.0
      FKEY(2,1) = 1.0E-10
      FKEY(1,2) = 1.0
      FKEY(2,2) = 1.0E-10
C                                       If the sort columns are the same
C                                       and a character string use up to
C                                       8 characters for sort.
      IF ((COL1.EQ.COL2) .AND. (C1TYPE.EQ.3)) THEN
         FKEY(1,1) = 1.0
         FKEY(2,1) = MIN (4, C1DIM)
         KEY(2,1) = COL1
         IF (C1DIM.GT.4) THEN
            FKEY(1,2) = 5.0
            FKEY(2,2) = MIN (4, C1DIM-4)
            KEY(2,2) = COL2
            END IF
         END IF
C                                       Any descending order?
      IF (DESC1) THEN
         FKEY(1,1) = - ABS (FKEY(1,1))
         FKEY(2,1) = - ABS (FKEY(2,1))
      ELSE
         FKEY(1,1) = ABS (FKEY(1,1))
         FKEY(2,1) = ABS (FKEY(2,1))
         END IF
      IF (DESC2) THEN
         FKEY(1,2) = - ABS (FKEY(1,2))
         FKEY(2,2) = - ABS (FKEY(2,2))
      ELSE
         FKEY(1,2) = ABS (FKEY(1,2))
         FKEY(2,2) = ABS (FKEY(2,2))
         END IF
C                                       Sort
      CALL TABSRT (TDISK, TCNO, TTYPE, TVER, TVER, KEY, KEYSUB, FKEY,
     *   IBUFFR(1,BUFNO2), CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'PROBLEM SORTING TABLE OBJECT: ' // NAME
      CALL MSGWRT (7)
C                                       Make sure object is closed
      IF (ISOPEN) CALL TABCLO (NAME, IER)
      IF (ISOP2) CALL TABCLO (TNAM, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1002 FORMAT ('TBLSRT: ERROR ',I3,' READING CATBLK')
      END
      SUBROUTINE TBLMRG (NAME, MKOL, TOLER, SKOL, NSKOL, SRTKOL, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Merges a table by first sorting the table using the merge columns,
C   then merging rows with columns MKOL agreeing to within the tolerance
C   given by TOLER and then sorting by SRTKOL.
C      The default output ordering is in ascending order of the key
C   values. Descending order or ordering by absolute value can be
C   requested by an appropriate prefix to the column labels (SRTKOL)
C   If the column label begins with a minus sign "-" then the
C   sorting will be by descending value of that column.  After the minus
C   sign, if any, "ABS:" indicates that the absolute values of numeric
C   entries in that column be used in the sorting.  For example to sort
C   column labeled 'SOMETHING_OR_OTHER' by descending absolute order
C   then the corresponding SRTKOL entry would be:
C   '-ABS:SOMETHING_OR_OTHER'
C   Note: This routine uses fixed LUNs and may not function when any
C   objects are open.
C   This routine may create scratch files.
C   Inputs:
C      NAME     C*?    The name of the object.
C      MKOL     C(2)*? Columns to be compared
C      TOLER    R(2)   Tolerance for numeric values in columns MKOL
C      SKOL     C(*)*? Labels of columns to be summed
C      NSKOL    I      Number of entries in SKOL. (max 25)
C      SRTKOL   C(2)*? Labels of columns for output sort.  May include
C                      abs. and/or descending order prefix.
C                      varying keys.
C      KEY2     C*?    Label of the column containing the fastest
C                      varying keys.
C   Outputs:
C      IERR     I      Error return code, 0=OK
C-----------------------------------------------------------------------
      INTEGER   NSKOL, IERR
      CHARACTER NAME*(*), MKOL(2)*(*), SKOL(NSKOL)*(*), SRTKOL(2)*(*)
      REAL      TOLER(2)
C
      INTEGER   BUFNO, BUFNO2, TDISK, TCNO, TVER, IER, BS, ECOL(3),
     *   SCOL(25), OUTNUM, LIM
      LOGICAL   ISOPEN, ISOP2
      CHARACTER TTYPE*2, STAT*4, TNAM*36
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'TABLEIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Sort input by MKOL
      CALL TBLSRT (NAME, MKOL(1), MKOL(2), IERR)
      IF (IERR.NE.0) GO TO 990
      BS = BUFSIZ
      ISOPEN = .FALSE.
      ISOP2 = .FALSE.
C                                       Shallow copy table to get second
C                                       buffer.
      TNAM = 'TBMRG Temporary table'
      CALL TBCOPY (NAME, TNAM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open
      CALL TABOPN (NAME, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      ISOPEN = .TRUE.
C                                       Look up table info
      CALL TBLKUP (NAME, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get CATBLK
      STAT = 'REST'
      CALL CATIO ('READ', TDISK, TCNO, CATBLK, STAT, SBUFF, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
         WRITE (MSGTXT,1002) IERR
         GO TO 990
         END IF
      IERR = 0
C                                       Get buffer number
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get column numbers
      CALL TABCOL (NAME, 2, MKOL, ECOL, IERR)
      IF (IERR.NE.0) GO TO 990
      ECOL(3) = 0
      LIM = MIN (NSKOL, 25)
      CALL TABCOL (NAME, LIM, SKOL, SCOL, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (LIM.LT.25) SCOL(LIM+1) = 0
C                                       Open copy for second buffer
      CALL TABOPN (TNAM, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      ISOP2 = .TRUE.
C                                       Get second buffer number
      CALL OBINFO (TNAM, BUFNO2, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close tables; will use buffers
C                                       allocated for the table when it
C                                       was open.
      CALL TABCLO (NAME, IERR)
      IF (IERR.NE.0) GO TO 990
      ISOPEN = .FALSE.
      CALL TABCLO (TNAM, IERR)
      IF (IERR.NE.0) GO TO 990
      ISOP2 = .FALSE.
C                                       Delete temporary object.
      CALL DESTRY (TNAM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Merge
      CALL TABMRG (TDISK, TCNO, TTYPE, TVER, TVER, ECOL, SCOL, TOLER,
     *   IBUFFR(1,BUFNO), IBUFFR(1,BUFNO2), CATBLK,
     *   OUTNUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Sort output by SRTKOL
      CALL TBLSRT (NAME, SRTKOL(1), SRTKOL(2), IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'PROBLEM MERGING TABLE OBJECT: ' // NAME
      CALL MSGWRT (7)
C                                       Make sure object is closed
      IF (ISOPEN) CALL TABCLO (NAME, IER)
      IF (ISOP2) THEN
         CALL TABCLO (TNAM, IER)
         CALL DESTRY (TNAM, IER)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1002 FORMAT ('TBLMRG: ERROR ',I3,' READING CATBLK')
      END
      SUBROUTINE TABEXI (NAME, EXISTS, IERR)
C-----------------------------------------------------------------------
C   Tests whether the table exists.
C
C   Inputs:
C      NAME    C*?    Object name
C
C   Outputs:
C      EXISTS  L      True if and only if the table exists
C      IERR    I      Return code: 0 indicates success
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      LOGICAL   EXISTS
      INTEGER   IERR
C-----------------------------------------------------------------------
      INTEGER   TDISK, TCNO, TVER, LUN
      CHARACTER TTYPE*2
      LOGICAL   TABLE, EXIST, FITASC
      INCLUDE 'INCS:CLASSIO.INC'
C-----------------------------------------------------------------------
C                                       Look up table:
      CALL TBLKUP (NAME, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Check existance:
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ISTAB (TTYPE, TDISK, TCNO, TVER, LUN, SBUFF, TABLE, EXIST,
     *   FITASC, IERR)
      EXISTS = EXIST .AND. TABLE .AND. (IERR.EQ.0)
C                                       Free LUN
      CALL OBLUFR (LUN)
C
  999 RETURN
      END
      SUBROUTINE TBLMEM (KEYWRD, MEM, ARG, LOCAL, IERR)
C-----------------------------------------------------------------------
C   Private function
C   Parses the keyword into component parts and then checks if mem is a
C   special local value.
C   Inputs:
C      KEYWRD  C*?  Keyword in form 'base.mem.mem'
C   Outputs:
C      MEM     C*?  Table class member
C      ARG     C*?  Any argument, for MEM='ENTRY' it is a right
C                   justified string.
C      LOCAL   L    If true then MEM is recognized as a locally stored
C                   member.
C      IERR    I    Return code, 0=>OK else unknown base class.
C-----------------------------------------------------------------------
      CHARACTER KEYWRD*(*), MEM*(*), ARG*(*)
      LOGICAL   LOCAL
      INTEGER   IERR
C
      INTEGER   POINT, PNT2, LOOP, LARG, L, LEN
      INCLUDE 'TABLE.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Look for base_class.member_name
C                                       in KEYWRD.
      POINT = INDEX (KEYWRD, '.')
C                                       Base class reference
      IF (POINT.GE.1) THEN
         MEM = KEYWRD(1:POINT-1)
C                                       Special trap for ENTRY member
         IF (MEM.EQ.'ENTRY') THEN
            LARG = LEN (ARG)
            PNT2 = INDEX (KEYWRD, ' ')
            IF (PNT2.LE.0) PNT2 = LEN (KEYWRD) + 1
            L = PNT2 - POINT - 2
            ARG(LARG-L:) = KEYWRD(POINT+1:PNT2-1)
         ELSE
            ARG = KEYWRD(POINT+1:)
            END IF
      ELSE
         MEM = KEYWRD
         ARG = '  '
         END IF
C                                       Search list of local members.
         LOCAL = .FALSE.
         DO 20 LOOP = 1,NMEM
            LOCAL = LOCAL .OR. (MEM.EQ.LMEM(LOOP))
 20         CONTINUE
C
 999  RETURN
      END
      SUBROUTINE TBLKUP (NAME, TDISK, TCNO, TTYPE, TVER, IERR)
C-----------------------------------------------------------------------
C   Private
C   Looks up information on a table object. Fills in names etc. in
C   object.  Reads CATBLK.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      TDISK I     AIPS disk number
C      TCNO  I     AIPS catalog slot number
C      TTYPE C*2   Table type
C      TVER  I     Table version number
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), TTYPE*(*)
      INTEGER   TDISK, TCNO, TVER, IERR
C
      INTEGER   OBJNUM, TYPE, DIM(7), TSEQ, USID, DUMMY
      CHARACTER TNAME*12, TCLASS*6, PTYPE*2, STAT*4, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find object number
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Look up table info
      CALL OBGET (OBJNUM, 'NAME', TYPE, DIM, DUMMY, TNAME, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBGET (OBJNUM, 'CLASS', TYPE, DIM, DUMMY, TCLASS, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBGET (OBJNUM, 'IMSEQ', TYPE, DIM, TSEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBGET (OBJNUM, 'DISK', TYPE, DIM, TDISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBGET (OBJNUM, 'TBLTYPE', TYPE, DIM, DUMMY, TTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBGET (OBJNUM, 'VER', TYPE, DIM, TVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      TCNO = 1
      USID = 0
      PTYPE = '  '
      CALL CATDIR ('SRCH', TDISK, TCNO, TNAME, TCLASS, TSEQ, PTYPE,
     *   USID, STAT, SBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1000) IERR, TNAME, TCLASS, TSEQ, TDISK
         CALL MSGWRT (6)
         GO TO 990
         END IF
C                                       Get CATBLK
      STAT = 'REST'
      CALL CATIO ('READ', TDISK, TCNO, CATBLK, STAT, SBUFF, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
         WRITE (MSGTXT,1002) IERR
         CALL MSGWRT (7)
         GO TO 990
         END IF
      IERR = 0
C                                       Update any information
      CALL OBHPUT (NAME, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      DIM(1) = 12
      CALL OBPUT (OBJNUM, 'NAME', OOACAR, DIM, DUMMY, TNAME, IERR)
      IF (IERR.NE.0) GO TO 990
      DIM(1) = 6
      CALL OBPUT (OBJNUM, 'CLASS', OOACAR, DIM, DUMMY, TCLASS, IERR)
      IF (IERR.NE.0) GO TO 990
      DIM(1) = 1
      CALL OBPUT (OBJNUM, 'IMSEQ', OOAINT, DIM, TSEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBPUT (OBJNUM, 'DISK', OOAINT, DIM, TDISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBPUT (OBJNUM, 'CNO', OOAINT, DIM, TCNO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'TBLKUP: ERROR WITH TABLE OBJECT ' // NAME
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' FINDING ',A,'.',A,'.',I4,' DISK=', I3)
 1002 FORMAT ('ERROR ',I3,' READING CATBLK')
      END
      SUBROUTINE TBLHIV (NAME, NVER, IERR)
C-----------------------------------------------------------------------
C   Private
C   Finds highest version number of TBLTYPE table associated with
C   catalog entry. Returns 0 if there are no versions of that type.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      NVER  I     Table version number
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   NVER, IERR
C
      INTEGER   TDISK, TCNO, TVER
      CHARACTER TTYPE*2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IERR = 0
      NVER = 0
C                                       Get table type and force read of
C                                       CATBLK.
      CALL TBLKUP (NAME, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Read CATBLK
      CALL OBHGET (NAME, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Loop thru CATBLK
      CALL FNDEXT (TTYPE, CATBLK, NVER)
      GO TO 999
C                                       Error
 990  MSGTXT = 'TBLHIV: ERROR WITH TABLE ' // NAME
      CALL MSGWRT (6)
C
 999  RETURN
      END
