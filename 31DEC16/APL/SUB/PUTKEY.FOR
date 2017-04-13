      SUBROUTINE PUTKEY (IVOL, ISLOT, KEYWRD, KEYTYP, KEYVAL, KEYSTR,
     *   IERR)
C-----------------------------------------------------------------------
C! Adds a keyword/value pair to a catalog header.
C# Header
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006
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
C   Writes a new keyword/value pair in a catalog header and makes an
C   entry in the history file about the new values.
C   Inputs:
C      IVOL    I    Disk number
C      ISLOT   I    Catalog slot number
C      KEYWRD  C*8  Keyword
C      KEYTYPE C*4  Data type ('D','R','C','I','L')
C      KEYVAL  R(2) Numeric data. 'D' uses the sum of (1) & (2)
C                   'R' uses (1), 'I' uses IROUND((1))
C                   'L' is true of KEYVAL(1) > 1
C      KEYSTR  C*8  Character value
C   Output:
C      IERR    I    Return code, 0=OK else failed
C   Output in common (DERR.INC)
C      ERRNUM  I    POPS error number
C      ERRLEV  I    POPS traceback pointer
C      PNAME   C*6(*) Subroutine name in POPS traceback stack.
C-----------------------------------------------------------------------
      INTEGER   IVOL, ISLOT, IERR
      CHARACTER KEYWRD*(*), KEYTYP*(*), KEYSTR*(*)
      REAL      KEYVAL(2)
C
      CHARACTER HILINE*72, PRGNAM*6
      INTEGER   POTERR, IBUFF2(256), IVALUE, ITYPE, NUMKEY, KEYLOC,
     *   IERH, CATBLK(256), IHLUN, IROUND
      REAL      AVALUE(2), RVALUE
      DOUBLE PRECISION DVALUE
      LOGICAL   LVALUE, OPEN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA PRGNAM /'PUTKEY'/
      DATA IHLUN /27/
C-----------------------------------------------------------------------
      OPEN = .FALSE.
C                                       Get CATBLK
      CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'WRIT', IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         IF (IERR.EQ.5) WRITE (MSGTXT,1001)
         POTERR = 33
         GO TO 970
         END IF
      OPEN = .TRUE.
C                                       Set value by type
C                                       Double precision
      IF (KEYTYP.EQ.'D') THEN
         ITYPE = 1
         CALL POPSRD ('R2D', KEYVAL, DVALUE)
         CALL RCOPY (NWDPDP, DVALUE, AVALUE)
C                                       Real
      ELSE IF (KEYTYP.EQ.'R') THEN
         ITYPE = 2
         RVALUE = KEYVAL(1)
         AVALUE(1) = RVALUE
         AVALUE(2) = 0.0
C                                       Character
      ELSE IF (KEYTYP.EQ.'C') THEN
         ITYPE = 3
         CALL CHR2H (8, KEYSTR, 1, AVALUE)
C                                       Integer
      ELSE IF (KEYTYP.EQ.'I') THEN
         ITYPE = 4
         IVALUE = IROUND (KEYVAL(1))
         AVALUE(2) = 0.0
         CALL COPY (1, IVALUE, AVALUE)
C                                       Logical
      ELSE IF (KEYTYP.EQ.'L') THEN
         ITYPE = 5
         LVALUE = KEYVAL(1) .GT. 0.0
         AVALUE(2) = 0.0
         CALL COPY (1, LVALUE, AVALUE)
C                                       Unknown data type
      ELSE
         MSGTXT = 'UNKNOWN DATA TYPE = ' // KEYTYP
         POTERR = 23
         GO TO 970
         END IF
      NUMKEY = 1
      KEYLOC = 1
      CALL CATKEY ('WRIT', IVOL, ISLOT, KEYWRD, NUMKEY, KEYLOC,
     *   AVALUE, ITYPE, IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1002) IERR
         POTERR = 33
         GO TO 970
         END IF
C                                       Make history entries
      CALL HIOPEN (IHLUN, IVOL, ISLOT, IBUFF2, IERH)
      IERR = IERH
C                                       Keyword, type
      WRITE (HILINE,2000) TSKNAM, KEYWRD, KEYTYP
      IF (IERH.EQ.0) CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
C                                       Value by type
C                                       Double precision
      IF (KEYTYP.EQ.'D') THEN
         WRITE (HILINE,2001) TSKNAM, DVALUE
C                                       Real
      ELSE IF (KEYTYP.EQ.'R') THEN
         WRITE (HILINE,2002) TSKNAM, RVALUE
C                                       Character
      ELSE IF (KEYTYP.EQ.'C') THEN
         WRITE (HILINE,2003) TSKNAM, KEYSTR
C                                       Integer
      ELSE IF (KEYTYP.EQ.'I') THEN
         WRITE (HILINE,2004) TSKNAM, IVALUE
C                                       Logical
      ELSE IF (KEYTYP.EQ.'L') THEN
         WRITE (HILINE,2005) TSKNAM, KEYVAL(1)
         END IF
      IF (IERH.EQ.0) CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
C                                       Close history file.
      IF (IERR.EQ.0) CALL HICLOS (IHLUN, .TRUE., IBUFF2, IERH)
C                                       Update CATBLK
      CALL CATIO ('UPDT', IVOL, ISLOT, CATBLK, 'CLWR', IBUFF2, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.9)) POTERR = 33
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1005) IERR
         GO TO 970
         END IF
      IERR = 0
      GO TO 999
C                                       Print error.
 970  CALL MSGWRT (8)
C                                       AIPS error management.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C                                       Update CATBLK if necessary
      IF (OPEN) THEN
         CALL CATIO ('UPDT', IVOL, ISLOT, CATBLK, 'CLWR', IBUFF2, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.9)) POTERR = 33
         OPEN = .FALSE.
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1005) IERR
            GO TO 970
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ',I3,' READING CATALOG HEADER')
 1001 FORMAT ('MAP TOO BUSY')
 1002 FORMAT ('ERROR ',I3,' WRITING KEYWORD/VALUE PAIR')
 1005 FORMAT ('ERROR ',I4,' UPDATING CATALOG HEADER')
 2000 FORMAT (A,' KEYWORD = ''',A,''', KEYTYPE = ''',A,
     *   ''' / new catalog keyword')
 2001 FORMAT (A,' KEYVALUE = ',1PD20.12,',0 / Keyword value')
 2002 FORMAT (A,' KEYVALUE = ',1PE15.7,',0 / Keyword value')
 2003 FORMAT (A,' KEYSTRING = ',A,' / Keyword value')
 2004 FORMAT (A,' KEYVALUE = ',I2,',0 / Keyword value')
 2005 FORMAT (A,' KEYVALUE = ',1PE15.7,',0 / Keyword value')
      END
