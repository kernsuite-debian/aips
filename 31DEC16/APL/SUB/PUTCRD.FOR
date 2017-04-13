      SUBROUTINE PUTCRD (CARD, VOL, CNO, IERR)
C-----------------------------------------------------------------------
C! parses header keyword from FITS card and adds to catalog header
C# FITS Parsing Header
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998
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
C   PUTCRD takes a FITS history card of the form
C        HISTORY AIPS HEADERi  keyword = value / comment
C   parses the type code (i), the keyword, and the value and then
C   adds them to the catalog header file with CATKEY.
C   Inputs:
C      CARD   C*80   Card image
C      VOL    I      Disk volume
C      CNO    I      Catalog slot number
C   Output:
C      IERR   I      Error code: 0 okay, 1 => input error, > 1 => bad
C                       error
C-----------------------------------------------------------------------
      INTEGER   VOL, CNO, IERR
      CHARACTER CARD*80
C
      CHARACTER KEYWRD*8, KEYVAL*8
      INTEGER   ITYPE, NP, IT, IROUND, NUMKEY, LOCS, SCRTCH(256), I
      LOGICAL   LVALUE
      REAL      VALUES(2)
      DOUBLE PRECISION X
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 1
      IF (CARD(:19).NE.'HISTORY AIPS HEADER') THEN
         MSGTXT = 'PUTCRD CALLED WITH WRONG SORT OF FITS HEADER CARD:'
         CALL MSGWRT (6)
         GO TO 990
         END IF
C                                       type of data
      ITYPE = 0
      IF (CARD(20:20).EQ.'1') ITYPE = 1
      IF (CARD(20:20).EQ.'2') ITYPE = 2
      IF (CARD(20:20).EQ.'3') ITYPE = 3
      IF (CARD(20:20).EQ.'4') ITYPE = 4
      IF (CARD(20:20).EQ.'5') ITYPE = 5
      IF (ITYPE.LE.0) THEN
         MSGTXT = 'ILLEGAL HEADER KEYWORD TYPE ''' // CARD(20:20) //
     *      ''' IN CARD'
         CALL MSGWRT (6)
         GO TO 990
         END IF
C                                       get keyword
C                                       allowing imbedded blanks
      NP = 20
 10   NP = NP + 1
         IF (NP.GE.80) GO TO 980
         IF (CARD(NP:NP).EQ.' ') GO TO 10
      IT = NP
 20   NP = NP + 1
         IF (NP.GE.80) GO TO 980
         IF (CARD(NP:NP).NE.'=') GO TO 20
      I = MIN (IT+7, NP-1)
      KEYWRD = CARD(IT:I)
      NP = NP + 1
C                                       get keyword value
      VALUES(2) = 0
      IF (ITYPE.EQ.1) THEN
         CALL GETNUM (CARD, 80, NP, X)
         IF (X.EQ.DBLANK) GO TO 985
         CALL RCOPY (NWDPDP, X, VALUES)
      ELSE IF (ITYPE.EQ.2) THEN
         CALL GETNUM (CARD, 80, NP, X)
         IF (X.EQ.DBLANK) GO TO 985
         VALUES(1) = X
      ELSE IF (ITYPE.EQ.3) THEN
         CALL GETSTR (CARD, 80, 8, NP, KEYVAL, IT)
         CALL CHR2H (8, KEYVAL, 1, VALUES)
      ELSE IF (ITYPE.EQ.4) THEN
         CALL GETNUM (CARD, 80, NP, X)
         IF (X.EQ.DBLANK) GO TO 985
         VALUES(1) = X
         IT = IROUND (VALUES(1))
         CALL RCOPY (1, IT, VALUES)
      ELSE
         CALL GETLG (CARD, 80, NP, IT)
         LVALUE = IT.GT.0
         CALL COPY (1, LVALUE, VALUES)
         END IF
      NUMKEY = 1
      LOCS = 1
      CALL CATKEY ('WRIT', VOL, CNO, KEYWRD, NUMKEY, LOCS, VALUES,
     *   ITYPE, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         IERR = 2
         END IF
      GO TO 990
C
 980  MSGTXT = 'KEYWORD = VALUE PAIR NOT FOUND IN CARD'
      CALL MSGWRT (6)
      GO TO 990
 985  MSGTXT = 'PUTCRD: BAD VALUE ON CARD'
      CALL MSGWRT (6)
      IERR = 1
C
 990  IF (IERR.NE.0) THEN
         MSGTXT = CARD
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CATKEY RETURNS ERROR',I4,' ON CARD')
      END
