      SUBROUTINE GTPAIR (IPAIR, TABBUF, NAME, VALUE, ITYPE)
C-----------------------------------------------------------------------
C! Returns specified Keyword-value pair from an open AIPS table
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   This routine will get a given keyword value pair from an open table,
C   return the name, type (as an integer code) and value.
C   Inputs:
C      IPAIR   I      The number of the keyword/value pair to retrieve.
C   In/Out:
C      TABBUF  I(768)   Table data buffers and I/O control.
C   Outputs:
C      NAME    C*8    The keyword name.
C      DVALUE  D      The value of the pair. Logicals are coded as -1.0
C                     for false, 1.0 for true. Characters are HOLLERITH
C      ITYPE   I      The type code : 0 => Bad value.
C                        1 => Double
C                        2 => Real
C                        3 => Character
C                        4 => Integer
C                        5 => logical
C-----------------------------------------------------------------------
      CHARACTER NAME*8
      INTEGER   IPAIR, TABBUF(768), ITYPE
      DOUBLE PRECISION VALUE
C
      DOUBLE PRECISION DVALUE
      HOLLERITH HVALUE(3)
      REAL      RESULT(5)
      INTEGER   IVALUE(3), KEYVAL, IERR
      LOGICAL   LVALUE(3)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RESULT(3), DVALUE, IVALUE, LVALUE, HVALUE)
      DATA KEYVAL /5/
C-----------------------------------------------------------------------
      ITYPE = 0
C                                       Range error for IPAIR.
      IF ((IPAIR.GT.0) .AND. (IPAIR.LE.TABBUF(53))) GO TO 100
         WRITE (MSGTXT,1000) IPAIR, TABBUF(53)
         CALL MSGWRT (7)
         GO TO 999
C
 100  CALL TABIO ('READ', KEYVAL, IPAIR, RESULT, TABBUF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get name and type code.
      CALL H2CHR (8, 1, RESULT, NAME)
      ITYPE = IVALUE(3)
      IF ((ITYPE.LE.0) .OR. (ITYPE.GT.5)) GO TO 900
      GO TO (110, 120, 130, 140, 150), ITYPE
C                                       Double precision
 110     VALUE = DVALUE
         GO TO 999
C                                       Real.
 120     VALUE = RESULT(3)
         GO TO 999
C                                       Char.
 130     CALL CHCOPY (8, 1, RESULT(3), 1, VALUE)
         GO TO 999
C                                       Integer
 140     VALUE = IVALUE(1)
         GO TO 999
C                                       Logical
 150     VALUE = -1.0D0
         IF (LVALUE(1)) VALUE = 1.0D0
         GO TO 999
C                                       Bad type
 900  WRITE (MSGTXT,1900) ITYPE
      CALL MSGWRT (7)
      ITYPE = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GTPAIR: RANGE ERROR FOR PAIR. VALUE, MAX = ',2I8)
 1900 FORMAT ('GTPAIR: INVALID TYPE CODE : ',I4)
      END
