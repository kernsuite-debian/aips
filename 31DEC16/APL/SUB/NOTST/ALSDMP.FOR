      SUBROUTINE ALSDMP (ALIST)
C-----------------------------------------------------------------------
C! Dump Associative list (ALIST)
C# Utility 
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
C   Public function.
C   Simple minded dump of ALIST to MSGWRT.  Only does first two
C   dimensions. 
C   See ALSINI for details of the package.
C   Inputs:
C      ALIST  I(*)  ALIST array
C   Outputs:
C-----------------------------------------------------------------------
      INTEGER   ALIST(*)
C
      CHARACTER ALNAME*32, KEYWRD*16, CTEMP*132
      INTEGER   TYPE, I1, I2, ADDR, INDEX, CINDEX, ITRIM, NCHAR,
     *   NDIM, DATOFF, DIM1, DIM2, LKEY, MAXL
      DOUBLE PRECISION DTEMP
      REAL      RTEMP
      LOGICAL   LTEMP
      INCLUDE 'INCS:PALIST.INC'
C-----------------------------------------------------------------------
      NDIM = ALIST(ALSNDM)
      DATOFF = ALIST(ALSDAT)
C                                       Validity test
      IF (ALIST(ALSCHK).NE.ALSCVL) THEN
         MSGTXT = 'ALSDMP: DAMAGED OR UNINITIALIZED ALIST'
         GO TO 990
         END IF
C                                       Name
      CALL H2CHR (32, 1, ALIST(ALSNAM), ALNAME)
      MSGTXT = 'ALSDMP: Dump of ALIST ' // ALNAME
      CALL MSGWRT (3)
C                                       Size
      WRITE (MSGTXT,1000) ALIST(ALSIZE)
      CALL MSGWRT (3)
C                                       Number used
      WRITE (MSGTXT,1001) ALIST(ALSEND)
      CALL MSGWRT (3)
C                                       Max. dimensionality
      WRITE (MSGTXT,1002) ALIST(ALSNDM)
      CALL MSGWRT (3)
      IF (ALIST(ALSNDM).GT.2) THEN
         MSGTXT = 'Note: Only first two dimensions dumped'
         CALL MSGWRT (3)
         END IF
C                                       Loop throught ALIST
      ADDR = ALIST(ALSBEG)
      IF (ADDR.EQ.ALIST(ALSEND)) THEN
C                                       Empty ALIST
         MSGTXT = 'ALIST is empty'
         CALL MSGWRT (3)
         GO TO 999
         END IF
C                                       Loop over entries
 50      CALL H2CHR (16, 1, ALIST(ADDR+ALSKWD), KEYWRD)
         LKEY = ITRIM (KEYWRD)
         DIM1 = MAX (1, ALIST(ADDR+ALSDIM))
         IF (NDIM.GT.1) THEN
            DIM2 = MAX (1, ALIST(ADDR+ALSDIM+1))
         ELSE
            DIM2 = 1
            END IF
         TYPE = ALIST(ADDR+ALSTYP)
         INDEX = ADDR + DATOFF
         CINDEX = 1
C                                       Characters special
         NCHAR = DIM1
         IF (TYPE.EQ.3) DIM1 = 1
         DO 200 I2 = 1,DIM2
            DO 100 I1 = 1,DIM1
C                                       By type
               IF (TYPE.EQ.1) THEN
C                                       Double precision
                  CALL COPY (NWDPDP, ALIST(INDEX), DTEMP)
                  WRITE (MSGTXT,1050,ERR=90) KEYWRD(1:LKEY), I1, I2,
     *               DTEMP
                  INDEX = INDEX + NWDPDP
               ELSE IF (TYPE.EQ.2) THEN
C                                       Single precision
                  CALL COPY (1, ALIST(INDEX), RTEMP)
                  WRITE (MSGTXT,1051,ERR=90) KEYWRD(1:LKEY), I1, I2,
     *               RTEMP
                  INDEX = INDEX + 1
               ELSE IF (TYPE.EQ.3) THEN
C                                       Character
                  CALL H2CHR (NCHAR, CINDEX, ALIST(INDEX), CTEMP)
                  MAXL = 80 - 16 - LKEY
                  MAXL = MIN (ITRIM(CTEMP), MAXL)
                  WRITE (MSGTXT,1052,ERR=90) KEYWRD(1:LKEY), I2,
     *               CTEMP(1:MAXL)
                  CINDEX = CINDEX + ALIST(ADDR+ALSDIM)
               ELSE IF (TYPE.EQ.4) THEN
C                                       Integer
                  WRITE (MSGTXT,1053,ERR=90) KEYWRD(1:LKEY), I1, I2,
     *               ALIST(INDEX)
                  INDEX = INDEX + 1
               ELSE IF (TYPE.EQ.5) THEN
C                                       Logical
                  CALL COPY (1, ALIST(INDEX), LTEMP)
                  WRITE (MSGTXT,1054,ERR=90) KEYWRD(1:LKEY), I1, I2,
     *               LTEMP
                  INDEX = INDEX + 1
               ELSE
C                                       Bad type
                  WRITE (MSGTXT,2000) TYPE
                  GO TO 990
                  END IF
 90            CALL MSGWRT (3)
 100           CONTINUE
 200        CONTINUE
C                                       End of list?
         ADDR = ALIST(ADDR)
         IF ((ADDR.GT.0) .AND. (ADDR.LT.ALIST(ALSIZE)) .AND.
     *      (ALIST(ADDR).GT.ADDR)) GO TO 50
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
      MSGTXT = 'PROBLEM DUMPING ALIST: ' // ALNAME
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ALSDMP: Number of words = ', I9)
 1001 FORMAT ('ALSDMP: Number of words used= ', I9)
 1002 FORMAT ('ALSDMP: Maximum dimensionality = ', I3)
 1050 FORMAT ('ALSDMP: ', A, '(',I3,',',I3,') =', 1PD25.15)
 1051 FORMAT ('ALSDMP: ', A, '(',I3,',',I3,') =', 1PE16.8)
 1052 FORMAT ('ALSDMP: ', A, '(',I3,') = ', A)
 1053 FORMAT ('ALSDMP: ', A, '(',I3,',',I3,') =', I12)
 1054 FORMAT ('ALSDMP: ', A, '(',I3,',',I3,') =', L1)
 2000 FORMAT ('ALSDMP: BAD DATA TYPE = ', I7)
      END
