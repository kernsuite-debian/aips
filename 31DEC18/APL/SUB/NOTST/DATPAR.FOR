      SUBROUTINE DATPAR (TIME, ISUB, IERR)
C-----------------------------------------------------------------------
C! updates the parallactic angles if needed
C# Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2017
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
      INTEGER   ISUB, IERR
      REAL      TIME
C
      INTEGER   LUN, I
      REAL      TINC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DPDC.INC'
C                                       Time increment (days) to update
C                                       correction matrices (0.1 sec.)
      DATA TINC /1.15E-6/
      DATA LUN /49/
C-----------------------------------------------------------------------
C                                       subarray changing?
      IF (ISUB.NE.PARSUB) THEN
         CALL GETANT (IUDISK, IUCNO, ISUB, CATUV, ANBUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'GETTING UPDATED ANTENNA INFO'
            GO TO 990
            END IF
         PARSUB = ISUB
         END IF
C                                       See if parallactic angles
C                                       current
      IF ((CURSOU.NE.PARSOU) .OR. (ABS(TIME-PARTIM).GT.TINC)) THEN
C                                       Source info
         IF ((CURSOU.NE.PARSOU) .OR. (CURSOU.NE.IDSOUR)) THEN
            CALL GETSOU (CURSOU, IUDISK, IUCNO, CATUV, LUN, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'GETTING UPDATED SOURCE INFO'
               GO TO 990
               END IF
            PARSOU = CURSOU
            END IF
C                                       Parallactic angles
         CALL PARANG (TIME, PANGLE)
         DO 10 I = 1,NSTNS
            PARAGL(1,I) = COS (PANGLE(I))
            PARAGL(2,I) = -SIN (PANGLE(I))
 10         CONTINUE
C                                       Update par. angle time
         PARTIM = TIME
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DATPAR ERROR',I5,' ON ',A)
      END
