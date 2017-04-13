      SUBROUTINE QUCONJ (DIR, UV, VIS, CNJPTR, N, LREC)
C-----------------------------------------------------------------------
C! Makes data buffer be all U > 0
C# ap-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2008, 2012
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
C   Converts a buffer of UV data so that all samples are at V >= 0.
C   Input:
C      DIR      I   1 => convert, 2 => undo conversion
C      UV       I   Base address of vis buffer
C      VIS      I   Base address of data in buufer
C      CNJPTR   I   Base address of pointer array makes which have been
C                      conjugated
C      N        I   Number of input vis records
C      LREC     I   Increment in AP between vis records
C-----------------------------------------------------------------------
      INTEGER   DIR, UV, VIS, CNJPTR, N, LREC
C
      LONGINT   JU, JD, JP, JI
      INTEGER   NP, NV, I, J
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      JU = UV + PSAPOF
      JD = VIS + PSAPOF
      JP = CNJPTR + PSAPOF
      NP = VIS - UV
      NV = (LREC - NP) / 3
C                                       conjugate
      IF (DIR.NE.2) THEN
         DO 20 I = 1,N
            APCORE(JP) = 1.0D0
            IF (APCORE(JU).LT.0.0) THEN
               APCORE(JP) = -1.0D0
               APCORE(JU) = -APCORE(JU)
               APCORE(JU+1) = -APCORE(JU+1)
               APCORE(JU+2) = -APCORE(JU+2)
               JI = JD + 1
               DO 10 J = 1,NV
                  APCORE(JI) = -APCORE(JI)
                  JI = JI + 3
 10               CONTINUE
               END IF
            JP = JP + 1
            JU = JU + LREC
            JD = JD + LREC
 20         CONTINUE
C                                       undo conjugate
      ELSE
         DO 40 I = 1,N
            IF (APCORE(JP).LT.0.0D0) THEN
               APCORE(JU) = -APCORE(JU)
               APCORE(JU+1) = -APCORE(JU+1)
               APCORE(JU+2) = -APCORE(JU+2)
               JI = JD + 1
               DO 30 J = 1,NV
                  APCORE(JI) = -APCORE(JI)
                  JI = JI + 3
 30               CONTINUE
               END IF
            JP = JP + 1
            JU = JU + LREC
            JD = JD + LREC
 40         CONTINUE
         END IF
C
 999  RETURN
      END
