      SUBROUTINE QCSQTR (CORNER, SIZE, ROW)
C-----------------------------------------------------------------------
C! Pseudo AP routine: inplace transpose of square, complex matrix.
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2006, 2012
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
C   Vectorizing compiler version
C   QCSQTR does an inplace transpose of square matrices of complex
C   values.
C   Inputs:
C      CORNER  I  AP location of first corner of matrix encountered.
C      SIZE    I  Size (number of reals) of a row or column.
C      ROW     I  Number of locations in AP between beginnings
C                 of the rows.
C-----------------------------------------------------------------------
      INTEGER   CORNER, SIZE, ROW
C
      INTEGER   II, JK, LIMIT1, LIMIT2
      LONGINT   JCORN, INDEX1, INDEX2
      DOUBLE PRECISION RTEMP, ITEMP
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                       Address to 1-rel.
      JCORN = CORNER + PSAPOF
C                                        Begin loop thru rows
      LIMIT1 = SIZE - 1
      INCLUDE 'INCS:ZVND.INC'
      DO 100 II = 1,LIMIT1
C                                        Loop down row exchanging
C                                        values with col.
         INDEX1 = JCORN
         INDEX2 = JCORN
         LIMIT2 = SIZE - II + 1
      INCLUDE 'INCS:ZVND.INC'
         DO 50 JK = 1,LIMIT2
            RTEMP = APCORE(INDEX1)
            ITEMP = APCORE(INDEX1+1)
            APCORE(INDEX1) = APCORE(INDEX2)
            APCORE(INDEX1+1) = APCORE(INDEX2+1)
            APCORE(INDEX2) = RTEMP
            APCORE(INDEX2+1) = ITEMP
            INDEX1 = INDEX1 + 2
            INDEX2 = INDEX2 + ROW
 50         CONTINUE
C                                        Update current corner.
         JCORN = JCORN + ROW + 2
 100     CONTINUE
C
 999  RETURN
      END
