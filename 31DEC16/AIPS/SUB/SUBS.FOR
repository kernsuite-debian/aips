      SUBROUTINE SUBS
C-----------------------------------------------------------------------
C! converts variable with subscript to the appropriate scalar
C# POPS-lang
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   SUBS finds the value of a subscripted variable and places the
C   value (or string pointers) on the stack.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   I, IDATA, INDEX, IOFF, ISIZE, J, N, NDELTA, NDIM, POTERR
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DIO.INC'
      DATA PRGNAM /'SUBS  '/
C-----------------------------------------------------------------------
C                  S U B S   O P E R A T O R                     98
C-----------------------------------------------------------------------
      POTERR = 8
      IF (SP.LT.4) GO TO 980
      IDATA = STACK(SP-2)
      TAG = STACK(SP-1)
      SYTYPE = STACK(SP-3)
      N = K(IDATA)
      NDIM = K(IDATA+1)
      IF ((SYTYPE.NE.2) .AND. (SYTYPE.NE.7)) GO TO 980
      POTERR = 6
      SP = SP - 4
      INDEX = 0
C                            get field length data if character string
      IF (SYTYPE.NE.7) GO TO 15
         NDIM = NDIM - 1
         ISIZE = K(IDATA + 3)
         IDATA = IDATA + 2
 15   I = SP - NDIM + 1
      SP = I
      NDELTA = 1
      DO 20 J = 1,NDIM
         IDATA = IDATA + 2
         IF (STACK(I).EQ.2) THEN
            POTERR = 8
            GO TO 980
            END IF
         IOFF = V(I) - K(IDATA)
         IF ((IOFF.LT.0) .OR. (IOFF.GE.K(IDATA+1))) GO TO 980
         INDEX = INDEX + NDELTA*IOFF
         NDELTA = NDELTA * K(IDATA+1)
         I = I + 1
 20      CONTINUE
C                            put value of array location on stack
      IF (SYTYPE.EQ.7) GO TO 30
         IF (INDEX.GT.N) GO TO 980
         INDEX = TAG + INDEX
         STACK(SP) = INDEX
         V(SP) = C(INDEX)
         GO TO 999
C                            put string subfield parameters onto stack
 30   CONTINUE
         INDEX = INDEX * ((ISIZE+3) / 4)
         IF (INDEX.GT.N) GO TO 980
         INDEX = INDEX + TAG
         STACK(SP) = 8
         STACK(SP+1) = ISIZE
         STACK(SP+2) = INDEX
         STACK(SP+3) = 2
         SP = SP + 3
         GO TO 999
C                                 Error return.
 980  ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
