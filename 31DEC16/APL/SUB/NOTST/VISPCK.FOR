      SUBROUTINE VISPCK (NRPARM, TBUFF, BUFF)
C-----------------------------------------------------------------------
C! Compresses a visibility record.
C# EXT-appl Calibration
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
C   Routine to compress a visibility record. A utility routine
C   to make life a little easier.
C   Inputs:
C     NRPARM      I      # random parameters
C     TBUFF(*)    R      Uncompressed visibility
C   Inputs from common /CMPPRM/
C     MLOCWT      I      Location of weight in random parameter array
C     NDECMP      I      Number of entries in DECMP
C     DECMP       I(2,*) (1,*) = number of compressed correlator values
C                        (2,*) = 0-rel offset in vis data.
C                        (from beginning of vis data NOT ran. parms.)
C
C   Outputs:
C     BUFF(*)     R      Compressed visibility
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER NRPARM
      REAL    TBUFF(*), BUFF(*)
C
      INCLUDE 'INCS:DCMP.INC'
      INTEGER I
C-----------------------------------------------------------------------
      CALL RCOPY (NRPARM, TBUFF, BUFF)
      DO 100 I = 1, NDECMP
         CALL ZUVPAK (DECMP(1,I), TBUFF(1+NRPARM), BUFF(1+MLOCWT),
     *      BUFF(1+NRPARM))
 100     CONTINUE
      RETURN
      END
