      SUBROUTINE UVSETB( VSOFF, LVIS, UMAX, UMIN, USCALE, NCHGRD, BUFF)
C-----------------------------------------------------------------------
C! Set Beam values for a visibility, Real=1, Imaginary=0.
C# UV-util
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
C UV SET Beam: Sets UV point values for creating a BEAM.
C Weight is not checked.  Also checks sort on U and prints error
C messages for miss sorted data.
C Band width synthesis is handled by this routine
C Inputs:
C  VSOFF      I     Offset to UV Visibility record
C  LVIS       I     Length of Visibility record
C  UMAX       R     MAx U value allowed for data.
C  UMIN       R     Min U value allowed for data.
C  UMAX       R     U value of output Vis record.
C  USCALE     R     U scale value to kilo lambda
C  NCHGRD     I     Number of channels to Grid
C Output:
C  BUFF       R(*)  Visibility record
C-----------------------------------------------------------------------
      INTEGER VSOFF, LVIS, NCHGRD
      REAL UMAX, UMIN, USCALE, BUFF(*)
C-----------------------------------------------------------------------
      INTEGER JJ, JNDX, BITCNT
      REAL UU1, UU2, UU3
      INCLUDE 'INCS:DMSG.INC'
      SAVE BITCNT
      DATA BITCNT/0/
C-----------------------------------------------------------------------
C                                       set index to start of vis rec
C                                       for first channel to average
      JNDX = 1 + VSOFF
C                                       for all channels to average
      DO 145 JJ = 1,NCHGRD
C                                       BEAM replace data with (1,0)
         BUFF(JNDX)   = 1.0
         BUFF(JNDX+1) = 0.0
C                                       index to start of next channel
         JNDX = JNDX + LVIS
 145     CONTINUE
C                                       Check if data sorted by U
      IF (ABS (BUFF(1)).GT.UMAX) THEN
C                                       print only a few error messages
         IF (BITCNT.LE.10) THEN
            BITCNT = BITCNT + 1
            UU1 = BUFF(1) * USCALE
            UU2 = UMIN * USCALE
            UU3 = UMAX * USCALE
            WRITE (MSGTXT,1125) UU1, UU2, UU3
            CALL MSGWRT (6)
            END IF
         END IF
      RETURN
C-----------------------------------------------------------------------
 1125 FORMAT ('IU=ABS(',F10.3,') NOT IN RANGE ',2F8.2,', MISSORTED')
      END

