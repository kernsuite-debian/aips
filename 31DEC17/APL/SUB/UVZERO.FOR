      SUBROUTINE UVZERO( WTOFF, LVIS, U, NCHGRD, BUFF)
C-----------------------------------------------------------------------
C! Zero's a single VIS rec, except U value to maintain sorting.
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
C UV ZERO Buffer: zeros one UV point and assigns weight to zero.
C Band width synthesis is handled by this routine
C Inputs:
C  WTOFF      I     Offset to UV Weight
C  LVIS       I     Length of Visibility record
C  U          R     U value of output Vis record.
C  NCHGRD     I     Number of channels to Grid
C Output:
C  BUFF       R(*)  Visibility record
C-----------------------------------------------------------------------
      REAL U, BUFF(*)
      INTEGER WTOFF, LVIS, NCHGRD
C-----------------------------------------------------------------------
      INTEGER JJ, JNDX
C-----------------------------------------------------------------------
C                                       reset U and V
      BUFF(1)   = U
      BUFF(2) = 0.0
C                                       set index to weight
      JNDX = 1 + WTOFF
C                                       if weight with random parms
      IF (LVIS.LE.2) THEN
C                                       set weight once in random parm.
         BUFF(JNDX) = 0.0
      ELSE
C                                       weight in vis recs,
C                                       for all channels
         DO 130 JJ = 1,NCHGRD
C                                       set weight in each channel
            BUFF(JNDX) = 0.0
C                                       update pointer for next chan.
            JNDX = JNDX + LVIS
 130        CONTINUE
         END IF
      RETURN
C-----------------------------------------------------------------------
      END

