      SUBROUTINE REMETS (PREFIX, X)
C-----------------------------------------------------------------------
C! given a METSCA prefix, return the scaling parameter
C# Utility Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 2009
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
C   METSCA scales the real value X to a value 0.99995 - 999.95 and picks
C   a standard metric prefix to match.  REMETS is used to convert that
C   prefix back into a multiplier
C   In:
C      PREFIX  C*5   metric prefix
C   Outputs:
C      X       R     multiplier: multiply data by this to get scaled
C                    value
C-----------------------------------------------------------------------
      CHARACTER PREFIX*5
      REAL      X
C                                       Declare max prefix number,blank
      INTEGER   MXPREF, NF
      PARAMETER (MXPREF=17,NF=9)
      CHARACTER PLIST(MXPREF)*5, PREFXC*5
      INTEGER   I, J
      DATA PLIST /'YOCTO', 'ZEPTO', 'ATTO ', 'FEMTO', 'PICO ', 'NANO ',
     *   'MICRO', 'MILLI', '     ', 'KILO ', 'MEGA ', 'GIGA ', 'TERA ',
     *   'PETA ', 'EXA  ', 'ZETTA', 'YOTTA'/
C-----------------------------------------------------------------------
      X = 1.0
C                                       ignore case
      PREFXC = PREFIX
      CALL CHLTOU (5, PREFXC)
      DO 10 I = 1,MXPREF
         IF (PREFXC.EQ.PLIST(I)) THEN
            J = (NF - I) * 3
            X = 10.0**J
            END IF
 10      CONTINUE
C
 999  RETURN
      END
