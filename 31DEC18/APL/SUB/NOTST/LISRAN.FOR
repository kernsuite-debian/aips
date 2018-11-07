      SUBROUTINE LISRAN (OUTRAN, ANOTA)
C-----------------------------------------------------------------------
C! Returns the list of the random parameters
C# UTIL
C-----------------------------------------------------------------------
C;  Copyright (C) 1998-1999, 2015
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
C   Returns the list of the random parameters and relevant array
C   of logicals - whether the parameter is (is not) averaged
C   Input from common:
C      NRPARM   I        Number of random parameters
C      CATBLK   I(256)   Header
C   Outputs:
C      OUTRAN   C(*)*8   Array of random parameters
C      ANOTA    I(*)     2 => the parameter is averaged
C                        1 => the parameter is summed
C                        0 => the parameter is copied intact
C                        -1 => parameter unknown
C-----------------------------------------------------------------------
      INTEGER   ANOTA(*)
      CHARACTER OUTRAN(*)*8
C
      INTEGER   NUMPAR
      PARAMETER (NUMPAR = 18)
C
      INTEGER   IPARM, INDPAR, NRPRIN, I, J
      CHARACTER CTEMP*8, NAMPAR(NUMPAR)*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA NAMPAR / 'UU-L    ', 'VV-L    ', 'WW-L    ', 'TIME1   ',
     *   'BASELINE', 'SOURCE  ', 'FREQSEL ', 'INTTIM  ', 'GATEID  ',
     *   'CORR-ID ', 'FILTER  ', 'DATE    ', 'WEIGHT  ', 'SCALE   ',
     *   'REMOVED ', 'SUBARRAY', 'ANTENNA1', 'ANTENNA2'/
C-----------------------------------------------------------------------
      NRPRIN = NRPARM
      NRPRIN = MIN (NRPRIN, KIPTPN)
      DO 40 IPARM = 1,NRPRIN
         ANOTA(IPARM) = -1
         INDPAR = KHPTP + 2 * (IPARM - 1)
         CALL H2CHR (8, 1, CATH(INDPAR), CTEMP)
         OUTRAN(IPARM) = CTEMP
         DO 20 I = 1,NUMPAR
            J = 8
            IF (I.LT.4) J = 4
            IF (CTEMP(:J).EQ.NAMPAR(I)(:J)) THEN
               ANOTA(IPARM) = 0
               IF (I.LE.4) ANOTA(IPARM) = 2
               IF (I.EQ.8) ANOTA(IPARM) = 1
               IF (I.EQ.13) ANOTA(IPARM) = 1
               END IF
 20         CONTINUE
         IF (ANOTA(IPARM).LT.0) THEN
            WRITE (MSGTXT,1000) OUTRAN(IPARM)
            CALL MSGWRT (6)
            END IF
 40      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WARNING: ',A,' NOT RECOGNIZED, WILL NOT BE AVERAGED')
      END
