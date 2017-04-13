      SUBROUTINE YLUT (OP, CHANNL, COLOR, VRTRTC, LUT, IERR)
C-----------------------------------------------------------------------
C! read/write channel-based look-up-table
C# Y1 TV-basic
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
C   YLUT reads/writes full channel look up tables to TV.
C   Inputs:
C      OP      C*4    'READ' from TV, 'WRIT' to TV
C      CHANNL  I      channel select bit mask
C      COLOR   I      color select bit mask (RGB <-> 421)
C      VRTRTC  L      T => do it only during vertical retrace
C   In/Out:
C      LUT     I(*)   look up table (dimension = MAXINT+1, values to
C                     LUTOUT are used)
C   Out:
C      IERR    I      error code of Z...XF : 0 => ok, 2 => input error
C   Modified NOV-83 for DEANZA by W. Jaffe
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHANNL, COLOR, LUT(*), IERR
      LOGICAL   VRTRTC
C
      INCLUDE 'INCS:YDEA.INC'
      INTEGER   LCHAN, IP, ITEMP, NB, CMREG, AG0, ICHAN, LUTREG, GRALUT,
     *   CMASK0, ZAND
      LOGICAL   T, UPDATE
      INCLUDE 'INCS:DTVC.INC'
      DATA T /.TRUE./
      DATA AG0, CMREG, LUTREG, NB /0, 32, 3, 512/
C                                       Gray scale LUT control word,
C                                       LUT enabled, address 0 = Z1000
      DATA GRALUT /4096/
C                                       Channel mask #0 equals Interface
C                                       register 32 (=CMREG)
      DATA CMASK0 /0/
C-----------------------------------------------------------------------
C                                       I dont reset graphics memory LUT
      ITEMP = 2 ** NGRAY - 1
      LCHAN = ZAND (CHANNL, ITEMP)
      IF (OP.EQ.'WRIT') THEN
C                                       Set up Channel mask
         UPDATE = .FALSE.
         CALL YTCOMP (YBUFF(CMR0), LCHAN, UPDATE)
         IF (UPDATE) THEN
            CALL ZDEAXF ('LR  ', LCHAN, 2, 0, CMREG, T, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                       Set up ITT/LUT address/control,
C                                       Memory register #3
         CALL ZDEAXF ('BMC ', GRALUT, 2, CMASK0, LUTREG, T, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Set soft registers to agree
         DO 10 IP = 1,NGRAY
            ITEMP = 2 ** (IP-1)
            IF (ZAND (LCHAN, ITEMP).NE.0) YBUFF(MEMLUT+4*(IP-1)) =
     *         GRALUT
 10         CONTINUE
C                                       Broadcast array: Byte array
C                                       group 0 points to memory LUTs.
C                                       CMR0 selects channels
         CALL ZDEAXF ('BPA ', LUT, NB, CMASK0, AG0, T, IERR)
      ELSE IF (OP.EQ.'READ') THEN
C                                       Can only read one channel
         IF (LCHAN.EQ.0) THEN
            IERR = 2
         ELSE
            CALL YLOWON (LCHAN, ICHAN)
            CALL ZDEAXF ('RPA ', LUT, NB, ICHAN, AG0, T, IERR)
            END IF
      ELSE
         IERR = 2
         END IF
C
 999  RETURN
      END
