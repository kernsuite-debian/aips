      SUBROUTINE YZERO (CHAN, IERR)
C-----------------------------------------------------------------------
C! fill a TV memory plane with zeros
C# Y1 TV-util
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
C   YZERO fills an TV TV memory plane with zeros the fast way.
C   Inputs:
C      CHAN   I     channel # ( 1 - NGRAY+NGRAPH), 0 => all
C   Outputs:
C      IERR   I     error code of Z...XF: 0 - ok, 2 - input error
C   Modified Nov-83 by Jaffe for DeAnza
C-----------------------------------------------------------------------
      INTEGER   CHAN, IERR
C
      INCLUDE 'INCS:YDEA.INC'
      LOGICAL   T, F, GRAY, MEMUP, UPDATE
      INTEGER   NCON, MEMB, BCHAN, BITS, FORE, ITEMP, IDUM
      INCLUDE 'INCS:DTVC.INC'
      DATA T, F /.TRUE.,.FALSE./
C                                       Mem reg #2 holds bit pattern
      DATA MEMB /2/
C                                       Reg #10 is control word
      DATA NCON, FORE /10, 0/
C-----------------------------------------------------------------------
      IERR = 2
      IF ((CHAN.LT.0) .OR. (CHAN.GT.NGRAY+NGRAPH)) GO TO 999
      GRAY = CHAN.LE.NGRAY
      IF (GRAY) THEN
         BCHAN = 2 ** (CHAN - 1)
      ELSE
         BCHAN = 2 ** NGRAY
         BITS  = 2 ** (CHAN - NGRAY - 1)
         END IF
C                                       Update interface registers
      UPDATE = .FALSE.
C                                       Load channel select reg
      CALL YTCOMP (YBUFF(CMR0), BCHAN, UPDATE)
C                                       Load control & resolution
      CALL YTCOMP (YBUFF(CONTRL), 0, UPDATE)
C                                       Load foreground reg
      CALL YTCOMP (YBUFF(FG), FORE, UPDATE)
C                                       Load X,Y reg
      CALL YTCOMP (YBUFF(XR), 0, UPDATE)
      CALL YTCOMP (YBUFF(YR), 0, UPDATE)
      ITEMP = MAXXTV(1) - 1
      CALL YTCOMP (YBUFF(XMAX), ITEMP, UPDATE)
      CALL YTCOMP (YBUFF(DX), YBUFF(XMAX), UPDATE)
      ITEMP = MAXXTV(2) - 1
      CALL YTCOMP (YBUFF(YMAX), ITEMP, UPDATE)
      CALL YTCOMP (YBUFF(DY), YBUFF(YMAX), UPDATE)
      CALL YTCOMP (YBUFF(XMIN), 0, UPDATE)
      CALL YTCOMP (YBUFF(YMIN), 0, UPDATE)
C                                       Rewrite registers if necessary
      IF (UPDATE) THEN
         CALL ZDEAXF ('LR  ', YBUFF(INTREG), IRBYTE, 1, NCON, T, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Update Memory registers
C                                       with bitpattern for graphics
      IF (.NOT.GRAY) THEN
         MEMUP = .FALSE.
         CALL YTCOMP (YBUFF(BITPL+4*NGRAY), BITS, MEMUP)
         IF (MEMUP) THEN
            CALL ZDEAXF ('BMC ', BITS, 2, 0, MEMB, T, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
C                                       Do it
C                                       WR with FORE fills
C                                       rectangle with foreground
C                                       register constant
      IDUM = 0
      CALL ZDEAXF ('WR  ', IDUM, 2, 0, FORE ,T, IERR)
C
 999  RETURN
      END
