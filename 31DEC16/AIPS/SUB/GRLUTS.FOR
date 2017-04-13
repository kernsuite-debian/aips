      SUBROUTINE GRLUTS (PMAX, IG, IC, IL, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! interactive piecewise linear LUT using graphic plane and cursor
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2015
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
C   GRLUTS implements an interactive piecewise linear LUT dialed in
C   by the user using the cursor and a graphics plane.
C   Inputs:
C      PMAX    I          Max number segments: 0 => any number
C      IG      I          Graphics plane to use (1 - NGRAPH)
C      IC      I          TV channels to set LUT (bit mask)
C      IL      I          Colors to set LUT (bit mask)
C   Output:
C      SCRTCH  I(2048)    Scratch buffer (IVAS needs 2048 min)
C      IERR    I          Error code
C-----------------------------------------------------------------------
      INTEGER   PMAX, IG, IC, IL, SCRTCH(*), IERR
C
      INTEGER   IGC, I, J, IX0, IX1, IY0, IY1, IX(129), IY(129), IPO,
     *   QUAD, IBUT, IP, MP, ITW(3), LZOOM(3), JERR, MX, IXL, IYL, IXH,
     *   IYH, NP, ISZX, ISZY
      LOGICAL   T, F, DOIT, ONGR, DOON, DOLIM, FIRST
      REAL      PPOS(2), RPOS(2), DLIM, XSCALE, YSCALE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Check parms
      IERR = 2
      IF ((IG.LT.1) .OR. (IG.GT.NGRAPH)) GO TO 999
      IF ((IC.LT.1) .OR. (IC.GE.2**NGRAY)) GO TO 999
      IF ((IL.LE.0) .OR. (IL.GT.7)) GO TO 999
      DOLIM = (PMAX.GE.3) .AND. (PMAX.LE.127)
      FIRST = DOLIM
C                                       Off any zoom
      CALL YHOLD ('ONNN', IERR)
      CALL COPY (3, TVZOOM, LZOOM)
      CALL YZOOMC (0, 1, 1, T, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Clear graphics
      IGC = IG + NGRAY
      CALL YZERO (IGC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       On graphics
      I = 2 ** (IGC-1)
      ONGR = MOD (TVLIMG(1)/I, 2) .EQ. 1
      IPO = 1
      CALL YSLECT ('ONNN', IGC, 0, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Coordinates
      ISZX = WINDTV(3) - WINDTV(1) + 1
      ISZY = WINDTV(4) - WINDTV(2) + 1
      XSCALE = REAL (ISZX) / (MAXINT+1.0) / 2.0
      YSCALE = REAL (ISZY) / (LUTOUT+1.0) / 2.0
      IX0 = (ISZX - (MAXINT+1) * XSCALE) / 2 + WINDTV(1) - 1
      IY0 = (ISZY - (LUTOUT+1) * YSCALE) / 2 + WINDTV(2) - 1
      IX1 = IX0 + MAXINT * XSCALE
      IY1 = IY0 + LUTOUT * YSCALE
C                                       Border
      IX(1) = IX0 - 1
      IX(2) = IX1 + 1
      IX(3) = IX(2)
      IX(4) = IX(1)
      IX(5) = IX(1)
      IY(1) = IY0 - 1
      IY(2) = IY(1)
      IY(3) = IY1 + 1
      IY(4) = IY(3)
      IY(5) = IY(1)
      CALL IMVECT ('ONNN', IGC, 5, IX, IY, SCRTCH, IERR)
      IPO = 2
      IF (IERR.NE.0) GO TO 900
C                                       Ticks
      IY(1) = IY0 - 6
      IY(2) = IY(1) + 10
      IY(3) = IY1 + 6
      IY(4) = IY(3) - 10
      IX(5) = IX0 - 6
      IX(6) = IX(5) + 10
      IX(7) = IX1 + 6
      IX(8) = IX(7) - 10
      DO 20 I = 1,3
         IX(1) = (I / 4.0) * (IX1 - IX0) + IX0
         IX(2) = IX(1)
         IX(3) = IX(1)
         IX(4) = IX(1)
         IY(5) = (I / 4.0) * (IY1 - IY0) + IY0
         IY(6) = IY(5)
         IY(7) = IY(5)
         IY(8) = IY(5)
         DO 15 J = 1,8,2
            CALL IMVECT ('ONNN', IGC, 2, IX(J), IY(J), SCRTCH, IERR)
            IPO = 3
            IF (IERR.NE.0) GO TO 900
 15         CONTINUE
 20      CONTINUE
C                                       Labels
      IX(1) = (IX1 + IX0) / 2
      IY(1) = IY0 - 6 - CSIZTV(2) * 1.2
      CALL IMCHAR (IGC, IX, IY, 0, 1, 'INPUT', SCRTCH, IERR)
      IPO = 4
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 900
      IX(1) = IX0 - 6 - CSIZTV(1) * 1.2
      IY(1) = (IY1 + IY0) / 2
      CALL IMCHAR (IGC, IX, IY, 3, 1, 'OUTPUT', SCRTCH, IERR)
      IPO = 5
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 900
C                                       Init for interaction
      IX(1) = IX0
      IY(1) = IY0
      IX(2) = IX(1)
      IY(2) = IY(1)
      IX(3) = IX1
      IY(3) = IY1
      QUAD = -1
      IP = 2
      MP = 3
      CALL ZTIME (ITW)
C                                       Basic instructions
 30   IF (.NOT.DOLIM) GO TO 40
         IF (FIRST) WRITE (MSGTXT,1030)
         IF (.NOT.FIRST) WRITE (MSGTXT,1031)
         CALL MSGWRT (1)
         IF (FIRST) GO TO 45
            WRITE (MSGTXT,1032)
            CALL MSGWRT (1)
            GO TO 45
 40   CONTINUE
         WRITE (MSGTXT,1040)
         CALL MSGWRT (1)
         WRITE (MSGTXT,1041)
         CALL MSGWRT (1)
         WRITE (MSGTXT,1032)
         CALL MSGWRT (1)
C                                       reposition & on cursor
 45   RPOS(1) = IX(IP)
      RPOS(2) = IY(IP)
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL YHOLD ('OFFF', IERR)
      CALL YCURSE ('ONNN', FIRST, F, RPOS, QUAD, IBUT, IERR)
      IPO = 6000 + IP
      IF (IERR.NE.0) GO TO 900
      IF (IBUT.NE.0) GO TO 55
C                                       read loop
 50   CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
         IPO = 7000 + IP
         IF (IERR.NE.0) GO TO 900
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
         IF (.NOT.DOIT) GO TO 50
C                                       Erase 2 segments
 55      CALL YHOLD ('ONNN', IERR)
         CALL IMVECT ('OFFF', IGC, 3, IX(IP-1), IY(IP-1), SCRTCH, IERR)
         IPO = 8000 + IP
         IF (IERR.NE.0) GO TO 900
C                                       new point
         IX(IP) = RPOS(1) + 0.01
         IY(IP) = RPOS(2) + 0.01
         DOON = (IX(IP).LT.IX(IP-1)) .OR. (IX(IP).GT.IX(IP+1)) .OR.
     *      (IY(IP).LT.IY0) .OR. (IY(IP).GT.IY1)
         IF (DOON) THEN
            IX(IP) = MAX (IX(IP-1), MIN (IX(IP), IX(IP+1)))
            IY(IP) = MAX (IY0, MIN (IY(IP), IY1))
            END IF
C                                       draw new segments
         CALL IMVECT ('ONNN', IGC, 3, IX(IP-1), IY(IP-1), SCRTCH, IERR)
         IPO = 9000 + IP
         IF (IERR.NE.0) GO TO 900
C                                       Compute new LUT
         NP = 1
         MX = MP - 1
         IXH = (IX(1) - IX0) / XSCALE
         IYH = (IY(1) - IY0) / YSCALE
         DO 70 I = 1,MX
            IXL = IXH
            IYL = IYH
            IXH = (IX(I+1) - IX0) / XSCALE
            IYH = (IY(I+1) - IY0) / YSCALE
            CALL STRLIN (IXL, IYL, IXH, IYH, SCRTCH)
 70         CONTINUE
         SCRTCH(1) = 0
         DO 75 I = 1,MAXINT
            SCRTCH(I+1) = MAX (0, MIN (LUTOUT, SCRTCH(I+1)))
 75         CONTINUE
         CALL YLUT ('WRIT', IC, IL, T, SCRTCH, IERR)
         IPO = 10000 + IP
         IF (IERR.NE.0) GO TO 900
         CALL YHOLD ('OFFF', IERR)
C                                       Respond to buttons
         IF ((IBUT.GT.7) .AND. (.NOT.FIRST)) GO TO 900
         IF ((IBUT.EQ.0) .AND. (DOON)) GO TO 45
         IF (IBUT.EQ.0) GO TO 50
C                                       Create new vertex
         IF ((DOLIM) .AND. (MP.EQ.PMAX)) FIRST = F
         IF ((DOLIM) .AND. (.NOT.FIRST)) GO TO 100
         IF ((IBUT.GT.3) .AND. (MP.GT.3) .AND. (.NOT.DOLIM)) GO TO 100
            IX(MP+1) = IX(MP)
            IY(MP+1) = IY(MP)
            IX(MP) = IX(MP-1)
            IY(MP) = IY(MP-1)
            IP = MP
            MP = MP + 1
            IF (MP.EQ.129) DOLIM = T
            GO TO 45
C                                       User points at old vertex
 100     WRITE (MSGTXT,1100)
         CALL MSGWRT (1)
         CALL YCURSE ('READ', T, F, RPOS, QUAD, IBUT, IERR)
         IPO = 11000 + IP
         IF (IERR.NE.0) GO TO 900
         IF (IBUT.GT.7) GO TO 900
C                                       Find nearest
         DLIM = 0.5
         NP = MP - 1
 105     DO 110 I = 2,NP
            IF ((ABS(RPOS(1)-IX(I)).LE.DLIM) .AND. (ABS(RPOS(2)-
     *         IY(I)).LE.DLIM)) GO TO 120
 110        CONTINUE
         DLIM = DLIM + 1.5
         IF (DLIM.LE.6.0) GO TO 105
         GO TO 100
C                                       Set point and do it
 120     IP = I
         GO TO 30
C                                       DONE !!!
 900  CALL YCURSE ('OFFF', F, F, RPOS, QUAD, IBUT, JERR)
      CALL YZOOMC (LZOOM(1), LZOOM(2), LZOOM(3), T, JERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1900) IERR, IPO
         CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('Hit button A, B, or C to mark vertex & go on to next',
     *   ' new segment')
 1031 FORMAT ('Hit button A, B, or C to mark vertex & go on to next',
     *   ' old vertex')
 1032 FORMAT ('Hit button D to exit')
 1040 FORMAT ('Hit button A or B to create new segment')
 1041 FORMAT ('Hit button C to reset old vertex')
 1100 FORMAT ('Move cursor to desired vertex and then hit button ',
     *   'A, B, or, C')
 1900 FORMAT ('GRLUTS: ERROR CODE',I7,' AT',I7)
      END
