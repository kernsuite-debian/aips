      SUBROUTINE OFMMOD (OPER, IG, N, OFM, IOFM, IERR)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002, 2015
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
C   Verbs to modify existing OFMs by "TWEAKing" or "ADJUSting"
C   Input:
C      OPER     C*(*)    'TWEAK', 'ADJUS'
C      IG       I        graphics plane to use (> NGRAY)
C      N        I        Dimension of OFM
C   In/out:
C      OFM      R(N,3)   In: current OFM, Out: new OFM
C   Output:
C      IOFM     R(N,3)   Initial OFM
C      IERR     I        POPS Errror code
C   Ideas from Mark Calabretta, Australia Telescope.
C-----------------------------------------------------------------------
      CHARACTER OPER*(*)
      INTEGER   IG, N, IERR
      REAL      OFM(N,3), IOFM(N,3)
C
      REAL      FK, RPOS(2), PPOS(2), MX, MY, LX, LY, DELTAI, SLOPE,
     *   OFFSET
      INTEGER   I, J, QUAD, IBUT, ITW(3), NLEVS, J0, POTERR, INX(2),
     *   INY(2), OUTX(2), OUTY(4), NHIST, MODE, LG, TX(2,4), TY(4),
     *   RNGE(2,4), IX, IXP(5), IYP(5), IY, IX0, IX1, IY0, IY1, MAG,
     *   KOLOR, IYC(5), IXC(2), TTY(2), INC, RHIST(8,200), I1, I2,
     *   PRNGE(2,4)
      LOGICAL   T, F, DOIT, DOADJU
      CHARACTER STRING*8, MSGBUF*72
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCNT.INC'
      INTEGER   BUFFER(MAXIMG)
      EQUIVALENCE (BUFFER, ILROW)
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      QUAD = -1
      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
      RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
      CALL ZTIME (ITW)
      NLEVS = OFMINP + 1
      CALL RCOPY (N, OFM(1,1), IOFM(1,1))
      CALL RCOPY (N, OFM(1,2), IOFM(1,2))
      CALL RCOPY (N, OFM(1,3), IOFM(1,3))
      POTERR = 49
      LG = IG + NGRAY
C                                        tweak the OFMs
      IF (OPER.EQ.'TWEAK') THEN
         MX = RPOS(1)
         MY = RPOS(2)
         LX = WINDTV(3) - WINDTV(1) + 1
         LY = WINDTV(4) - WINDTV(2) + 1
C                                        instructions
         MSGTXT = 'Use the cursor to modify the OFM LUTs'
         CALL MSGWRT (1)
         MSGTXT = 'Press button A to undo changes'
         CALL MSGWRT (1)
         MSGTXT = 'Press button D to exit'
         CALL MSGWRT (1)
 20      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       read loop
 30      CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
C                                       do something
            IF (DOIT) THEN
               IF (IBUT.GE.8) THEN
                  POTERR = 0
                  GO TO 980
C                                       Revert to the input OFM.
               ELSE IF (IBUT.EQ.1) THEN
                  CALL YWINDO ('READ', WINDTV, IERR)
                  RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
                  RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
                  MX = RPOS(1)
                  MY = RPOS(2)
                  LX = WINDTV(3) - WINDTV(1) + 1
                  LY = WINDTV(4) - WINDTV(2) + 1
                  END IF
C                                       Find new OFM
               J0 = (1.5 * NLEVS * (MX-RPOS(1))) / LX
               FK = 2.0 ** ((RPOS(2)-MY) / (LY/4.0))
               DO 40 I = 2,NLEVS
                  J = (I + J0) * FK + 0.5
                  J = MAX (1, MIN (NLEVS, J))
                  OFM(I,3) = IOFM(J,3)
                  OFM(I,2) = IOFM(J,2)
                  OFM(I,1) = IOFM(J,1)
 40               CONTINUE
C                                       Send to TV
               CALL OFMIO ('WRIT', N, NLEVS, F, OFM, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1040) IERR
                  CALL MSGWRT (6)
                  GO TO 980
                  END IF
               IF (IBUT.EQ.1) GO TO 20
               END IF
            GO TO 30
C                                       Adjust OFMs
      ELSE IF (OPER.EQ.'ADJUS') THEN
         TTY(1) = 5
         CALL LSERCH ('SRCH', TTY(1), TTY(2), F, IERR)
         CALL IMANOT ('INIT', IG, IX, TY(3), 0, 0, 'R', BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
C                                       get visible corners
         MAG = 1 + TVZOOM(1)
         IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
         IX0 = WINDTV(1) - (MAG-1)/2
         IX1 = WINDTV(3) - (MAG-1)/2
         IY0 = WINDTV(2) - (MAG-1)/2
         IY1 = WINDTV(4) - (MAG-1)/2
         IF (MAG.GT.1) IY0 = IY0 + MAG
         IF (MAG.GT.1) IY1 = IY1 + MAG
         IX0 = (IX0 - TVZOOM(2))/MAG + TVZOOM(2)
         IX1 = (IX1 - TVZOOM(2))/MAG + TVZOOM(2)
         IY0 = (IY0 - TVZOOM(3))/MAG + TVZOOM(3)
         IY1 = (IY1 - TVZOOM(3))/MAG + TVZOOM(3)
         IX0 = MAX (1, MIN (MAXXTV(1), IX0))
         IX1 = MAX (1, MIN (MAXXTV(1), IX1))
         IY0 = MAX (1, MIN (MAXXTV(1), IY0))
         IY1 = MAX (1, MIN (MAXXTV(1), IY1))
C                                       Plot locations: Input bar
         INY(2) = IY1 - 2 * CSIZTV(2) + 1
         INY(1) = INY(2) - 2 * CSIZTV(2) + 1
         TY(4) = INY(1) + (CSIZTV(2) + 1) / 2
         I = (IX1 - IX0 + 1 - CSIZTV(1)) / (OFMINP + 1)
         IF (I.LT.1) THEN
            INX(1) = IX0 + CSIZTV(1) / 2
            INX(2) = IX1 - CSIZTV(1) / 2
         ELSE
            INX(1) = (IX1 + IX0 - I * (OFMINP+1)) / 2
            INX(2) = INX(1) + I * (OFMINP + 1) - 1
            END IF
         TX(1,4) = INX(1) + (CSIZTV(1) + 1) / 2
         TX(2,4) = INX(2) - (CSIZTV(1) + 1) / 2 - 4 * CSIZTV(1)
C                                       Plot locations: Output bar
         OUTY(4) = INY(1) - 3 * CSIZTV(2)
         OUTY(3) = OUTY(4) - 2 * CSIZTV(2) + 1
         OUTY(2) = OUTY(3) - 2 * CSIZTV(2) + 1
         OUTY(1) = OUTY(2) - 2 * CSIZTV(2) + 1
         TY(1) = OUTY(1) + (CSIZTV(2) + 1) / 2
         TY(2) = OUTY(2) + (CSIZTV(2) + 1) / 2
         TY(3) = OUTY(3) + (CSIZTV(2) + 1) / 2
         I = (IX1 - IX0 - 3 * CSIZTV(1)) / (OFMOUT + 1)
         IF (I.LT.1) THEN
            OUTX(1) = IX0 + CSIZTV(1) * 2
            OUTX(2) = IX1 - CSIZTV(1) * 2
         ELSE
            OUTX(1) = (IX1 + IX0 - I * (OFMOUT+1)) / 2
            OUTX(2) = OUTX(1) + I * (OFMOUT + 1) - 1
            END IF
         TX(1,1) = OUTX(1) + (CSIZTV(1) + 1) / 2
         TX(2,1) = OUTX(2) - (CSIZTV(1) + 1) / 2 - 4 * CSIZTV(1)
         TX(1,2) = TX(1,1)
         TX(2,2) = TX(2,1)
         TX(1,3) = TX(1,1)
         TX(2,3) = TX(2,1)
C                                       Draw lines and labels: IN
         IXP(1) = INX(1)
         IXP(2) = INX(2)
         IXP(3) = INX(2)
         IXP(4) = INX(1)
         IXP(5) = INX(1)
         IYP(1) = INY(1)
         IYP(2) = INY(1)
         IYP(3) = INY(2)
         IYP(4) = INY(2)
         IYP(5) = INY(1)
         CALL IMVECT ('ONNN', LG, 5, IXP, IYP, BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IYP(1) = INY(1)
         IYP(2) = INY(2)
         IXP(1) = INX(1) + 0.25 * (INX(2) - INX(1)) + 0.5
         IXP(2) = IXP(1)
         CALL IMVECT ('ONNN', LG, 2, IXP, IYP, BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IXP(1) = INX(1) + 0.50 * (INX(2) - INX(1)) + 0.5
         IXP(2) = IXP(1)
         CALL IMVECT ('ONNN', LG, 2, IXP, IYP, BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IXP(1) = INX(1) + 0.75 * (INX(2) - INX(1)) + 0.5
         IXP(2) = IXP(1)
         CALL IMVECT ('ONNN', LG, 2, IXP, IYP, BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IX = INX(1) - CSIZTV(1) / 2
         IY = INY(2) + (CSIZTV(2) + 1) / 2
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, '0', BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IX = INX(2) - 3.5 * CSIZTV(1)
         WRITE (STRING,1100) OFMINP
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, STRING(:4), BUFFER,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IX = (INX(2) + INX(1) - CSIZTV(1)) / 2
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'IN', BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
C                                       Draw lines and labels: OUT
         IXP(1) = OUTX(1)
         IXP(2) = OUTX(2)
         IXP(3) = OUTX(2)
         IXP(4) = OUTX(1)
         IXP(5) = OUTX(1)
         IYP(1) = OUTY(1)
         IYP(2) = OUTY(1)
         IYP(3) = OUTY(4)
         IYP(4) = OUTY(4)
         IYP(5) = OUTY(1)
         CALL IMVECT ('ONNN', LG, 5, IXP, IYP, BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IYP(1) = OUTY(3)
         IYP(2) = IYP(1)
         CALL IMVECT ('ONNN', LG, 2, IXP, IYP, BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IYP(1) = OUTY(2)
         IYP(2) = IYP(1)
         CALL IMVECT ('ONNN', LG, 2, IXP, IYP, BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IYP(1) = OUTY(1)
         IYP(2) = OUTY(4)
         IXP(1) = OUTX(1) + 0.25 * (OUTX(2) - OUTX(1)) + 0.5
         IXP(2) = IXP(1)
         CALL IMVECT ('ONNN', LG, 2, IXP, IYP, BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IXP(1) = OUTX(1) + 0.50 * (OUTX(2) - OUTX(1)) + 0.5
         IXP(2) = IXP(1)
         CALL IMVECT ('ONNN', LG, 2, IXP, IYP, BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IXP(1) = OUTX(1) + 0.75 * (OUTX(2) - OUTX(1)) + 0.5
         IXP(2) = IXP(1)
         CALL IMVECT ('ONNN', LG, 2, IXP, IYP, BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IX = OUTX(1) - CSIZTV(1) / 2
         IY = OUTY(4) + (CSIZTV(2) + 1) / 2
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, '0', BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IX = OUTX(2) - 3.5 * CSIZTV(1)
         WRITE (STRING,1100) OFMINP
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, STRING(:4), BUFFER,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IX = (OUTX(1) + OUTX(2) - 3 * CSIZTV(1)) / 2
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'OUT', BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IX = OUTX(1) - (3 * CSIZTV(1) + 1) / 2
         CALL IMANOT ('WRIT', IG, IX, TY(1), 0, 0, 'B', BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         CALL IMANOT ('WRIT', IG, IX, TY(2), 0, 0, 'G', BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         CALL IMANOT ('WRIT', IG, IX, TY(3), 0, 0, 'R', BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IX = OUTX(2) + (CSIZTV(1) + 1) / 2
         CALL IMANOT ('WRIT', IG, IX, TY(1), 0, 0, 'B', BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         CALL IMANOT ('WRIT', IG, IX, TY(2), 0, 0, 'G', BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         CALL IMANOT ('WRIT', IG, IX, TY(3), 0, 0, 'R', BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
C                                       Options menu
         IXC(1) = IX0 + (IX1 - IX0) / 3
         IXC(2) = IXC(1) + (IX1 - IX0) / 3
         IYC(1) = IY0 + CSIZTV(2) / 2
         IYC(2) = IYC(1) + 3 * CSIZTV(2) - 1
         IYC(3) = IYC(2) + 3 * CSIZTV(2) - 1
         IYC(4) = IYC(3) + 3 * CSIZTV(2) - 1
         IYC(5) = IYC(4) + 3 * CSIZTV(2) - 1
         IX = IX0 + CSIZTV(1) / 2
         IY = IYC(1) + CSIZTV(2)
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'SET B OUT', BUFFER,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IY = IYC(2) + CSIZTV(2)
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'SET G OUT', BUFFER,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IY = IYC(3) + CSIZTV(2)
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'SET R OUT', BUFFER,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IY = IYC(4) + CSIZTV(2)
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'SET IN RANGE', BUFFER,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IX = (IX0 + IX1) / 2  - 8 * CSIZTV(1)
         IY = IYC(1) + CSIZTV(2)
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'EXIT', BUFFER,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IY = IYC(2) + CSIZTV(2)
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'UNDO ALL CHANGES',
     *      BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IY = IYC(3) + CSIZTV(2)
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'SHOW HISTORY', BUFFER,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IY = IYC(4) + CSIZTV(2)
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'UNDO ONE CHANGE',
     *      BUFFER, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IX = IX1 - 13.5 * CSIZTV(2)
         IY = IYC(1) + CSIZTV(2)
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'TYPE IN B OUT', BUFFER,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IY = IYC(2) + CSIZTV(2)
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'TYPE IN G OUT', BUFFER,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IY = IYC(3) + CSIZTV(2)
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'TYPE IN R OUT', BUFFER,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         IY = IYC(4) + CSIZTV(2)
         CALL IMANOT ('WRIT', IG, IX, IY, 0, 0, 'TYPE IN RANGE', BUFFER,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
C                                       Initial values
         CALL FILL (8, -999, RNGE)
         CALL FILL (8, -999, PRNGE)
         MODE = 0
         KOLOR = 0
         NHIST = 1
         CALL FILL (8, -999, RHIST(1,NHIST))
C                                       cursor on
         RPOS(1) = (IX0 + IXC(1)) / 2.0
         RPOS(2) = (IYC(4) + IYC(5)) / 2.0
         MSGTXT = 'Hit button D to exit at any time'
         CALL MSGWRT (1)
         MSGTXT = 'Select an option with cursor and button A, B, or C'
         CALL MSGWRT (1)
         CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       read loop
 120     CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IERR)
            IF (IERR.NE.0) GO TO 980
            DOADJU = F
C                                       Defeat screen wrap algo.
            IF ((RPOS(1).NE.PPOS(1)) .OR. (RPOS(2).NE.PPOS(2))) THEN
               PPOS(1) = 0.0
               PPOS(2) = 0.0
               END IF
            CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
            IF (DOIT) THEN
               IF (IBUT.GE.8) THEN
                  POTERR = 0
                  GO TO 980
                  END IF
C                                       find operation
               IF (MODE.EQ.0) THEN
                  IF (IBUT.LE.0) GO TO 120
                  IX = 2
                  IF (RPOS(1).LE.IXC(1)) IX = 1
                  IF (RPOS(1).GE.IXC(2)) IX = 3
                  IY = 1
                  DO 125 I = 2,5
                     IF (RPOS(2).GT.IYC(I)) IY = I
 125                 CONTINUE
                  IF (IY.EQ.5) GO TO 190
                  KOLOR = 0
                  IF (IX.NE.2) THEN
                     IF ((IY.LT.4) .AND. (RNGE(1,4).EQ.-999)) THEN
                        MSGTXT = 'AN INPUT RANGE MUST BE SPECIFIED'
     *                     // ' FIRST'
                        CALL MSGWRT (1)
                        GO TO 190
                        END IF
C                                       save this range
                     IF ((IY.EQ.4) .AND. (RNGE(1,4).NE.-999)) THEN
                        DO 126 KOLOR = 1,3
                           IF (RNGE(1,KOLOR).NE.-999) GO TO 127
                           IF (RNGE(1,KOLOR).NE.-999) GO TO 127
 126                       CONTINUE
                        GO TO 128
127                    IF (NHIST.EQ.200) THEN
                           MSGTXT = 'History stack is filled'
                           CALL MSGWRT (1)
                        ELSE
                           NHIST = NHIST + 1
                           END IF
 128                    CALL FILL (8, -999, RNGE)
                        CALL FILL (8, -999, PRNGE)
                        CALL FILL (8, -999, RHIST(1,NHIST))
                        STRING = ' '
                        DO 130 KOLOR = 1,4
                           DO 130 J = 1,2
                              CALL IMANOT ('WRIT', IG, TX(J,KOLOR),
     *                           TY(KOLOR), 0, 0, STRING(:4), BUFFER,
     *                           IERR)
                              IF ((IERR.NE.0) .AND. (IERR.NE.2))
     *                           GO TO 980
 130                       CONTINUE
                        END IF
                     KOLOR = IY
                     END IF
C                                       Init interactive numbers
                  IF (IX.EQ.1) THEN
                     PRNGE(1,KOLOR) = RNGE(1,KOLOR)
                     PRNGE(2,KOLOR) = RNGE(2,KOLOR)
                     MODE = 1
                     IF (RNGE(1,KOLOR).EQ.-999) THEN
                        RNGE(1,KOLOR) = 0
                        IF (KOLOR.LT.4) THEN
                           I = MIN (RNGE(1,4), OFMINP)
                           IF (I.GT.0) RNGE(1,KOLOR) = OFM(I,KOLOR) *
     *                        OFMOUT
                           END IF
                        END IF
                     IF (RNGE(2,KOLOR).EQ.-999) THEN
                        RNGE(2,KOLOR) = OFMINP
                        IF (KOLOR.LT.4) THEN
                           RNGE(2,KOLOR) = OFM(OFMINP+1,KOLOR) * OFMOUT
     *                        - 0.001
                           I = MAX (RNGE(2,4), 0)
                           IF (I.LT.OFMINP) RNGE(2,KOLOR) =
     *                        OFM(I+2,KOLOR) * OFMOUT - 0.001
                           END IF
                        END IF
                     WRITE (STRING,1100) RNGE(1,KOLOR)
                     CALL IMANOT ('WRIT', IG, TX(1,KOLOR), TY(KOLOR), 0,
     *                  0, STRING(:4), BUFFER, IERR)
                     IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
                     WRITE (STRING,1100) RNGE(2,KOLOR)
                     CALL IMANOT ('WRIT', IG, TX(2,KOLOR), TY(KOLOR), 0,
     *                  0, STRING(:4), BUFFER, IERR)
                     IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
                     MSGTXT = 'Cursor X position controls starting '
     *                  // 'value'
                     CALL MSGWRT (1)
                     MSGTXT = 'Hit button A to do ending value'
                     CALL MSGWRT (1)
                     MSGTXT = 'Hit button B to revert to initial '
     *                  // 'values'
                     CALL MSGWRT (1)
                     MSGTXT = 'Hit button C to go to new color or '
     *                  // 'operation'
                     CALL MSGWRT (1)
                     IF (KOLOR.EQ.4) THEN
                        RPOS(1) = (RNGE(1,KOLOR) * (INX(2) - INX(1))) /
     *                     OFMINP + INX(1)
                        RPOS(2) = (INY(1) + INY(2)) / 2
                     ELSE
                        RPOS(1) = (RNGE(1,KOLOR) * (OUTX(2) - OUTX(1)))
     *                     / OFMOUT + OUTX(1)
                        RPOS(2) = (OUTY(KOLOR) + OUTY(KOLOR+1)) / 2
                        END IF
                     CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
                     IF (IERR.NE.0) GO TO 980
C                                       Request typed numbers
                  ELSE IF (IX.EQ.3) THEN
                     IF (KOLOR.EQ.4) WRITE (MSGBUF,1120) OFMINP
                     IF (KOLOR.EQ.3) WRITE (MSGBUF,1121) 'Red', OFMOUT
                     IF (KOLOR.EQ.2) WRITE (MSGBUF,1121) 'Green', OFMOUT
                     IF (KOLOR.EQ.1) WRITE (MSGBUF,1121) 'Blue', OFMOUT
                     CALL INQINT (TTY, MSGBUF, 2, RNGE(1,KOLOR), IERR)
                     IF (IERR.NE.0) THEN
                        WRITE (MSGTXT,1122) IERR
                        CALL MSGWRT (6)
                        POTERR = 101
                        GO TO 990
                        END IF
                     DOADJU = KOLOR.LE.3
                     DO 135 J = 1,2
                        WRITE (STRING,1100) RNGE(J,KOLOR)
                        CALL IMANOT ('WRIT', IG, TX(J,KOLOR), TY(KOLOR),
     *                     0, 0, STRING(:4), BUFFER, IERR)
                        IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
 135                    CONTINUE
                     CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
                     IF (IERR.NE.0) GO TO 980
                     CALL COPY (8, RNGE, RHIST(1,NHIST))
C                                       Misc. operations
                  ELSE
C                                       Quit
                     IF (IY.EQ.1) THEN
                        POTERR = 0
                        GO TO 980
C                                       Undo ALL changes
                     ELSE IF (IY.EQ.2) THEN
                        NHIST = 1
                        CALL RCOPY (N, IOFM(1,1), OFM(1,1))
                        CALL RCOPY (N, IOFM(1,2), OFM(1,2))
                        CALL RCOPY (N, IOFM(1,3), OFM(1,3))
                        CALL OFMIO ('WRIT', N, NLEVS, F, OFM, IERR)
                        IF (IERR.NE.0) GO TO 980
                        CALL FILL (8, -999, RNGE)
                        CALL FILL (8, -999, PRNGE)
                        CALL FILL (8, -999, RHIST(1,NHIST))
                        STRING = ' '
                        DO 140 KOLOR = 1,4
                           DO 140 J = 1,2
                              CALL IMANOT ('WRIT', IG, TX(J,KOLOR),
     *                           TY(KOLOR), 0, 0, STRING(:4), BUFFER,
     *                           IERR)
                              IF ((IERR.NE.0) .AND. (IERR.NE.2))
     *                           GO TO 980
 140                       CONTINUE
C                                       Display history
                     ELSE IF (IY.EQ.3) THEN
                        WRITE (MSGTXT,1130)
                        CALL MSGWRT (2)
                        DO 145 I = 1,NHIST
                           IF (I.EQ.NHIST) THEN
                              MSGTXT = 'Current segment:'
                              CALL MSGWRT (2)
                              END IF
                           WRITE (MSGTXT,1140) I, RHIST(7,I),
     *                        RHIST(8,I), (RHIST(J,I), J = 1,6)
                           CALL MSGWRT (2)
 145                       CONTINUE
C                                       Undo a single change
                     ELSE IF (IY.EQ.4) THEN
                        WRITE (MSGBUF,1145) NHIST
                        CALL INQINT (TTY, MSGBUF, 1, J0, IERR)
                        IF (IERR.NE.0) THEN
                           WRITE (MSGTXT,1122) IERR
                           CALL MSGWRT (6)
                           POTERR = 101
                           GO TO 990
                           END IF
                        IF ((J0.LT.1) .OR. (J0.GT.NHIST)) THEN
                           WRITE (MSGTXT,1146) J0, NHIST
                           CALL MSGWRT (1)
                           GO TO 190
                           END IF
                        CALL RCOPY (N, IOFM(1,1), OFM(1,1))
                        CALL RCOPY (N, IOFM(1,2), OFM(1,2))
                        CALL RCOPY (N, IOFM(1,3), OFM(1,3))
                        CALL FILL (8, -999, RHIST(1,J0))
C                                       Undo current one
                        IF (J0.EQ.NHIST) THEN
                           STRING = ' '
                           DO 150 KOLOR = 1,4
                              DO 150 J = 1,2
                                 CALL IMANOT ('WRIT', IG, TX(J,KOLOR),
     *                              TY(KOLOR), 0, 0, STRING(:4), BUFFER,
     *                              IERR)
                                 IF ((IERR.NE.0) .AND. (IERR.NE.2))
     *                              GO TO 980
 150                          CONTINUE
                           CALL FILL (8, -999, RNGE)
                           END IF
C                                       rebuild OFM
                        DO 165 J = 1,NHIST
                           I1 = RHIST(7,J)
                           I2 = RHIST(8,J)
                           IF ((I1.EQ.-999) .OR. (I2.EQ.-999))
     *                        GO TO 165
                           INC = 1
                           IF (I1.GT.I2) INC = -1
                           DELTAI = I2 - I1
                           IF (I2.EQ.I1) DELTAI = 1.0
                           J0 = I1
                           I1 = MAX (0, MIN (OFMINP, I1)) + 1
                           I2 = MAX (0, MIN (OFMINP, I2)) + 1
                           DO 160 KOLOR = 1,3
                              IX = 2 * KOLOR - 1
                              IF ((RHIST(IX,J).EQ.-999) .OR.
     *                           (RHIST(IX+1,J).EQ.-999)) GO TO 160
                              SLOPE  = (RHIST(IX+1,J)-RHIST(IX,J))
     *                           / (DELTAI * OFMOUT)
                              OFFSET = RHIST(IX,J) / REAL(OFMOUT)
     *                           - J0 * SLOPE
                              DO 155 I = I1,I2,INC
                                 OFM(I,KOLOR) = (I-1)*SLOPE + OFFSET
 155                             CONTINUE
 160                          CONTINUE
 165                       CONTINUE
                        KOLOR = 0
                        CALL OFMIO ('WRIT', N, NLEVS, F, OFM, IERR)
                        IF (IERR.NE.0) GO TO 980
                        END IF
                     END IF
C                                       interacting TV color/level
               ELSE
                  IF (IBUT.LE.0) THEN
                     J = MODE
                     IF (J.EQ.3) J = 1
                     IF (KOLOR.EQ.4) THEN
                        RNGE(J,KOLOR) = REAL ((RPOS(1) - INX(1)) *
     *                      OFMINP) / REAL (INX(2) - INX(1)) + 0.5
                     ELSE
                        RNGE(J,KOLOR) = REAL ((RPOS(1) - OUTX(1)) *
     *                     OFMOUT) / REAL (OUTX(2) - OUTX(1)) + 0.5
                        END IF
                     WRITE (STRING,1100) RNGE(J,KOLOR)
                     CALL IMANOT ('WRIT', IG, TX(J,KOLOR), TY(KOLOR), 0,
     *                  0, STRING(:4), BUFFER, IERR)
                     IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
                     DOADJU = (KOLOR.LE.3) .AND. (MODE.NE.3)
                  ELSE IF (IBUT.EQ.1) THEN
                     MODE = 3 - MODE
                     IF (MODE.EQ.0) MODE = 2
                     MSGTXT = 'Cursor X position controls starting '
     *                  //  'value'
                     CALL MSGWRT (1)
                     IF (MODE.EQ.1) THEN
                        MSGTXT = 'Hit button A to do ending value'
                        CALL MSGWRT (1)
                     ELSE IF (MODE.EQ.2) THEN
                        MSGTXT = 'Hit button A to do starting value'
                        CALL MSGWRT (1)
                        END IF
                     MSGTXT = 'Hit button C to freeze the '
     *                  // 'present values'
                     CALL MSGWRT (1)
                     IF (KOLOR.EQ.4) THEN
                        RPOS(1) = 0.5 + REAL (RNGE(MODE,KOLOR) *
     *                     (INX(2) - INX(1))) / OFMINP + INX(1)
                        IF (RNGE(MODE,KOLOR).EQ.-999) RPOS(1) =
     *                     INX(MODE)
                        RPOS(2) = (INY(1) + INY(2)) / 2
                     ELSE
                        RPOS(1) = 0.5 + REAL (RNGE(MODE,KOLOR) *
     *                     (OUTX(2) - OUTX(1))) / OFMOUT + OUTX(1)
                        IF (RNGE(MODE,KOLOR).EQ.-999) RPOS(1) =
     *                     OUTX(MODE)
                        RPOS(2) = (OUTY(KOLOR) + OUTY(KOLOR+1)) / 2
                        END IF
                     IF (RPOS(1).LT.WINDTV(1)) RPOS(1) = WINDTV(1)
                     IF (RPOS(1).GT.WINDTV(3)) RPOS(1) = WINDTV(3)
                     CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
                     IF (IERR.NE.0) GO TO 980
C                                       revert
                  ELSE IF (IBUT.LE.3) THEN
                     RNGE(1,KOLOR) = PRNGE(1,KOLOR)
                     RNGE(2,KOLOR) = PRNGE(2,KOLOR)
                     DO 180 J = 1,2
                        WRITE (STRING,1100) RNGE(J,KOLOR)
                        IF (RNGE(J,KOLOR).EQ.-999) STRING = ' '
                        CALL IMANOT ('WRIT', IG, TX(J,KOLOR), TY(KOLOR),
     *                     0, 0, STRING(:4), BUFFER, IERR)
                        IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
 180                    CONTINUE
                     DOADJU = KOLOR.LE.3
                     IF (KOLOR.EQ.4) THEN
                        RPOS(1) = 0.5 + REAL (RNGE(MODE,KOLOR) *
     *                     (INX(2) - INX(1))) / OFMINP + INX(1)
                        IF (RNGE(MODE,KOLOR).EQ.-999) RPOS(1) =
     *                     INX(MODE)
                        RPOS(2) = (INY(1) + INY(2)) / 2
                     ELSE
                        RPOS(1) = 0.5 + REAL (RNGE(MODE,KOLOR) *
     *                      (OUTX(2) - OUTX(1))) / OFMOUT + OUTX(1)
                        IF (RNGE(MODE,KOLOR).EQ.-999) RPOS(1) =
     *                     OUTX(MODE)
                        RPOS(2) = (OUTY(KOLOR) + OUTY(KOLOR+1)) / 2
                        END IF
                     IF (RPOS(1).LT.WINDTV(1)) RPOS(1) = WINDTV(1)
                     IF (RPOS(1).GT.WINDTV(3)) RPOS(1) = WINDTV(3)
                     CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
                     IF (IERR.NE.0) GO TO 980
                  ELSE
                     CALL COPY (8, RNGE, RHIST(1,NHIST))
                     MODE = 0
                     RPOS(1) = (IX0 + IXC(1)) / 2.0
                     RPOS(2) = (IYC(KOLOR) + IYC(KOLOR+1)) / 2.0
                     CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
                     IF (IERR.NE.0) GO TO 980
                     END IF
                  END IF
C                                       change the OFM in fact
               IF (DOADJU) THEN
                  I1 = RNGE(1,4)
                  I2 = RNGE(2,4)
                  IF ((I1.EQ.-999) .OR. (I2.EQ.-999)) GO TO 190
                  IF ((RNGE(1,KOLOR).EQ.-999) .OR.
     *               (RNGE(2,KOLOR).EQ.-999)) GO TO 190
                  INC = 1
                  IF (I1.GT.I2) INC = -1
                  DELTAI = I2 - I1
                  IF (I2.EQ.I1) DELTAI = 1.0
                  SLOPE  = (RNGE(2,KOLOR) - RNGE(1,KOLOR)) /
     *               (DELTAI * OFMOUT)
                  OFFSET = RNGE(1,KOLOR) / REAL(OFMOUT) -
     *               I1 * SLOPE
                  I1 = MAX (0, MIN (OFMINP, I1)) + 1
                  I2 = MAX (0, MIN (OFMINP, I2)) + 1
                  DO 185 I = I1,I2,INC
                     OFM(I,KOLOR) = (I - 1) * SLOPE + OFFSET
 185                 CONTINUE
                  CALL OFMIO ('WRIT', N, NLEVS, F, OFM, IERR)
                  IF (IERR.NE.0) GO TO 980
                  END IF
 190           IF (MODE.EQ.0) THEN
                  MSGTXT = 'Select an option with cursor and button '
     *               // 'A, B, or C'
                  CALL MSGWRT (1)
                  END IF
               END IF
            GO TO 120
      ELSE
         IERR = 0
         POTERR = 19
         END IF
C                                       close down
C                                       messages on TV error
 980  IF (IERR.EQ.0) GO TO 990
         WRITE (MSGTXT,1980) IERR
         CALL MSGWRT (7)
C                                       error management
 990  IERR = POTERR
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('OFMMOD:: ERROR',I3,' UPDATING THE OFM LUTS')
 1100 FORMAT (I4.3)
 1120 FORMAT ('Enter lower and upper input intensity values, 0-',I4,
     *   ' (2 I)')
 1121 FORMAT ('Enter ',A,' output values at lower and upper input ',
     *   'levels, 0-',I4,' (2 I)')
 1122 FORMAT ('TERMINAL ERROR',I6)
 1130 FORMAT (8X,'Input range',4X,'Output blue',3X,'Output green',5X,
     *   'Output red')
 1140 FORMAT (I4,4(3X,I5,' :',I5))
 1145 FORMAT ('Enter segment number to be undone: 1 thru',I4)
 1146 FORMAT ('SEGMENT',I5,' OUTSIDE RANGE 1 -',I4,' NOTHING UNDONE')
 1980 FORMAT ('TV ACTION ERROR CODE',I5,' RETURNED')
      END
