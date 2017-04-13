      SUBROUTINE IENHNS (ICHAN, ICOLOR, ITYPE, RPOS, BUFFER, IERR)
C-----------------------------------------------------------------------
C! interactive linear enhancement of TV black & white LUTs
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2009, 2015
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
C   IENHNS performs an interactive linear enhancement of TV LUTs.
C   X cursor => intercept, Y cursor => slope, high button => quit
C   Inputs:
C      ICHAN   I          channel select bit mask
C      ICOLOR  I          color select bit mask
C   In/Out:
C      ITYPE   I          on in: 1 => do plot, A, B switch plot
C                                     C switch sign of slope
C                                2 => no plot, A, B return
C                                     C switch sign of slope
C                                3 => no plot, return any button
C                         on out - button value
C      RPOS    R(2)       Cursor position: initial -> final
C   Output:
C      BUFFER  I(>3072)   Scratch buffer
C      IERR    I          Error code of ZM70XF: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   ICHAN, ICOLOR, ITYPE, BUFFER(*), IERR
      REAL      RPOS(2)
C
      CHARACTER ON*4, OFF*4, GROP(2)*4, SUBR*6, OP*4
      INTEGER   IG, IG1, NPIXX, IX, IY, IX0, IY0, IOP, IBLK(9), JBLK(9),
     *   IBUT, IFLIP, QUAD, JERR, ITW(3), I, J, IPLSIZ, ISZX, ISZY,
     *   MAG, NPIXY
      LOGICAL   T, F, DOPLT, PLOTIT, DOIT, WASON
      REAL      PPOS(2), X, SLOPE, OFFSET, RAXINT, RVAL, XSCALE, YSCALE,
     *   TEMP, POW, F0, TMAG
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      EQUIVALENCE (ON, GROP(1)),   (OFF, GROP(2))
      DATA GROP /'ONNN','OFFF'/
      DATA T, F /.TRUE.,.FALSE./
      DATA POW /1.33/
C-----------------------------------------------------------------------
C                                        plot preparations
      PLOTIT = (ITYPE.EQ.1) .AND. (NGRAPH.GT.0)
      DOPLT = PLOTIT
      WASON = .TRUE.
      ISZX = WINDTV(3) - WINDTV(1) + 1
      ISZY = WINDTV(4) - WINDTV(2) + 1
      IF (PLOTIT) THEN
         DOPLT = F
         IG1 = MAX (2, NGRAPH) - 1
         IG = IG1 + NGRAY
         I = TVLIMG(1) / (2 ** (IG-1))
         WASON = MOD(I,2).EQ.1
         CALL YHOLD ('ONNN', IERR)
         SUBR = 'YSLECT'
         CALL YSLECT ('OFFF', IG, 0, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 900
C                                        set up plot area
         MAG = 1 + TVZOOM(1)
         IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
         IPLSIZ = (MAXINT + 1) / 2
         IF (IPLSIZ.GT.ISZX/4) IPLSIZ = ISZX / 4
         IF (IPLSIZ.GT.ISZY/4) IPLSIZ = ISZY / 4
         IF (IPLSIZ.LT.ISZX/16) IPLSIZ = ISZX / 16
         IF (IPLSIZ.LT.ISZY/16) IPLSIZ = ISZY / 16
         IPLSIZ = MIN (IPLSIZ, ISZX/MAG-3*CSIZTV(1))
         IPLSIZ = MIN (IPLSIZ, ISZY/MAG-3*CSIZTV(1))
         XSCALE = REAL (IPLSIZ) / (MAXINT+1.0)
         YSCALE = REAL (IPLSIZ) / (LUTOUT+1.0)
         NPIXY = IPLSIZ + 2.5*CSIZTV(2)
         IY0 = WINDTV(4) - MAG*NPIXY + 1 - (MAG-1)/2
         IF (MAG.GT.1) IY0 = IY0 + MAG
         IY0 = (IY0 - TVZOOM(3)) / MAG + TVZOOM(3) - TVSCGY
         NPIXX = IPLSIZ + 2.5*CSIZTV(1)
         TMAG = MAG
         IX0 = WINDTV(1) - (MAG-1)/2
         IX0 = (IX0 - TVZOOM(2)) / MAG + TVZOOM(2)
         IX0 = IX0 + ISZX / MAG - NPIXX - TVSCGX
         IX0 = MOD (IX0+4*MAXXTV(1)-1, MAXXTV(1)) + 1
         IY0 = MOD (IY0+4*MAXXTV(2)-1, MAXXTV(2)) + 1
         IF (IX0+NPIXX.GT.MAXXTV(1)) IX0 = MAXXTV(1) - NPIXX
         IF (IY0+NPIXY.GT.MAXXTV(2)) IY0 = MAXXTV(2) - NPIXY
         SUBR = 'YZERO'
         CALL YZERO (IG, IERR)
         IF (IERR.NE.0) GO TO 900
C                                        label, prepare init line
         IX = IX0 + IPLSIZ/2 + 0.5*CSIZTV(1) + 0.5
         IY = IY0 + IPLSIZ/2 + 2*CSIZTV(2)
         SUBR = 'IMCHAR'
         CALL IMCHAR (IG, IX, IY0, 0, 0, 'IN', BUFFER, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL IMCHAR (IG, IX0, IY, 3, 0, 'OUT', BUFFER, IERR)
         IF (IERR.NE.0) GO TO 900
         IX0 = IX0 + 1.5*CSIZTV(1) - 1
         IY0 = IY0 + 1.5*CSIZTV(2) - 1
         IX = 1 + IX0
         IY = IPLSIZ + IX0
         IBLK(1) = IX
         IBLK(2) = IY
         IBLK(3) = IY
         IBLK(4) = IX
         IBLK(5) = IX
         IBLK(6) = IX
         IBLK(7) = IX
         IBLK(8) = IY
         IBLK(9) = IY
         IX = 1 + IY0
         IY = IPLSIZ + IY0
         JBLK(1) = IX
         JBLK(2) = IX
         JBLK(3) = IY
         JBLK(4) = IY
         JBLK(5) = IX
         JBLK(6) = IX
         JBLK(7) = IX
         JBLK(8) = IY
         JBLK(9) = IY
         SUBR = 'IMVECT'
         CALL IMVECT (ON, IG, 9, IBLK, JBLK, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL YHOLD ('OFFF', IERR)
         END IF
C                                        init values, on cursor
      F0 = (WINDTV(4) - WINDTV(2)) / 3.
      IF ((RPOS(1).GE.WINDTV(1)) .AND. (RPOS(1).LE.WINDTV(3)) .AND.
     *   (RPOS(2).GE.WINDTV(2)) .AND. (RPOS(2).LE.WINDTV(4))) GO TO 37
         RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
         RPOS(2) = F0 + WINDTV(2)
 37   PPOS(1) = 0.0
      PPOS(2) = 0.0
      IFLIP = 1
      CALL ZTIME (ITW)
      QUAD = -1
      CALL YCURSE (ON, F, F, RPOS, QUAD, IBUT, IERR)
      SUBR = 'YCURSE'
      IF (IERR.NE.0) GO TO 900
C                                        read loop
 40   CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
      SUBR = 'YCURSE'
      IF ((IBUT.GT.7) .OR. (IERR.NE.0)) GO TO 900
      IF ((ITYPE.EQ.3) .AND. (IBUT.GT.0)) GO TO 900
      IF ((ITYPE.EQ.2) .AND. ((IBUT.EQ.1) .OR. (IBUT.EQ.2))) GO TO 900
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
         IF (.NOT.DOIT) GO TO 40
C                                       plot on/off button C
         IF ((IBUT.EQ.4) .OR. (.NOT.PLOTIT) .OR. (IBUT.EQ.0)) GO TO 50
            JERR = 0
            IBUT = IBUT - 4
            DOPLT = .NOT.DOPLT
            IOP = 2
            IF (DOPLT) IOP = 1
            SUBR = 'YSLECT'
            CALL YSLECT (GROP(IOP), IG, 0, BUFFER, IERR)
            IF ((IERR.NE.0) .OR. (JERR.NE.0)) GO TO 900
C                                        new LUT
 50      IF (IBUT.GT.0) IFLIP = -IFLIP
         J = MAXINT + 1
         RAXINT = LUTOUT
         X = ((RPOS(1)-WINDTV(1)+1.) / REAL (ISZX) - 0.5)
     *      * 2.0 * MAXINT
         SLOPE = ((RAXINT+1.)/(MAXINT+1.)) * IFLIP *
     *      (F0 / MAX (1., RPOS(2)-WINDTV(2))) ** POW
         OFFSET = ((1.0-X) * SLOPE + LUTOUT * (1-IFLIP) / 2) - 1
         DO 60 I = 1,J
            RVAL = (I-1)*SLOPE + OFFSET + 0.5
            BUFFER(I) = MAX (1.0, MIN (RAXINT, RVAL))
 60         CONTINUE
         BUFFER(1) = 0
         SUBR = 'YLUT'
         CALL YHOLD ('ONNN', IERR)
         CALL YLUT ('WRIT', ICHAN, ICOLOR, T, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 900
C                                        update line plot
         IF (DOPLT) THEN
            CALL IMVECT (OFF, IG, 9, IBLK, JBLK, BUFFER(MAXINT+2),
     *         IERR)
            SUBR = 'IMVECT'
            IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 900
            JBLK(6) = (2+BUFFER(2)) * YSCALE + IY0
            JBLK(9) = (2+BUFFER(MAXINT+1)) * YSCALE + IY0
            I = LUTOUT * (1-IFLIP) / 2
            TEMP = (I-OFFSET)/SLOPE + 1.0
            IF (TEMP.GT.MAXINT+1) TEMP = MAXINT + 1
            IX = TEMP + 0.01
            IX = MAX (2, IX)
            JBLK(7) = (2+BUFFER(IX)) * YSCALE + IY0
            IBLK(7) = (1+IX) * XSCALE + IX0
            I = LUTOUT * (1+IFLIP) / 2
            TEMP = (I-OFFSET)/SLOPE + 1.99
            IF (TEMP.GT.MAXINT+1) TEMP = MAXINT + 1
            IX = TEMP + 0.01
            IX = MAX (2, IX)
            JBLK(8) = (2+BUFFER(IX)) * YSCALE + IY0
            IBLK(8) = (1+IX) * XSCALE + IX0
            JBLK(6) = MIN (IPLSIZ+IY0, MAX (IY0 + 1, JBLK(6)))
            JBLK(7) = MIN (IPLSIZ+IY0, MAX (IY0 + 1, JBLK(7)))
            JBLK(8) = MIN (IPLSIZ+IY0, MAX (IY0 + 1, JBLK(8)))
            JBLK(9) = MIN (IPLSIZ+IY0, MAX (IY0 + 1, JBLK(9)))
            IBLK(7) = MIN (IPLSIZ+IX0, MAX (IX0 + 1, IBLK(7)))
            IBLK(8) = MIN (IPLSIZ+IX0, MAX (IX0 + 1, IBLK(8)))
            CALL IMVECT (ON, IG, 9, IBLK, JBLK, BUFFER, IERR)
            IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 900
            END IF
         CALL YHOLD ('OFFF', IERR)
         GO TO 40
C
C                                        off graphics & cursor
 900  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1900) IERR, SUBR
         CALL MSGWRT (7)
         END IF
      ITYPE = IBUT
      CALL YCURSE (OFF, F, F, RPOS, QUAD, IBUT, JERR)
      IF (PLOTIT) THEN
         OP = 'OFFF'
C         IF (WASON) OP = 'ONNN'
         CALL YSLECT (OP, IG, 0, BUFFER, JERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('ERROR',I5,' GENERATED BY ',A)
      END
