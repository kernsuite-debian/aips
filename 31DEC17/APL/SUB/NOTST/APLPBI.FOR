      SUBROUTINE APLPBI (IFIELD, IN, VAL, OUT1, OUT2, IERR)
C-----------------------------------------------------------------------
C! *TESS routine to apply a taper to an image. VLA only!
C# Map-util VLA
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000, 2003, 2006
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
C   APLPBI multiplies IN by VAL into OUT1, applying a PB taper,
C   while applying a PB taper^2 to VAL and putting in OUT2
C   Programmer =  R. Braun                               November 1987
C   BEAMTP=2 uses PBCALC with LAMBDA in BMSIZE,
C   BEAMTP=1 uses Gaussian with FWHM in BMSIZE
C-----------------------------------------------------------------------
      INTEGER   IN, OUT1, OUT2, IFIELD
      REAL      VAL, XCEN, YCEN
      REAL      RCONSX, RCONSY, YSQ, RADSQ, TAPER
      INTEGER   AKOPEN, AKCESS, AKCLOS
      DOUBLE PRECISION ANGLE, LAMBDA
      LOGICAL   OUTSID
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C
      IF ((BMSIZE(IFIELD).GT.0.0) .AND. (BEAMTP(IFIELD).GT.0)) THEN
         IF (BEAMTP(IFIELD).EQ.1) THEN
            RCONSX = 2.772588722 * (CELLX/BMSIZE(IFIELD))**2
            RCONSY = 2.772588722 * (CELLY/BMSIZE(IFIELD))**2
         ELSE
            LAMBDA = BMSIZE(IFIELD)
            RCONSX = (CELLX/3600.)**2
            RCONSY = (CELLY/3600.)**2
            END IF
      ELSE
         RCONSX = 0.0
         RCONSY = 0.0
         END IF
C
      XCEN = RCENX(IFIELD) - BLC(1) + XBEG
      YCEN = RCENY(IFIELD) - BLC(2) + YBEG
C
      IF (AKOPEN (IN,1,'READ',BUFFR1).NE.0) GO TO 990
      IF (AKOPEN (OUT1,2,'WRIT',BUFFR2).NE.0) GO TO 990
      IF (AKOPEN (OUT2,3,'WRIT',BUFFR3).NE.0) GO TO 990
C                                        Loop thru map
      DO 30 IY = 1,HNY
C
         IF (AKCESS(OUT1,BUFFR2).NE.0) GO TO 990
         IF (AKCESS(OUT2,BUFFR3).NE.0) GO TO 990
C                                        Find new map estimate
         CALL RFILL (HNX, FBLANK, BUFFR2(BIND(2)))
         CALL RFILL (HNX, FBLANK, BUFFR3(BIND(3)))
         IF ((IY.GE.YBEG) .AND. (IY.LE.YEND)) THEN
            IF (AKCESS(IN,BUFFR1).NE.0)  GO TO 990
            I1 = BIND(1)
            I2 = BIND(2) + XBEG - 1
            I3 = BIND(3) + XBEG - 1
            YSQ = RCONSY*(REAL(IY)-YCEN)**2
            IF (RCONSY.EQ.0.) THEN
               DO 20 IX = XBEG,XEND
                  IF (BUFFR1(I1).NE.FBLANK) THEN
                     BUFFR2(I2) = BUFFR1(I1) * VAL
                     BUFFR3(I3) = VAL
                     END IF
                  I1 = I1 + 1
                  I2 = I2 + 1
                  I3 = I3 + 1
 20               CONTINUE
            ELSE IF (BEAMTP(IFIELD).EQ.1) THEN
               DO 21 IX = XBEG,XEND
                  RADSQ = YSQ + RCONSX*(REAL(IX)-XCEN)**2
                  TAPER = EXP(-RADSQ)
                  IF ((TAPER.GT.PBPARM(1)) .AND.
     *               (BUFFR1(I1).NE.FBLANK)) THEN
                     BUFFR2(I2) = TAPER * BUFFR1(I1) * VAL
                     BUFFR3(I3) = TAPER * TAPER * VAL
                     END IF
                  I1 = I1 + 1
                  I2 = I2 + 1
                  I3 = I3 + 1
 21               CONTINUE
            ELSE
               DO 25 IX = XBEG,XEND
                  IF (BUFFR1(I1).NE.FBLANK) THEN
                     RADSQ = YSQ + RCONSX*(REAL(IX)-XCEN)**2
                     ANGLE = SQRT (RADSQ)
                     CALL PBCALC (ANGLE, LAMBDA, ANAME, PBPARM(2),
     *                  TAPER, OUTSID)
                     OUTSID = OUTSID .OR. (TAPER.LE.PBPARM(1))
                     IF (.NOT.OUTSID) THEN
                        BUFFR2(I2) = TAPER * BUFFR1(I1) * VAL
                        BUFFR3(I3) = TAPER * TAPER * VAL
                        END IF
                     END IF
                  I1 = I1 + 1
                  I2 = I2 + 1
                  I3 = I3 + 1
 25               CONTINUE
               END IF
            END IF
 30      CONTINUE
C
      IF (AKCLOS (IN,BUFFR1).NE.0) GO TO 990
      IF (AKCLOS (OUT1,BUFFR2).NE.0) GO TO 990
      IF (AKCLOS (OUT2,BUFFR3).NE.0) GO TO 990
      GO TO 999
C
 990  WRITE (MSGTXT,1990)
      CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('ERROR FROM APLPBI')
      END
