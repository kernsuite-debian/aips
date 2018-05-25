      SUBROUTINE OFMGAM (IG, N, OFM, RPOS, GAMMA, BUFFER, IERR)
C-----------------------------------------------------------------------
C! verb to try values of TV gamma correction and store for AIPS Manager
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2009, 2015
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
C   interactive display of various gamma values, allowing saving of a
C   new TV Gamma correction exponent for AIPS Managers
C   Inputs:
C      IG       I        Graphics channel to use
C      N        I        First dimension of oFM
C      OFM      R(N,3)   Initial OFM - does not change here.
C   Output:
C      RPOS     R(2)     Final cursor position
C      GAMMA    R        Final gamma
C      BUFFER   R(*)     Scratch buffer
C      IERR     I        Error code suitable for pOPS
C-----------------------------------------------------------------------
      INTEGER   IG, N, IERR
      REAL      OFM(N,3), RPOS(2), GAMMA, BUFFER(*)
C
      INTEGER   QUAD, ITW(3), IBUT, NEDGE, NPIX, NROW, MAG, IX0, IY0,
     *   POTERR, NLEVS
      REAL      OGAMMA, PPOS(2)
      LOGICAL   F, DOIT
      CHARACTER STRING*20
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                        Common TV parms
      QUAD = -1
      GAMMA = 0.0
      POTERR = 49
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL ZTIME (ITW)
      NLEVS = OFMINP + 1
      CALL IMANOT ('INIT', IG, IX0, IY0, 0, 0, STRING(1:6), BUFFER,
     *   IERR)
      IF (IERR.NE.0) GO TO 980
C                                        Instructions
      MSGTXT = 'Cursor Y sets Gamma correction'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button A or B to reset original Gamma and continue'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button C to save Gamma and exit'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button D to just exit'
      CALL MSGWRT (1)
C                                       get old values
      OGAMMA = TVGAMA
C                                       set up graphics display
      NEDGE = (2 * MAXXTV(1)) / 512
      IF (NEDGE.LT.2) NEDGE = 2
      NPIX = 6 * CSIZTV(1) + 2 * NEDGE
      NROW = CSIZTV(2) + 2 * NEDGE
      MAG = 1 + TVZOOM(1)
      IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
      IX0 = WINDTV(1) - (MAG-1)/2
      IY0 = WINDTV(4) - MAG*NROW + 1 - (MAG-1)/2
      IF (MAG.GT.1) IY0 = IY0 + MAG
      IX0 = (IX0 - TVZOOM(2))/MAG + TVZOOM(2)
      IY0 = (IY0 - TVZOOM(3))/MAG + TVZOOM(3)
      IX0 = MAX (1, IX0)
      IY0 = MAX (1, IY0)
      IF (IX0+NPIX-1.GT.MAXXTV(1)) IX0 = MAXXTV(1) - NPIX + 1
      IF (IY0+NROW-1.GT.MAXXTV(2)) IY0 = MAXXTV(2) - NROW + 1
C                                       set to top row of text
C                                       with scroll correction
      IX0 = IX0 + NEDGE - TVSCGX
      IY0 = IY0 + NEDGE - TVSCGY
      IX0 = MAX (NEDGE, IX0)
      IY0 = MAX (NEDGE, IY0)
      IF (IX0+NPIX-1.GT.MAXXTV(1)) IX0 = 1
      IF (IY0+NROW-1.GT.MAXXTV(2)) IY0 = MAXXTV(2) - NROW + 1
C                                       turn on cursor
 20   RPOS(1) = (WINDTV(3) + WINDTV(1)) / 2.0
         RPOS(2) = (1./4. - 0.01) * (WINDTV(4) - WINDTV(2) + 1.0)
     *      + WINDTV(2) - 1.0
         CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       read loop
 30      CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
C                                       set a gamma
            IF (DOIT) THEN
               GAMMA =  OGAMMA * 4.0 * ((RPOS(2) - WINDTV(2) + 1.) /
     *            (WINDTV(4) - WINDTV(2) + 1.0) + 0.01)
               IF (IBUT.GE.8) THEN
                  TVGAMA = OGAMMA
               ELSE
                  TVGAMA = GAMMA
                  END IF
               CALL OFMIO ('WRIT', N, NLEVS, F, OFM, IERR)
               IF (IERR.NE.0) GO TO 980
               WRITE (STRING,1030) TVGAMA
               CALL IMANOT ('WRIT', IG, IX0, IY0, 0, 0, STRING(1:6),
     *            BUFFER, IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
C                                       loop or quit
            IF (IBUT.EQ.0) GO TO 30
         IF (IBUT.LE.3) GO TO 20
      POTERR = 0
      IF (IBUT.LT.8) THEN
         WRITE (MSGTXT,1040) TVGAMA
         CALL MSGWRT (4)
         END IF
C                                        close down
C                                        messages on TV error
 980  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1980) IERR
         CALL MSGWRT (7)
         END IF
C                                       POPS error management
      IERR = POTERR
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (F6.3)
 1040 FORMAT ('TV gamma correction exponent set to',F7.3)
 1980 FORMAT ('TV ACTION ERROR CODE',I5,' RETURNED')
      END
