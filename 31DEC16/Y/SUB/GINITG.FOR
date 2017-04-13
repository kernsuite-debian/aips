      SUBROUTINE GINITG (IGLO, IGHI, RANGE, BUFF, IERR)
C-----------------------------------------------------------------------
C! writes an initialize-for-grey-scale record into a plot file or TV
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2002
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
C   GINITG will write an "init for gray-scale" command record to the
C   graphics file.  The output record has the form:
C            I     opcode (3)
C            I     lowest pixel value - scaled integer
C            I     highest pixel value - scaled integer
C            I     number of pixels on x axis
C            I     number of pixels on y axis
C            H*2   Function type
C            R(2)  Real image pixel range
C   Inputs:
C      IGLO  I         lowest allowed pixel value
C      IGHI  I         highest allowed pixel value
C   In/Out:
C      BUFF  I(256)    graphics buffer
C   Output:
C      IERR  I         error code: 0 => ok
C                                  1 => disk error
C                                  2 => input error
C-----------------------------------------------------------------------
      INTEGER    IGLO, IGHI, BUFF(256), IERR
      REAL       RANGE(2)
C
      INTEGER    NEED, OPCODE, IER, NPTSX, NPTSY, CMAXTV(2), NXA, NYA
      REAL       CATR(256)
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (GPHCAT, CATR)
      DATA NEED, OPCODE /249, 13/
C-----------------------------------------------------------------------
      IERR = 0
C                                        fill in common
      GPHTLO = IGLO
      GPHTHI = IGHI
      CALL RCOPY (2, RANGE, GPHRNG)
      IF (IGHI.LE.IGLO) THEN
         WRITE (MSGTXT,1000) IGLO, IGHI
         CALL MSGWRT (7)
         IERR = 2
         GO TO 999
         END IF
C                                       plot file
      IF (.NOT.GPHDOT) THEN
C                                        write buffer if needed
         IF (GPHPOS.GE.NEED) THEN
            CALL GPHWRT (BUFF, IER)
            IF (IER.NE.0) THEN
               IERR = 1
               GO TO 999
               END IF
            END IF
C                                        fill in values
         BUFF(GPHPOS+1) = OPCODE
         BUFF(GPHPOS+2) = IGLO
         BUFF(GPHPOS+3) = IGHI
         BUFF(GPHPOS+4) = ABS(GPHX2-GPHX1) + 1.5
         BUFF(GPHPOS+5) = ABS(GPHY2-GPHY1) + 1.5
         CALL CHR2H (2, GPHFUN, 1, BUFF(GPHPOS+6))
         CALL RCOPY (2, RANGE, BUFF(GPHPOS+7))
         GPHPOS = GPHPOS + 8
C                                       TV
      ELSE
         NPTSX = GPHIX2 - GPHIX1 + 1
         NPTSY = GPHIY2 - GPHIY1 + 1
         CMAXTV(1) = GPHTVW(3) - GPHTVW(1) + 1
         CMAXTV(2) = GPHTVW(4) - GPHTVW(2) + 1
         IF ((NPTSX.GT.MAXXTV(1)) .OR. (NPTSY.GT.MAXXTV(2))) THEN
            WRITE (MSGTXT,1110) NPTSX, NPTSY
            CALL MSGWRT (6)
            IERR = 2
            GO TO 999
            END IF
C                                       set origin
         GPHIX0 = GPHCRN(1)
         GPHIY0 = GPHCRN(2)
C                                       center
         IF ((GPHIX0.LE.0) .OR. (GPHIY0.LE.0)) THEN
            NXA = GPHIX2 - GPHIX1 + GPHCHL + GPHCHR + GPHRX2 - GPHRX1
            NYA = GPHIY2 - GPHIY1 + GPHCHB + GPHCHT + GPHRY2 - GPHRY1
            IF (NXA.GT.CMAXTV(1)) THEN
               GPHTVW(1) = 1
               GPHTVW(3) = MAXXTV(1)
               CMAXTV(1) = MAXXTV(1)
               END IF
            IF (NXA.GT.MAXXTV(1)) THEN
               GPHIX0 = (MAXXTV(1) - NPTSX - GPHRX2 + GPHRX1) / 2 + 1
            ELSE
               GPHIX0 = GPHCHL + (CMAXTV(1) - NXA) / 2 + GPHTVW(1)
               END IF
            IF (NYA.GT.CMAXTV(2)) THEN
               GPHTVW(2) = 1
               GPHTVW(4) = MAXXTV(2)
               CMAXTV(2) = MAXXTV(2)
               END IF
            IF (NYA.GT.MAXXTV(2)) THEN
               GPHIY0 = (MAXXTV(2) - NPTSY - GPHRY2 + GPHRY1) / 2 + 1
            ELSE
               GPHIY0 = GPHCHB + (CMAXTV(2) - NYA) / 2 + GPHTVW(2)
               END IF
            END IF
         GPHSCX = GPHIX2 - GPHIX1
         GPHSCY = GPHIY2 - GPHIY1
C                                        update catalog header for img
         GPHCAT(IIWIN  ) = GPHIX1
         GPHCAT(IIWIN+1) = GPHIY1
         GPHCAT(IIWIN+2) = GPHIX2
         GPHCAT(IIWIN+3) = GPHIY2
         GPHCAT(IICOR  ) = GPHIX0
         GPHCAT(IICOR+1) = GPHIY0
         GPHCAT(IICOR+2) = GPHIX0 + GPHSCX
         GPHCAT(IICOR+3) = GPHIY0 + GPHSCY
         CALL CHR2H (2, GPHFUN, 1, GPHCAT(IITRA))
         CATR(IRRAN) = RANGE(1)
         CATR(IRRAN+1) = RANGE(2)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GINITG: INVALID GRAY RANGE:',2I7)
 1110 FORMAT ('GINITG: NUMBER PIXELS ',2I5,' EXCEEDS TV SIZE')
      END
