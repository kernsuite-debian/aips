      SUBROUTINE GINITL (BLC, TRC, XYRATO, CHOUT, DEPTH, BUFF, IERR)
C-----------------------------------------------------------------------
C! writes an initialize-for-line-drawing command into a plot file or TV
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002, 2011
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
C   GINITL will write an 'init for line drawing' command record to a
C   graph file. Common variables will also be initialized.
C   The output record description is:
C            I       opcode, 2 in this program.
C            I       XYRATO * 100
C            I       scale factor used in calculating I   X and Y in
C                    GPOS and GVEC. Equal to SCALEF.
C            I(4)    BLC,TRC integer part (BLC rounded up, TRC down)
C            I(4)    floating remainder * 1000 from BLC, TRC
C            I(4)    10 * (CHOUT) number of characters
C                    outside pix area on left, bot, right, top, resp.
C            I(5)    array location on axes 3,4,5,6,7
C   OR inputs for the TV display of the plot will be prepared.
C   Inputs:
C      BLC     R(2)    number of first pixel in row & col
C      TRC     R(2)    last pixel pos in row/col (usually 1 more
C                      than the number of pixels)
C      XYRATO  R       ratio between x and y axis plotting
C      DEPTH   I(5)    array location on axes 3 thru 7
C   In/Out:
C      CHOUT   R(4)    number of characters outside pix on left,
C                      bottom, right, top, resp.
C      BUFF    I(256)  buffer to use for I/O.
C   Output:
C      IERR    I       error code. 0 = ok.
C                                  1 => disk problems
C   Common:
C      GPHPOS  incremented by 20.
C      GPHRRN  incremented by 1 if a write to disk is needed.
C      GPHX1   set to X1 = BLC(1) (rounded up to integer)
C      GPHX2   set to X2 = TRC(1) (rounded down to integer)
C      GPHY1   set to Y1 = BLC(2) (rounded up to integer)
C      GPHY2   set to Y2 = TRC(2) (rounded down to integer)
C-----------------------------------------------------------------------
      REAL      BLC(2), TRC(2), XYRATO, CHOUT(4)
      INTEGER   DEPTH(5), BUFF(256), IERR
C
      INTEGER   NEED, OPCODE, IER, CMAXTV(2), NXA, NYA, I
      REAL      AX, AY, X, Y, CATR(256)
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATR, GPHCAT)
      DATA NEED, OPCODE /237, 2/
C-----------------------------------------------------------------------
      IERR = 0
      DO 10 I = 1,4
         CHOUT(I) = MAX (CHOUT(I), 0.833)
 10      CONTINUE
C                                       Set common values.
      GPHIX1 = BLC(1) + 0.999
      GPHIY1 = BLC(2) + 0.999
      GPHIX2 = TRC(1) + 0.001
      GPHIY2 = TRC(2) + 0.001
      GPHRX1 = BLC(1) - GPHIX1
      GPHRY1 = BLC(2) - GPHIY1
      GPHRX2 = TRC(1) - GPHIX2
      GPHRY2 = TRC(2) - GPHIY2
      GPHX1 = GPHIX1
      GPHX2 = GPHIX2
      GPHY1 = GPHIY1
      GPHY2 = GPHIY2
C                                       Plot file
      IF (.NOT.GPHDOT) THEN
         IF (GPHPOS.GE.NEED) THEN
            CALL GPHWRT (BUFF, IER)
            IF (IER.NE.0) THEN
               IERR = 1
               GO TO 999
               END IF
            END IF
C                                       Put data in buffer.
         BUFF(GPHPOS+1) = OPCODE
         BUFF(GPHPOS+2) = XYRATO * 100.0 + 0.5
         BUFF(GPHPOS+3) = SCALEF + 0.5
         BUFF(GPHPOS+4) = GPHIX1
         BUFF(GPHPOS+5) = GPHIY1
         BUFF(GPHPOS+6) = GPHIX2
         BUFF(GPHPOS+7) = GPHIY2
         BUFF(GPHPOS+8) = 1000.0 * GPHRX1 + 0.5
         BUFF(GPHPOS+9) = 1000.0 * GPHRY1 + 0.5
         BUFF(GPHPOS+10) = 1000.0 * GPHRX2 + 0.5
         BUFF(GPHPOS+11) = 1000.0 * GPHRY2 + 0.5
         BUFF(GPHPOS+12) = CHOUT(1)*10.0 + 0.5
         BUFF(GPHPOS+13) = CHOUT(2)*10.0 + 0.5
         BUFF(GPHPOS+14) = CHOUT(3)*10.0 + 0.5
         BUFF(GPHPOS+15) = CHOUT(4)*10.0 + 0.5
         CALL COPY (5, DEPTH, BUFF(GPHPOS+16))
C                                       Update buffer counter.
         GPHPOS = GPHPOS + 20
C                                       TV
      ELSE
         GPHCHL = CHOUT(1) * CSIZTV(1) + 0.9
         GPHCHB = CHOUT(2) * CSIZTV(2) + 0.9
         GPHCHR = CHOUT(3) * CSIZTV(1) + 0.9
         GPHCHT = CHOUT(4) * CSIZTV(2) + 0.9
         CALL COPY (5, DEPTH, GPHCAT(IIDEP))
C                                       check number pixels
         CMAXTV(1) = GPHTVW(3) - GPHTVW(1) + 1
         CMAXTV(2) = GPHTVW(4) - GPHTVW(2) + 1
         NXA = CMAXTV(1) - GPHCHL - GPHCHR - 1
         IF (NXA.LE.0) THEN
            GPHTVW(1) = 1
            GPHTVW(3) = MAXXTV(1)
            CMAXTV(1) = MAXXTV(1)
            NXA = MAXXTV(1) - GPHCHL - GPHCHR - 1
            END IF
         NYA = CMAXTV(2) - GPHCHT - GPHCHB - 1
         IF (NYA.LE.0) THEN
            GPHTVW(2) = 1
            GPHTVW(4) = MAXXTV(2)
            CMAXTV(2) = MAXXTV(2)
            NYA = MAXXTV(2) - GPHCHT - GPHCHB - 1
            END IF
         IF ((NXA.LE.0) .OR. (NYA.LE.0)) THEN
            WRITE (MSGTXT,1100) NXA, NYA
            GO TO 990
            END IF
C                                        self scale
         AX = ABS (GPHIX2 - GPHIX1) * XYRATO
         AY = ABS (GPHIY2 - GPHIY1)
         IF ((GPHIX2.EQ.GPHIX1) .OR. (GPHIY2.EQ.GPHIY1)) THEN
            WRITE (MSGTXT,1110) AX, AY
            GO TO 990
            END IF
         IF ((GPHCRN(1).EQ.0) .OR. (GPHCRN(2).EQ.0)) THEN
            X = ABS (GPHIX2+GPHRX2-GPHIX1-GPHRX1) * XYRATO
            Y = ABS (GPHIY2+GPHRY2-GPHIY1-GPHRY1)
            IF ((X.LE.0) .OR. (Y.LE.0)) THEN
               WRITE (MSGTXT,1110) X, Y
               GO TO 990
               END IF
C
            IF ((X/Y).LE.FLOAT(NXA)/FLOAT(NYA)) THEN
               GPHSCY = NYA * AY / Y
               GPHSCX = GPHSCY * AX / AY
            ELSE
               GPHSCX = NXA * AX / X
               GPHSCY = GPHSCX * AY/AX
               END IF
C                                       Center
            NXA = GPHSCX * X / AX + GPHCHL + GPHCHR
            IF (NXA.GT.CMAXTV(1)) THEN
               GPHTVW(1) = 1
               GPHTVW(3) = MAXXTV(1)
               CMAXTV(1) = MAXXTV(1)
               END IF
            GPHIX0 = GPHTVW(1) + GPHCHL + MAX (0, CMAXTV(1)-NXA)/2
     *         + 0.9 - GPHRX1 * XYRATO * GPHSCX / AX
            NYA = GPHSCY * Y/AY + GPHCHB + GPHCHT
            IF (NYA.GT.CMAXTV(2)) THEN
               GPHTVW(2) = 1
               GPHTVW(4) = MAXXTV(2)
               CMAXTV(2) = MAXXTV(2)
               END IF
            GPHIY0 = GPHTVW(2) + GPHCHB + MAX (0, CMAXTV(2)-NYA)/2
     *         + 0.9 - GPHRY1 * GPHSCY / AY
C                                        pixel scaling
         ELSE
            GPHIX0 = GPHCRN(1)
            GPHIY0 = GPHCRN(2)
            GPHSCX = GPHIX2 - GPHIX1
            GPHSCY = GPHIY2 - GPHIY1
            END IF
C                                       catalog block
         GPHCAT(IIWIN  ) = GPHIX1
         GPHCAT(IIWIN+1) = GPHIY1
         GPHCAT(IIWIN+2) = GPHIX2
         GPHCAT(IIWIN+3) = GPHIY2
         GPHCAT(IICOR  ) = GPHIX0
         GPHCAT(IICOR+1) = GPHIY0
         GPHCAT(IICOR+2) = GPHIX0 + GPHSCX
         GPHCAT(IICOR+3) = GPHIY0 + GPHSCY
         CALL CHR2H (2, '  ', 1, GPHCAT(IITRA))
         CATR(IRRAN) = 0.0
         CATR(IRRAN+1) = 0.0
         END IF
      GO TO 999
C                                       error
 990  CALL MSGWRT (7)
      IERR = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('GINITL: NOT ROOM ON TV FOR CHARACTERS: NXA, NYA =',2I7)
 1110 FORMAT ('GINITL: NOT ENOUGH PIXELS SPECIFIED: X,Y =',2E10.3)
      END
