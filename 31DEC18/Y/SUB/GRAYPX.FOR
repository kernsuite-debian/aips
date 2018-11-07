      SUBROUTINE GRAYPX (NPIX, IANGL, IVALS, BUFF, IERR)
C-----------------------------------------------------------------------
C! writes an array of grey values into a plot file or to the TV
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002
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
C   GRAYPX places a "write string of gray values" command in the
C   graphics file.  The gray values are clipped using range given in the
C   call to GINITG and placed in graphics file.  The command form is:
C            I          opcode (7)
C            I          NPIX: number of values
C            I          IANGL; 0 horizontal, 1 vertical
C            I(NPIX)    clipped gray values
C   NOTE: this command may extend over more than 1 physical record
C   in the file!!!!!!!!!!
C   May go to TV in that mode instead.
C   Inputs:
C      NPIX   I          number of pixel values
C      IANGL  I          direction code: 0 horizontal, 1 vertical
C      IVALS  I(NPIX)    pixel values
C   In/out:
C      BUFF   I(256)     graphics working buffer
C   Output:
C      IERR   I          error code:  0 => ok
C                                     1 => disk error
C                                     2 => input data error
C-----------------------------------------------------------------------
      INTEGER   NPIX, IANGL, IVALS(*), BUFF(256), IERR
C
      INTEGER   OPCODE, NEED, I, IER, JANGL, MPIX, LPIX
      REAL      GROFF, GRSCAL
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA OPCODE, NEED /7, 254/
C-----------------------------------------------------------------------
      IERR = 0
      IF (NPIX.LE.0) GO TO 999
      IF (GPHTHI.LE.GPHTLO) THEN
         IERR = 2
         WRITE (MSGTXT,1000)
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Plot file
      IF (.NOT.GPHDOT) THEN
C                                        write record if needed
         IERR = 1
         IF (GPHPOS.GE.NEED) THEN
            CALL GPHWRT (BUFF, IER)
            IF (IER.NE.0) GO TO 999
            END IF
C                                        add variables
         BUFF(GPHPOS+1) = OPCODE
         BUFF(GPHPOS+2) = NPIX
         BUFF(GPHPOS+3) = IANGL
         GPHPOS = GPHPOS + 3
C                                        add gray vals: write rec as
C                                        needed
         DO 20 I = 1,NPIX
            IF (GPHPOS.GE.256) THEN
               CALL GPHWRT (BUFF, IER)
               IF (IER.NE.0) GO TO 999
               END IF
            GPHPOS = GPHPOS + 1
            BUFF(GPHPOS) = MAX (GPHTLO, MIN (GPHTHI, IVALS(I)))
 20         CONTINUE
         IERR = 0
C                                       TV
      ELSE
         GROFF = GPHTLO
         GRSCAL = REAL (MAXINT) / (REAL (GPHTHI) - REAL (GPHTLO))
C                                       NOTE: plot file angle 1 => 3
         JANGL = IANGL
         IF (JANGL.EQ.1) JANGL = 3
         MPIX = NPIX
         GPHNGR = GPHNGR + 1
C                                        update catalog, screen
         CALL GCINIT (GPHTVC(1), 0, IERR)
         TVSUBR = 'GCINIT'
         IF (IERR.NE.0) GO TO 960
         LPIX = 1
         DO 110 I = 1,MPIX
            GPHSCR(I) = MAX (GPHTLO, MIN (GPHTHI, IVALS(I)))
            GPHSCR(I) = (GPHSCR(I) - GROFF) * GRSCAL + 0.5
 110        CONTINUE
         IF (JANGL.EQ.0) THEN
            IF ((GPHIYL.LT.1) .OR. (GPHIYL.GT.MAXXTV(2))) THEN
               GPHERG = GPHERG + 1
               GO TO 999
               END IF
            IF (GPHIXL+MPIX-1.GT.MAXXTV(1)) THEN
               GPHERG = GPHERG + 1
               MPIX = MAXXTV(1) + 1 - GPHIXL
               END IF
            IF (GPHIXL.LE.0) THEN
               LPIX = 2 - GPHIXL
               MPIX = MPIX - LPIX + 1
               GPHERG = GPHERG + 1
               END IF
         ELSE IF (JANGL.EQ.3) THEN
            IF ((GPHIXL.LT.1) .OR. (GPHIXL.GT.MAXXTV(1))) THEN
               GPHERG = GPHERG + 1
               GO TO 999
               END IF
            IF (GPHIYL-MPIX+1.LT.1) THEN
               GPHERG = GPHERG + 1
               MPIX = GPHIYL
               END IF
            IF (GPHIYL.GT.MAXXTV(2)) THEN
               LPIX = GPHIYL - MAXXTV(2) + 1
               MPIX = MPIX - LPIX + 1
               GPHERG = GPHERG + 1
               END IF
            END IF
         IF (MPIX.LE.0) GO TO 999
C                                        send to TV
         CALL YIMGIO ('WRIT', GPHTVC, GPHIXL, GPHIYL, JANGL, MPIX,
     *      GPHSCR(LPIX), IERR)
         TVSUBR = 'YIMGIO'
         IF (IERR.NE.0) GO TO 960
         GPHCNT = GPHCNT + 8
         IF (GPHCNT.GT.5000) THEN
            CALL YHOLD ('OFFF', IER)
            CALL YHOLD ('ONNN', IER)
            GPHCNT = 0
            END IF
         END IF
      GO TO 999
C                                       TV error
 960  WRITE (MSGTXT,1960) TVSUBR, IERR
      CALL MSGWRT (7)
      IERR = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GRAYPX: GINITG WAS NOT CALLED')
 1960 FORMAT ('GRAYPX: TV ROUTINE ',A,' RETURNS ERROR',I5)
      END
