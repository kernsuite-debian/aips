      SUBROUTINE GFINIS (BUFF, IERR)
C-----------------------------------------------------------------------
C! writes the end of plot record into a plot file and closes it down
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2002-2003
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
C   GFINIS places an "end of plot" command in the buffer, writes the
C   last buffer to disk, compresses the plot file if needed, and
C   closes the plot file.  ******  NOTE: any catalog operations for
C   the plot file must be performed by the calling program. *******
C   The plot common is reinitialized in part.
C   The command record has the form
C            I     opcode (32767)
C   In TV mode, it closes the TV, updates the image catalog, and
C   reports any errors.
C   In/out:
C      BUFF  I(256)     plot file work buffer
C   Output:
C      IERR  I          error code: 0 => ok
C                         -1 => user requested stop on multi-page TV
C                          1 => disk error
C                          2 => data error
C                          3 => compress error
C                          4 => close error
C-----------------------------------------------------------------------
      INTEGER   BUFF(256), IERR
C
      INTEGER   NEED, OPCODE, IER, ISIZE, IMAWIN(4), IQ, IB, I, NC, IC
      REAL      RPOS(2), TD
      LOGICAL   F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA NEED, OPCODE /256, 32767/
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       plot file
      IF (.NOT.GPHDOT) THEN
C                                       fill & out last rec(s)
         IERR = 1
         IF (GPHPOS.GE.NEED) THEN
            CALL GPHWRT (BUFF, IER)
            IF (IER.NE.0) GO TO 999
            END IF
         BUFF(GPHPOS+1) = OPCODE
         GPHPOS = GPHPOS + 1
         CALL GPHWRT (BUFF, IER)
         IF (IER.NE.0) GO TO 999
C                                       size info message:temp ?
         WRITE (MSGTXT,1000) GPHRRN
         CALL MSGWRT (2)
C                                       compress file: check size
         IF (GPHSIZ.GT.GPHRRN) THEN
            ISIZE = GPHRRN + 1
            IERR = 3
            CALL ZCMPRS (GPHVOL, GPHNAM, GPHLUN, ISIZE, IER)
            IF (IER.NE.0) GO TO 999
            END IF
C                                       close
         IERR = 4
         CALL ZCLOSE (GPHLUN, GPHIND, IER)
         IF (IER.EQ.0) IERR = 0
C                                       TV mode
      ELSE
         CALL YHOLD ('OFFF', IER)
C                                       grey image catalog
         IMAWIN(1) = GPHIX0
         IMAWIN(2) = GPHIY0
         IMAWIN(3) = GPHIX0 + GPHIX2 - GPHIX1
         IMAWIN(4) = GPHIY0 + GPHIY2 - GPHIY1
         DO 10 I = 1,NGRAY
            IF (GPHCON(I).GT.0) THEN
               IF (I.EQ.GPHTVC(2)) CALL RCOPY (2, GPHRNG(1,2),
     *            GPHCAT(IRRAN))
               IF (I.EQ.GPHTVC(3)) CALL RCOPY (2, GPHRNG(1,3),
     *            GPHCAT(IRRAN))
               CALL YCWRIT (I, IMAWIN, GPHCAT, GPHSCR, IER)
               IF (IER.NE.0) THEN
                  WRITE (MSGTXT,1100) IER, GPHTVC
                  CALL MSGWRT (6)
                  END IF
               END IF
 10         CONTINUE
C                                       graphics plane catalog
         IMAWIN(1) = 1
         IMAWIN(2) = 1
         IMAWIN(3) = MAXXTV(1)
         IMAWIN(4) = MAXXTV(2)
         DO 20 I = NGRAY+1,NGRAY+8
            IF (GPHCON(I).GT.0) THEN
               CALL YCWRIT (I, IMAWIN, GPHCAT, GPHSCR, IER)
               IF (IER.NE.0) THEN
                  WRITE (MSGTXT,1110) IER
                  CALL MSGWRT (6)
                  END IF
               END IF
 20         CONTINUE
         IF (GPHERV.GT.0) THEN
            WRITE (MSGTXT,1120) GPHERV
            CALL MSGWRT (3)
            END IF
         IF (GPHERC.GT.0) THEN
            WRITE (MSGTXT,1121) GPHERC
            CALL MSGWRT (3)
            END IF
         IF (GPHERG.GT.0) THEN
            WRITE (MSGTXT,1122) GPHERG
            CALL MSGWRT (3)
            END IF
         IF (GPHERO.GT.0) THEN
            WRITE (MSGTXT,1123) GPHERO
            CALL MSGWRT (3)
            END IF
C                                       there will be another page
C                                       wait for 30 seconds or button
         IF (GPHPAG) THEN
            MSGTXT = 'Waiting 30 seconds:'
            CALL MSGWRT (1)
            MSGTXT = '   Hit TV button A to pause indefinitely.' //
     *         '  Hit button'
            CALL MSGWRT (1)
            MSGTXT = '   B or C to continue sooner, button D' //
     *         ' to stop plotting'
            CALL MSGWRT (1)
            RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2.0
            RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2.0
            TD = 3. / SQRT (MAX (1.0, AIPSMK))
            CALL YCURSE ('ONNN', F, F, RPOS, IQ, IB, IERR)
            IF (IERR.NE.0) GO TO 145
            NC = 30.0 / TD + 0.5
            IC = NC
 130        CONTINUE
            DO 140 I = 1,NC
               CALL ZDELAY (TD, IERR)
               CALL YCURSE ('READ', F, F, RPOS, IQ, IB, IERR)
               IF ((IB.GT.0) .OR. (IERR.NE.0)) THEN
                  IF ((IB.EQ.1) .AND. (NC.LE.IC)) THEN
                     NC = 3000.0 / TD + 0.5
                     MSGTXT = 'Pausing indefinitely:'
                     CALL MSGWRT (1)
                     MSGTXT = '   Hit TV button A, B, or C to continue,'
     *                  // ' D to quit'
                     CALL MSGWRT (1)
                     GO TO 130
                  ELSE
                     GO TO 145
                     END IF
                  END IF
 140           CONTINUE
C                                       Wants to quit
 145        IF (IB.GT.7) THEN
               WRITE (MSGTXT,1145)
               CALL MSGWRT (3)
               IF (IERR.EQ.0) IERR = -1
            ELSE
               WRITE (MSGTXT,1146)
               CALL MSGWRT (1)
               END IF
C                                       Off cursor
            CALL YCURSE ('OFFF', F, F, RPOS, IQ, IB, IER)
            END IF
C                                       close TV
         CALL TVCLOS (GPHSCR, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1150) IER
            CALL MSGWRT (6)
            END IF
         END IF
C                                        clear common some
      GPHSIZ = 0
      GPHLUN = 0
      GPHPOS = 0
      GPHRRN = 0
      GPHVOL = 0
      GPHNAM = ' '
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GFINIS: number records used',I8)
 1100 FORMAT ('GFINIS: ERROR',I5,' UPDATING CHANNEL',I3,
     *   ' IN IMAGE CATALOG')
 1110 FORMAT ('GFINIS: ERROR',I5,' UPDATING GRAPHICS CHANNEL',
     *   ' IN IMAGE CATALOG')
 1120 FORMAT (I8,' vectors truncated at edges')
 1121 FORMAT (I8,' character strings truncated at edges')
 1122 FORMAT (I8,' grey scale lines truncated at edges')
 1123 FORMAT (I8,' character strings omitted due to overlaps')
 1145 FORMAT ('Button D was hit - stop plots now')
 1146 FORMAT ('Continuing')
 1150 FORMAT ('GFINIS: ERROR',I5,' CLOSING THE TV')
      END
