      SUBROUTINE GTBWRT (SOURCE, MSOU, TIMES, NCOL, NCOLMX, IOBUF, ITYP,
     *   LUN, FIND, LAST, IROW, NROW, NUMBUF, RBUFF, SBUFF, NBUFF,
     *   IBPTR, RMAX, RMIN, IERR)
C-----------------------------------------------------------------------
C! Routine used by GRIDTB, GRIDTC to write buffers.
C# IO-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2009
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
C   Routine to write buffers to disk used by GRIDTB.
C   Note: GTBWRT is called before filling the buffer.
C   Inputs:
C      SOURCE   I          Source id number
C      SOU      I(*)       List of source numbers corresponding to times
C      TIMES    R(*)       List of start times of each row in days
C      NCOL     I          Number of columns
C      NCOLMX   I          Max # columns = 1st dimension SBUFF, NBUFF
C      IOBUF    I          First dimension of RBUFF
C      ITYP     I          Output data type: 0=amp (scalar avg), 1=amp,
C                             2=phase, 3=rms, 4=amp rms/mean, 5=real,
C                             6=imag, 7=real and imaginary
C      LUN      I(*)       Luns to use (1/buffer)
C      FIND     I(*)       FTAB pointers (1/buffer)
C      IROW     I          Current row number for grid.
C      LAST     L          If true then this is the last call for the
C                          current planes being gridded.
C      NUMBUF   I          Number of buffers to write
C   Input/Output:
C      NROW     I          Next row to grid 0 on first call.
C      RBUFF    R(IOBUF,*)  Output buffer, on input contains real part
C      SBUFF    R(mx,*)    Imaginary part (ITYP=7) this row only.
C                          (mx == NCOLMX)
C      NBUFF    I(mx,*)    Count for RBUFF, SBUFF
C      IBPTR    I(*)       Buffer pointers (1/buffer)
C      RMAX     R          Maximum value in image.
C      RMIN     R          Minimum value in image.
C   Output:
C      IERR     I          Return code, 0=OK, else MDISK error.
C-----------------------------------------------------------------------
      INTEGER   SOURCE, MSOU(*), NCOL, NCOLMX, IOBUF, ITYP, LUN(*),
     *   FIND(*), IROW, NROW, NUMBUF, NBUFF(NCOLMX,*), IBPTR(*), IERR
      LOGICAL   LAST
      REAL      TIMES(*), RBUFF(IOBUF,*), SBUFF(NCOLMX,*), RMAX, RMIN
C
      INTEGER   JTYP, COUNT, IBUFF, MPTR, LROW, LOOP, J, NBLANK, J1, J2
      REAL      VALUE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (NROW.LT.1) GO TO 600
C                                       Average
      JTYP = ITYP + 1
      J1 = -1
      J2 = 1
      GO TO (50, 100, 150, 200, 250, 50, 350, 400), JTYP
C                                       amp, scalar averaged or real
 50      DO 70 IBUFF = 1,NUMBUF
            J = IBPTR(IBUFF)
            DO 60 LOOP = 1,NCOL
               IF (RBUFF(J,IBUFF).NE.FBLANK) THEN
                  COUNT = MAX (1, NBUFF(LOOP,IBUFF))
                  RBUFF(J,IBUFF) = RBUFF(J,IBUFF) / COUNT
                  END IF
               J = J + 1
 60            CONTINUE
 70         CONTINUE
         GO TO 500
C                                       amp, vector average
 100     DO 120 IBUFF = 1,NUMBUF
            J = IBPTR(IBUFF)
            DO 110 LOOP = 1,NCOL
               IF (RBUFF(J,IBUFF).NE.FBLANK) THEN
                  COUNT = MAX (1, NBUFF(LOOP,IBUFF))
                  RBUFF(J,IBUFF) = SQRT (RBUFF(J,IBUFF)*RBUFF(J,IBUFF) +
     *               SBUFF(LOOP,IBUFF)*SBUFF(LOOP,IBUFF)) / COUNT
                  END IF
               J = J + 1
 110           CONTINUE
 120        CONTINUE
         GO TO 500
C                                       Phase
 150     DO 170 IBUFF = 1,NUMBUF
            J = IBPTR(IBUFF)
            DO 160 LOOP = 1,NCOL
               IF ((RBUFF(J,IBUFF).EQ.0.0) .AND.
     *            (SBUFF(LOOP,IBUFF).EQ.0.0)) RBUFF(J,IBUFF) = FBLANK
               IF (RBUFF(J,IBUFF).NE.FBLANK) RBUFF(J,IBUFF) =
     *            ATAN2 (SBUFF(LOOP,IBUFF), RBUFF(J,IBUFF)) * 57.29578
               J = J + 1
 160           CONTINUE
 170        CONTINUE
         GO TO 500
C                                       rms
 200     DO 220 IBUFF = 1,NUMBUF
            J = IBPTR(IBUFF)
            DO 210 LOOP = 1,NCOL
               COUNT = NBUFF(LOOP,IBUFF)
               IF (RBUFF(J,IBUFF).NE.FBLANK) THEN
                  IF (COUNT.LE.1) THEN
                     RBUFF(J,IBUFF) = 0.0
                  ELSE
                     RBUFF(J,IBUFF) = RBUFF(J,IBUFF) / COUNT
                     SBUFF(LOOP,IBUFF) = SBUFF(LOOP,IBUFF) / COUNT
                     VALUE = SBUFF(LOOP,IBUFF) -
     *                  RBUFF(J,IBUFF) * RBUFF(J,IBUFF)
                     VALUE = MAX (0.0, VALUE)
                     RBUFF(J,IBUFF) = SQRT (VALUE)
                     END IF
                  END IF
               J = J + 1
 210           CONTINUE
 220        CONTINUE
         GO TO 500
C                                       rms / mean
 250     DO 270 IBUFF = 1,NUMBUF
            J = IBPTR(IBUFF)
            DO 260 LOOP = 1,NCOL
               COUNT = NBUFF(LOOP,IBUFF)
               IF (RBUFF(J,IBUFF).NE.FBLANK) THEN
                  IF (COUNT.LE.1) THEN
                     RBUFF(J,IBUFF) = 0.0
                  ELSE
                     RBUFF(J,IBUFF) = RBUFF(J,IBUFF) / COUNT
                     SBUFF(LOOP,IBUFF) = SBUFF(LOOP,IBUFF) / COUNT
                     VALUE = SBUFF(LOOP,IBUFF) -
     *                  RBUFF(J,IBUFF) * RBUFF(J,IBUFF)
                     VALUE = MAX (0.0, VALUE)
                     IF (ABS (RBUFF(J,IBUFF)).LT.1.0E-20)
     *                  RBUFF(J,IBUFF) = 1.0
                     RBUFF(J,IBUFF) = SQRT (VALUE) / RBUFF(J,IBUFF)
                     END IF
                  END IF
               J = J + 1
 260           CONTINUE
 270        CONTINUE
         GO TO 500
C                                       imag
 350     DO 370 IBUFF = 1,NUMBUF
            J = IBPTR(IBUFF)
            DO 360 LOOP = 1,NCOL
               IF (NBUFF(LOOP,IBUFF).GT.0) THEN
                  RBUFF(J,IBUFF) = SBUFF(LOOP,IBUFF) / NBUFF(LOOP,IBUFF)
               ELSE
                  RBUFF(J,IBUFF) = FBLANK
                  END IF
               J = J + 1
 360           CONTINUE
 370        CONTINUE
         GO TO 500
C                                       source, time + real & imag & wt
 400     J1 = 3
         J2 = 3
         DO 420 IBUFF = 1,NUMBUF
            J = IBPTR(IBUFF)
C                                       Source number
            RBUFF(J,IBUFF) = SOURCE
            RBUFF(J+1,IBUFF) = TIMES(NROW)
            RBUFF(J+2,IBUFF) = TIMES(NROW+1)
            J = J + J1
            DO 410 LOOP = 1,NCOL
               IF (RBUFF(J,IBUFF).NE.FBLANK) THEN
                  COUNT = MAX (1, NBUFF(LOOP,IBUFF))
                  RBUFF(J,IBUFF) = RBUFF(J,IBUFF) / COUNT
                  RBUFF(J+1,IBUFF) = SBUFF(LOOP,IBUFF) / COUNT
                  RBUFF(J+2,IBUFF) = 0.0
                  END IF
               J = J + J2
 410           CONTINUE
 420        CONTINUE
C                                       Find max, min Real/Imag
         DO 450 IBUFF = 1,NUMBUF
C                                       Max
            J = IBPTR(IBUFF) + J1
            DO 430 LOOP = 1,NCOL
               IF (RBUFF(J,IBUFF).NE.FBLANK) RMAX = MAX (RMAX,
     *            RBUFF(J,IBUFF), RBUFF(J+1,IBUFF))
               J = J + J2
 430           CONTINUE
C                                       Min
            J = IBPTR(IBUFF) + J1
            DO 440 LOOP = 1,NCOL
               IF (RBUFF(J,IBUFF).NE.FBLANK) RMIN = MIN (RMIN,
     *            RBUFF(J,IBUFF), RBUFF(J+1,IBUFF))
               J = J + J2
 440           CONTINUE
 450        CONTINUE
         GO TO 600
C                                       Find max, min, others
 500  DO 560 IBUFF = 1,NUMBUF
C                                       Max
         J = IBPTR(IBUFF) + J1
         DO 530 LOOP = 1,NCOL
            IF (RBUFF(J,IBUFF).NE.FBLANK) RMAX = MAX (RMAX,
     *         RBUFF(J,IBUFF))
            J = J + J2
 530        CONTINUE
C                                       Min
         J = IBPTR(IBUFF) + J1
         DO 540 LOOP = 1,NCOL
            IF (RBUFF(J,IBUFF).NE.FBLANK) RMIN = MIN (RMIN,
     *         RBUFF(J,IBUFF))
            J = J + J2
 540        CONTINUE
 560     CONTINUE
C                                       Do writes
 600  NROW = NROW + 1
      DO 700 IBUFF = 1,NUMBUF
         DO 650 LROW = NROW,IROW
            CALL MDISK ('WRIT', LUN(IBUFF), FIND(IBUFF), RBUFF(1,IBUFF),
     *         IBPTR(IBUFF), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1600) IERR, LROW
               GO TO 990
               END IF
            MPTR = IBPTR(IBUFF) - 1
            NBLANK = NCOL
            IF (ITYP.EQ.7) NBLANK = NCOL * 3 + 3
C                                       Initialize accumulators
            DO 610 LOOP = 1,NBLANK
               RBUFF(MPTR+LOOP,IBUFF) = FBLANK
 610           CONTINUE
            IF (ITYP.EQ.7) THEN
               RBUFF(MPTR+1,IBUFF) = MSOU(LROW)
               RBUFF(MPTR+2,IBUFF) = TIMES(LROW)
               RBUFF(MPTR+3,IBUFF) = TIMES(LROW+1)
               END IF
            IF (.NOT.LAST) THEN
               NBLANK = NCOL
               DO 620 LOOP = 1,NBLANK
                  SBUFF(LOOP,IBUFF) = 0.0
                  NBUFF(LOOP,IBUFF) = 0
 620              CONTINUE
               END IF
 650        CONTINUE
 700     CONTINUE
      NROW = IROW
C                                       Flush buffer
      IF (LAST) THEN
         DO 800 IBUFF = 1,NUMBUF
            CALL MDISK ('FINI', LUN(IBUFF), FIND(IBUFF),
     *         RBUFF(1,IBUFF), IBPTR(IBUFF), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1700) IERR, IBUFF
               GO TO 990
               END IF
 800        CONTINUE
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1600 FORMAT ('GTBWRT: UNABLE TO WRITE OUTPUT IMAGE FILE - ERROR',I5,
     *   ' ROW',I5)
 1700 FORMAT ('GTBWRT: ERROR ',I3,' FLUSHING THE BUFFER',I3)
      END
