      SUBROUTINE ZDOPRT (IVOL, LUN, NCOPY, PNAME, ISIZE, INBLK, IERR)
C-----------------------------------------------------------------------
C! reads bit file and causes it to be plotted on printer/plotter
C# Z Printer Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   Read a bit map such as produced by PRTDRW and convert it into a
C   file that can be spooled to a Versatec printer/plotter.
C   Inputs:
C      IVOL     I      Disk volume containing file, 1,2,3,...
C      LUN      I      Logical unit number
C      NCOPY    I      Number of copies of the plot to make
C      PNAME    C*48   Physical file name (left justified)
C      ISIZE    I      Size of INBLK in words
C   In/Out:
C      INBLK    I(*)   Scratch buffer
C   Output:
C      IERR     I      Error return code: 0 => no error
C                         1 => error assigning plotter
C                         2 => error doing plot
C   VAX VERSION: works only for rev C or higher of the VERSATEC driver.
C   This version spools the plot to the device queue with the logical
C   name VERPLOT.  We use I*2 buffer - ISIZE in AIPS words (4 real
C   bytes each).
C-----------------------------------------------------------------------
      INTEGER   IVOL, LUN, NCOPY, ISIZE, IERR
      INTEGER*2 INBLK(*)
      CHARACTER PNAME*48
C
      INTEGER*2 IOBLK(65), FF, IPLOT, LIST2(8)
      CHARACTER MESSAG*64, CTEMP*1, DEVICQ*64, PLTFIL*13
      REAL      DELAY1, DELAY3
      INTEGER   FORERR, RMSERR, MSGLEN, JERR, SYS$CRELNM, ISIZE2, XLNB,
     *   INPOS, IOPOS, INIBLK, IRRN, IMLINE, ILINE, NBYTES, FIND, I,
     *   II, RBYTES, LIST4(4)
      LOGICAL   EXCL, NOMAP, WAIT, MAP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE '($LNMDEF)'
      EQUIVALENCE (LIST2, LIST4)
      EQUIVALENCE (EXCL, WAIT, MAP)
      DATA FF, IPLOT /'1013'X, '0012'X/
      DATA EXCL, NOMAP /.TRUE.,.FALSE./
      DATA DELAY1, DELAY3 /1.0, 3.0/
C-----------------------------------------------------------------------
C                                       Open the bit map file.
      CALL ZOPEN (LUN, FIND, IVOL, PNAME, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         IERR = 1
         GO TO 995
         END IF
C                                       Find out Queue name for plotter.
      CALL ZTRLOG (7, 'VERPLOT', 64, DEVICQ, XLNB, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010)
         IERR = 1
         GO TO 990
         END IF
C                                       Assign SYS$PRINT to plotter
C                                       device queue.
      LIST2(1) = XLNB
      LIST2(2) = LNM$_STRING
      LIST4(2) = %LOC (DEVICQ)
      LIST4(3) = 0
      LIST4(4) = 0
      JERR = SYS$CRELNM ( , 'LNM$PROCESS_TABLE', 'SYS$PRINT', , LIST4)
      IF (JERR.NE.1) THEN
         WRITE (MSGTXT,1012) JERR
         IERR = 1
         GO TO 990
         END IF
C                                       Initial buffer counters.
      INPOS = 257
      IOPOS = 1
      IRRN = 1
      NBYTES = ((2 * ISIZE) / NBPS) * NBPS
      ILINE = 0
      ISIZE2 = NBYTES
      CALL ZMIO ('READ', LUN, FIND, IRRN, NBYTES, INBLK, 1, IERR)
      IF (IERR.EQ.0) CALL ZWAIT (LUN, FIND, 1, IERR)
      IF (IERR.NE.0) GO TO 110
      IRRN = IRRN + NBYTES / NBPS
      IMLINE = INBLK(1)
      RBYTES = 512 + ((MAXXPR(1)-1) / NBITWD + 1) * IMLINE * 2 - NBYTES
C                                       Open the VERSATEC (spool file)
C                                       Figure out an initial size.
      INIBLK = .58 * IMLINE
C                                       Try for each available disk.
      DO 20 I = 1,NVOL
         II = I - 1
         CALL ZEHEX (II, 1, CTEMP)
         PLTFIL = 'DA0' // CTEMP // ':PLOT.DAT'
         OPEN (UNIT=1, FILE=PLTFIL, STATUS='NEW', FORM='FORMATTED',
     *      ACCESS='SEQUENTIAL', CARRIAGECONTROL='NONE', DISP='PRINT',
     *      RECORDTYPE='VARIABLE', INITIALSIZE=INIBLK, IOSTAT=IERR)
         IF (IERR.EQ.0) GO TO 50
            WRITE (MSGTXT,1015) IERR, I
            CALL MSGWRT (7)
 20      CONTINUE
      MSGTXT = 'CAN''T FIND ROOM - QUITTING'
      GO TO 990
C                                       Begin master loop.
 50   CONTINUE
C                                       read new buff
         IF (INPOS.GT.ISIZE2) THEN
            IF (RBYTES.LE.0) GO TO 100
C                                       Last input buffer.
            IF (RBYTES.LT.NBYTES) THEN
               NBYTES = RBYTES
               ISIZE2 = NBYTES
               END IF
            CALL ZMIO ('READ', LUN, FIND, IRRN, NBYTES, INBLK, 1, IERR)
            IF (IERR.EQ.0) CALL ZWAIT (LUN, FIND, 1, IERR)
            IF (IERR.NE.0) GO TO 110
            RBYTES = RBYTES - NBYTES
            IRRN = IRRN + NBYTES / NBPS
            INPOS = 1
            END IF
C                                       Determine length to copy.
         XLNB = MIN (ISIZE2+1-INPOS, 66-IOPOS)
         CALL ZBYTFL (XLNB, INBLK(INPOS), IOBLK(IOPOS))
         IOPOS = IOPOS + XLNB
         INPOS = INPOS + XLNB
C                                       Write to VERSATEC spool file.
         IF (IOPOS.GT.65) THEN
            WRITE (1,1050,ERR=120) IPLOT, IOBLK
            ILINE = ILINE + 1
            IOPOS = 1
            END IF
         GO TO 100
C                                       Flush last buffer.
 100  IOPOS = IOPOS - 1
      IF (IOPOS.GT.0) WRITE (1,1050,ERR=120) IPLOT, (IOBLK(I),
     *   I = 1,IOPOS)
      GO TO 150
C                                       Error handling.
 110  WRITE (MSGTXT,1110) IERR
      CALL MSGWRT (7)
      GO TO 150
C
 120  CALL ERRSNS (FORERR, RMSERR)
      CALL SYS$GETMSG (%VAL(RMSERR), MSGLEN, MESSAG, %VAL(1), )
      WRITE (MSGTXT,1120)
      CALL MSGWRT (7)
      MSGTXT = MESSAG
      CALL MSGWRT (7)
C                                       Issue final form feed and
C                                       close files.
 150  WRITE (1,1050,ERR=180) FF
      NCOPY = NCOPY - 1
      IF (NCOPY.LE.0) GO TO 180
         DO 170 I = 1,NCOPY
            CLOSE (UNIT=1)
C                                       Give spooler chance to get file.
            CALL ZDELAY (DELAY3, IERR)
C                                       Try a few times to open file.
            DO 160 II = 1,10
               CALL ZDELAY (DELAY1, IERR)
               OPEN (UNIT=1, FILE=PLTFIL, STATUS='OLD', DISP='PRINT',
     *            ACCESS='SEQUENTIAL', FORM='FORMATTED', IOSTAT=IERR,
     *            CARRIAGECONTROL='NONE', RECORDTYPE='VARIABLE')
               IF (IERR.EQ.0) GO TO 170
 160           CONTINUE
            WRITE (MSGTXT,1160)
            CALL MSGWRT (6)
 170        CONTINUE
 180  CLOSE (UNIT=1, DISP='PRINT/DELETE')
      CALL ZCLOSE (LUN,FIND,IERR)
      GO TO 999
C                                       Error close downs
 980  CALL MSGWRT (7)
      CLOSE (UNIT=1, DISP='PRINT/DELETE')
      CALL ZCLOSE (LUN, FIND, IERR)
      GO TO 999
C
 990  CALL MSGWRT (7)
      CALL ZCLOSE (LUN, FIND, IERR)
      GO TO 999
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZDOPRT: UNABLE TO OPEN BIT MAP FILE. IERR=',I4)
 1010 FORMAT ('ZDOPRT: NO LOGICAL NAME ''VERPLOT''')
 1012 FORMAT ('ZDOPRT: COULD NOT ASSIGN LOGICAL NAME, IERR=',I8)
 1015 FORMAT ('ZDOPRT: ERR ',I4,' OPENING SPOOL FILE ON DISK',I3,
     *   '. TRYING NEXT DISK')
 1050 FORMAT (66A2)
 1110 FORMAT ('ZDOPRT: ZMIO READ ERROR. IERR=',I4)
 1120 FORMAT ('ZDOPRT: FORTRAN WRITE ERROR TO PLOTTER')
 1160 FORMAT ('ZDOPRT: COULD NOT REOPEN FILE FOR NEXT COPY')
      END
