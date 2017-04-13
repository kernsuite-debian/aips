      SUBROUTINE APROLL (LUN, NWORD, BUFFER, BUFSZ, DELAY, IRET)
C-----------------------------------------------------------------------
C! Copies AP "memory" to disk, gives up AP then reloads AP
C# AP-util IO-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2006
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
C   APROLL copies the first NWORDs of AP MD memory to a scratch file,
C   gives up the AP, does a task delay for DELAY, grabs an AP and loads
C   the scratch file back into the AP.  NOTE: this subroutine uses
C   commom /CFILES/ for the scratch file.  Scratch file of "type" 'SC'
C   created on the first call and used in subsequent calls, thus NWORD
C   on the first call should be the largest value to be encountered.
C***********************************************************************
C   IMPORTANT NOTE:   APROLL works properly only for floating point
C   data.  Integer values rolled will not be restored correctly.
C***********************************************************************
C   Inputs: LUN     I       LUN to use for I/O
C           NWORD   I       Number of words of AP memory to save.
C                           IF <= 0, just give up AP, delay, grab it.
C           BUFFER  R(*)    Work buffer.  Should be > 6*NBPS.
C           BUFSZ   I       Size of BUFFER in bytes.
C           DELAY   R       Task delay time (sec)
C   Output: IRET    I       Return error code: 0 => OK
C                              1 => couldn't roll - data still in AP
C                              2 => couldn't reload AP.
C-----------------------------------------------------------------------
      INTEGER   LUN, NWORD, BUFSZ, IRET
      REAL      BUFFER(*), DELAY
C
      CHARACTER NAME*48
      INTEGER   NSAVE, VO, BO, ISIZE, IAPLEN, IAPLOC
      INTEGER   LRECL, LENBU, NLOOP, ILOOP, NIO, J, I, IERR,
     *   FIND, BIND, CATBLK(256)
      LOGICAL   T, F
      DOUBLE PRECISION    XSIZE, XTEMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      COMMON /MAPHDR/ CATBLK
      DATA BO, VO /1, 0/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IF (NWORD.LE.0) GO TO 100
      IRET = 1
C                                       Determine size of transfer.
C                                       Single buffer.
      LENBU = (BUFSZ / (2.0)) - (NBPS / 2.0)
      LRECL = 1
C                                       Check number of scratch files.
      IF (NSCR.LT.20) GO TO 10
         WRITE (MSGTXT,1000) NSCR
         GO TO 990
C                                       Create scratch file.
 10   ISIZE = (NWORD - 1) /256 + 3
      CALL SCREAT (ISIZE, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IERR
         GO TO 990
C                                       Initialize disk I/O.
 20   CALL ZPHFIL ('SC', SCRVOL(NSCR), SCRCNO(NSCR), 1, NAME, IERR)
      CALL ZOPEN (LUN, FIND, SCRVOL(NSCR), NAME, T, F, F, IERR)
         IF (IERR.EQ.0) GO TO 50
            WRITE (MSGTXT,1020) IERR
            GO TO 990
C                                       Initialize
 50   CALL UVINIT ('WRIT', LUN, FIND, NWORD, VO, LRECL, LENBU, BUFSZ,
     *   BUFFER, BO, BIND, IERR)
      IF (IERR.EQ.0) GO TO 60
         WRITE (MSGTXT,1050) IERR
         GO TO 990
C                                       Loop thru AP.
 60   NLOOP = (NWORD - 1) / LENBU + 1
      XSIZE = NWORD
      IAPLOC = 0
      IAPLEN = LENBU
      DO 70 ILOOP = 1,NLOOP
C                                       Get data from AP.
         XTEMP = XSIZE - (ILOOP - 1.0) * LENBU
         NIO = MIN (XTEMP, 32000.0D0)
         NIO = MIN (NIO, LENBU)
         IAPLEN = NIO
         CALL QGET (BUFFER(BIND), IAPLOC, IAPLEN, 2)
         IAPLOC = IAPLOC + IAPLEN
C                                       Write to scratch file.
         CALL QWD
         CALL UVDISK ('WRIT', LUN, FIND, BUFFER, NIO, BIND, IERR)
         IF (IERR.EQ.0) GO TO 70
            WRITE (MSGTXT,1060) 'WRIT', IERR
            GO TO 990
 70      CONTINUE
C                                       Flush buffer
      NIO = 0
      CALL UVDISK ('FLSH', LUN, FIND, BUFFER, NIO, BIND, IERR)
      IF (IERR.EQ.0) GO TO 100
         WRITE (MSGTXT,1060) 'FLSH', IERR
         GO TO 990
C                                       Release AP
 100  NSAVE = PSAPNW
      CALL QRLSE
      IRET = 0
C                                       Task delay.
      CALL ZDELAY (DELAY, IERR)
C                                       Grab AP
      CALL QINIT (NSAVE, 0, IAPLOC)
      IF (NWORD.LE.0) GO TO 999
C                                       Write it all back.
      IRET = 2
      CALL UVINIT ('READ', LUN, FIND, NWORD, VO, LRECL, LENBU, BUFSZ,
     *   BUFFER, BO, BIND, IERR)
      IF (IERR.EQ.0) GO TO 110
         WRITE (MSGTXT,1050) IERR
         GO TO 990
C                                       Loop thru AP load.
 110  IAPLOC = 0
      DO 130 ILOOP = 1, NLOOP
C                                       Read data.
         CALL UVDISK ('READ', LUN, FIND, BUFFER, NIO, BIND, IERR)
         IF (IERR.EQ.0) GO TO 120
            WRITE (MSGTXT,1060) 'READ', IERR
            GO TO 990
C                                       Stuff into AP
 120     IAPLEN = NIO
         CALL QPUT (BUFFER(BIND), IAPLOC, IAPLEN, 2)
         IAPLOC = IAPLOC + IAPLEN
         CALL QWD
 130     CONTINUE
C                                       Delete scratch file
      CALL ZCLOSE (LUN, FIND, IERR)
      J = 256  + 1
      CALL COPY (256, CATBLK, BUFFER(1))
      I = 1
      CALL MAPCLR (I, SCRVOL(NSCR), SCRCNO(NSCR), 2, BUFFER(J))
      NSCR = NSCR - 1
      CALL COPY (256, BUFFER(1), CATBLK)
C                                       Finished
      IRET = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('APROLL: TOO MANY SCRATCH FILES =',I5)
 1010 FORMAT ('APROLL: CANNOT CREATE SCRATCH FILE TO ROLL AP, IERR=',I5)
 1020 FORMAT ('APROLL: CANNOT OPEN SCRATCH FILE TO ROLL AP, IERR=',I5)
 1050 FORMAT ('APROLL: CANNOT INIT SCRATCH FILE TO ROLL AP, IERR=',I5)
 1060 FORMAT ('APROLL: FAILED ',A4,'ING SCRATCH FILE, IERR=',I5)
      END
