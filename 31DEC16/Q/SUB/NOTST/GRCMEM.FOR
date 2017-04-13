      SUBROUTINE GRCMEM (IFIELD, DOGCOR, GRID, SCALE, DISKO, CNOSCO,
     *   MAPMAX, MAPMIN, JBUFSZ, BUFF1, IRET)
C-----------------------------------------------------------------------
C! Normalizes and corrects image for gridding convolution fn in memory
C# AP-util Map Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2008
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
C   GRCMEM normalizes and corrects for the gridding convolution in
C   memory.  Called by IMGMEM.  See IMGMEM for details.
C   Uses AIPS LUNs 18
C    Input:
C      IFIELD      I     The subfield number
C                        If IFIELD < 0 the input is assumed to be
C                        beam -IFIELD.  (0 -> -1)
C      GRID        I     "AP" base address of data grid.
C      SCALE       R     Beam maximum for this field
C      DOGCOR      L     If TRUE, do gridding convolution correction.
C      DISKO       I     Output file disk number for catalogd files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCO      I     Output file catalog slot number or /CFILES/
C                        scratch file number.
C      JBUFSZ      I     Size in bytes of buffers. Dimension of
C                        BUFF1,2,3  must be at least 4096 words.
C    Output:
C      MAPMAX      R     The maximum value in the resultant image.
C      MAPMIN      R     The minimum value in the resultant image.
C      BUFF1       R     Working buffer
C      IRET        I     Return error code. 0=>OK, error otherwise.
C-----------------------------------------------------------------------
      INTEGER   IFIELD, GRID, DISKO, CNOSCO, JBUFSZ, IRET
      LOGICAL   DOGCOR
      REAL      SCALE, MAPMAX, MAPMIN, BUFF1(*)
C
      CHARACTER FILE*48
      INTEGER   VOL, I, LIMIT, NX, NY, JWIN(4), FIND2, BIND2, IXCEN,
     *   IYCEN, BOOUT, LUNO, IMAP, IXC, IYC, IWORK, IAPSIZ, ONENX,
     *   ONENY, YCORR, IAPMIN, IAPMAX, LFIELD
      LOGICAL   MAP, EXCL, WAIT
      REAL      BIMAX, TEMP(5)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA MAP, EXCL, WAIT /.TRUE.,.FALSE.,.TRUE./
      DATA LUNO /18/
C-----------------------------------------------------------------------
C                                       Init. max, min
      MAPMAX = -1.0E20
      MAPMIN =  1.0E20
C                                       Set image size, center pixel
      IF (IFIELD.LE.0) THEN
         LFIELD = MAX (1, -IFIELD)
         NX = NXBEM(LFIELD)
         NY = NYBEM(LFIELD)
         IXCEN = NX / 2
         IYCEN = NY / 2 + 1
         BOOUT = BOBEM(LFIELD)
      ELSE
         LFIELD = IFIELD
         NX = FLDSZ(1,IFIELD)
         NY = FLDSZ(2,IFIELD)
         IXCEN = ICNTRX(IFIELD)
         IYCEN = ICNTRY(IFIELD)
         BOOUT = BORES(IFIELD)
         END IF
      IF (BOOUT.LE.0) BOOUT = 1
C                                       Initialize AP addresses.
      ONENX = NX
      ONENY = NY
      IMAP = 0
      IXC = IMAP + ONENX
      IYC = IXC + ONENX
      IWORK = IYC + ONENY
      IAPMIN = IYC + ONENY
      IAPMAX = IAPMIN + 2
      IAPSIZ = IAPMAX + 2
C                                       Make sure GRID > IAPSIZ
      IF (GRID.LE.IAPSIZ) THEN
         MSGTXT = 'GCRMEM: CONFLICTING MEMORY USAGE'
         IRET = 4
         GO TO 990
         END IF
C                                       Compute Fourier Transform of
C                                       the gridding function in the AP.
C                                       Ones fill convolving fn.
C                                       correction
      TEMP(1) = 1.0
      CALL QPUT (TEMP, IMAP, 1, 2)
      CALL QWD
      CALL QVFILL (IMAP, IXC, 1, ONENX)
      CALL QVFILL (IMAP, IYC, 1, ONENY)
C                                       Convolving cor. fn. if req.
      IF (DOGCOR) THEN
         CALL GRDTAB (NX, IXCEN, CTYPX, XPARM, IXC, IWORK, BUFF1)
         CALL GRDTAB (NY, IYCEN, CTYPY, YPARM, IYC, IWORK, BUFF1)
         END IF
C                                      Open output file (write).
      IF (DISKO.GT.0) THEN
         VOL = DISKO
         CALL ZPHFIL ('MA', VOL, CNOSCO, 1, FILE, IRET)
      ELSE
         VOL = SCRVOL(CNOSCO)
         CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCO), 1, FILE, IRET)
         END IF
      CALL ZOPEN (LUNO, FIND2, VOL, FILE, MAP, EXCL, WAIT, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Divide X gridding correction by
C                                       the peak of the beam to
C                                       normalize.
      BIMAX = 1.0
      IF (ABS (SCALE).GT.1.0E-20) BIMAX = 1.0 / SCALE
      CALL QPUT (BIMAX, IMAP, 1, 2)
      CALL QWD
      CALL QVSMUL (IXC, 1, IMAP, IXC, 1, ONENX)
C                                       Set window.
      JWIN(1) = 1
      JWIN(2) = 1
      JWIN(3) = NX
      JWIN(4) = NY
C                                      Initialize output image.
      CALL MINIT ('WRIT', LUNO, FIND2, NX, NY, JWIN, BUFF1, JBUFSZ,
     *   BOOUT, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop thru map correcting for
C                                       the convolution function and
C                                       finding max. and min.
      IMAP = GRID
      LIMIT = NY
      DO 100 I = 1,LIMIT
C                                      Write row.
         CALL MDISK ('WRIT', LUNO, FIND2, BUFF1, BIND2, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1070) IRET,I
            GO TO 990
            END IF
         YCORR = IYC + I - 1
         CALL QWD
C                                       Correct map for gridding and
C                                       normalize. X correct.
         CALL QVMUL (IMAP, 1, IXC, 1, IMAP, 1, ONENX)
C                                       Y correction.
         CALL QVSMUL (IMAP, 1, YCORR, IMAP, 1, ONENX)
C                                       Read corrected residual.
C                                       Find max and min in row.
         CALL QMAXV (IMAP, 1, IAPMAX, ONENX)
         CALL QMINV (IMAP, 1, IAPMIN, ONENX)
C                                       Read results.
         CALL QWR
         CALL QGET (BUFF1(BIND2), IMAP, ONENX, 2)
         CALL QGET (TEMP, IAPMIN, 3, 2)
         CALL QWD
C                                       Save max and min.
         MAPMAX = MAX (MAPMAX, TEMP(3))
         MAPMIN = MIN (MAPMIN, TEMP(1))
         IMAP = IMAP + NX
 100     CONTINUE
C                                      Flush output
      CALL MDISK ('FINI', LUNO, FIND2, BUFF1, BIND2, IRET)
      IF (IRET.EQ.0) GO TO 150
         WRITE (MSGTXT,1070) IRET,I
         GO TO 990
C                                      Close residual file.
 150  CALL ZCLOSE (LUNO, FIND2, IRET)
      IF (IRET.EQ.0) GO TO 999
         WRITE (MSGTXT,1150) IRET
C                                       Error message.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GRCMEM: ERROR',I3,' OPENING FILE ')
 1010 FORMAT ('GRCMEM: ERROR',I3,' INIT FILE ')
 1020 FORMAT ('GRCMEM: READ ERROR',I3,' ROW ',I5)
 1070 FORMAT ('GRCMEM: WRITE ERROR',I3,' ROW ',I5)
 1150 FORMAT ('GRCMEM: ERROR',I3,' CLOSING FILE ')
      END
