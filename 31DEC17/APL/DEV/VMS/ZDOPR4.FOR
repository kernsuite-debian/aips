      SUBROUTINE ZDOPR4 (IVOL, IBMLUN, NCOPY, FILNAM, ISIZE, INBLK,
     *   IERR)
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
C   ZDOPRT will read a bit map such as produced by PRTDRW and convert
C   it into a FORTRAN file that can be spooled to the PRINTRONIX
C   printer-plotter as a plot.  The number of pixels per line for the
C   PRINTRONIX is set to 768 (128*6, 96*8, or 48 I).    This version
C   spools the plot to the device queue with the logical name PLOTTER.
C     INPUTS:
C        IVOL      I   volume no. of bit map disk (1 rel)
C        IBMLUN    I   bit map logical unit number.
C        NCOPY     I   Number of copies of the plot to make.
C        FILNAM    I(12)   physical file name of bit map.
C        ISIZE     I   size of INBLK in words
C      IN/Out:
C        INBLK     I(*)    scratch buffer
C     OUTPUTS:
C        IERR      I   error return code.
C                    0 - good.
C                   >0 - an error occurred.
C   VAX VERSION: works under VMS for PRINTRONIX as a regular printer
C       831107   JHS
C-----------------------------------------------------------------------
      IMPLICIT   NONE
      DOUBLE PRECISION
     *       RBYTES
      REAL       READ, DELAY1, DELAY3
      INTEGER    FORERR, RMSERR, QPRINT, IER
      INTEGER    I4ERR, SYS$CRELOG
      INTEGER    MSGLEN
      INTEGER    INBLK(1), IOBLK(66), ISIZE, FILNAM(12)
      INTEGER    IHURT(12)
      INTEGER    ILPLUN, ISIZE2, NCOPY, IERR
      INTEGER    ILEN, INPOS, IOPOS, IBMLUN, IBMIND, IVOL, INIBLK
      INTEGER    IRRN(2), FF, IMLINE, ILINE, NBYTES
      INTEGER    I, II
      INTEGER    N1, N6, N7, N48
      LOGICAL    EXCL, NOMAP, WAIT, MAP
      BYTE       PLOTBYT(24), LPBLK(128), SUFFIX(3)
      CHARACTER*64 MESSAGE
      CHARACTER*9 PRINTQ
      CHARACTER*7 PLOTER, DEVICQ
      CHARACTER  PLTFIL*13, PLOTDSN*24
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (EXCL, WAIT, MAP)
C  Note that character variable PLOTDSN is set through its equvalence
C  with IHURT which is set to FILNAM
      EQUIVALENCE (IHURT(1),PLOTBYT(1)),(PLOTBYT(1),PLOTDSN)
      DATA READ /'READ'/
      DATA FF/'000C'X/
      DATA ILPLUN/1/,       N1, N6, N7, N48 /1, 6, 7, 48/
      DATA SUFFIX / '05'X, '0D'X, '0A'X /
      DATA EXCL /.TRUE./,         NOMAP /.FALSE./
      DATA PLOTER /'PLOTTER'/,    PRINTQ /'SYS$PRINT'/
      DATA DELAY1, DELAY3 /1.0, 3.0/
C-----------------------------------------------------------------------
C                                       Open the bit map file.
      CALL ZOPEN (IBMLUN, IBMIND, IVOL, FILNAM, MAP, EXCL, WAIT, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (N7)
         GO TO 999
C                                       Find out Queue name for plotter.
 10   CONTINUE
      CALL ZTRLOG (PLOTER, DEVICQ, ILEN, IERR)
      IF (IERR.EQ.0) GO TO 12
         WRITE (MSGTXT,1010)
         CALL MSGWRT (N6)
         GO TO 995
 12   CONTINUE
C                                       Get the Bit file name into
C                                       characters for others to use
      DO I=1,12
          IHURT(I) = FILNAM(I)          !Note the equivalence to PLOTDSN
      ENDDO
C                                       Assign SYS$PRINT to plotter
C                                       device queue.
      I4ERR = SYS$CRELOG (%VAL(2), PRINTQ, DEVICQ(:ILEN),)
      IF (I4ERR.EQ.1) GO TO 15
         WRITE (MSGTXT,1012) I4ERR
         CALL MSGWRT (N6)
         GO TO 995
 15   CONTINUE
C                                       Initial buffer counters.
      INPOS = 257
      IOPOS = 1
      IRRN(1) = 1
      IRRN(2) = 0
      NBYTES = 2 * ISIZE                !Number of bytes in input buffer
      ILINE = 0
      ISIZE2 = ISIZE                    !Size of input buffer (words)
C     IBMLUN - Logical unit number for read
C     IBMIND - Pointer to FTAB returned by ZOPEN (always even number)
C     IRRN(1)- Block number 'One relative beginning block number.'
C     NBYTES - Number of bytes to transfer
C     INBLK  - The i/o buffer
C     N1     - One signifies use one buffer i/o
C     IERR   - Error return code (0-> success)
      CALL ZMI3 (READ, IBMLUN, IBMIND, IRRN, NBYTES, INBLK, N1, IERR)
      IF (IERR.EQ.0) CALL ZWAIT (IBMLUN, IBMIND, N1, IERR)
      IF (IERR.NE.0) GO TO 920
      IRRN(1) = IRRN(1) + NBYTES/NBPS   !Number of records read in
      IMLINE = INBLK(1)                 !Number of lines in picture
C
C   It appears that RBYTES is the number of bytes in the picture
C   remaining to be processed.  I am going to change the coefficient on
C   IMLINE from 264 (2112/8) to reflect the PRINTRONIX
C                                My guess is 768/8 = 96      JHS 831018
      RBYTES = 512.0D0 + 96.0D0 * IMLINE - NBYTES
C                                Open the PRINTRONIX (spool file)
C                                Estimate an initial size for PLOT.DAT
      INIBLK = .58 * IMLINE
C                                       Try for each available disk.
      DO 50 I = 1,NVOL
         WRITE (PLTFIL, '(''DA0'',I1,'':PLOT.DAT'')' ) I - 1
         OPEN (UNIT=ILPLUN, FILE=PLTFIL, STATUS='NEW',
     *      ACCESS='SEQUENTIAL', FORM='FORMATTED',
     *      CARRIAGECONTROL='NONE', DISP='PRINT',
     *      RECORDTYPE='VARIABLE', INITIALSIZE=INIBLK, IOSTAT=IERR)
         IF (IERR.EQ.0) GO TO 100
            WRITE (MSGTXT,1015) IERR, I
            CALL MSGWRT (N7)
 50      CONTINUE
      GO TO 990
C
C-----------------------------------------------------------------------
C
C                                       Begin master loop.
 100  CONTINUE
C                                       See if we need to read new buff
         IF (INPOS.LE.ISIZE2) GO TO 110
            IF (RBYTES.LT.0.01) GO TO 800
            IF (RBYTES.GE.NBYTES) GO TO 105
C                                       Last input buffer.
               NBYTES = RBYTES + 0.1
               ISIZE2 = NBYTES / 2
 105        CONTINUE
            CALL ZMI3 (READ, IBMLUN, IBMIND, IRRN, NBYTES, INBLK, N1,
     *         IERR)
            IF (IERR.EQ.0) CALL ZWAIT (IBMLUN, IBMIND, N1, IERR)
            IF (IERR.NE.0) GO TO 920
            RBYTES = RBYTES - NBYTES
C       NBPS is appearantly Number of Bytes Per Something (buffer?)
C           When I ran DEBUG NBPS was 512 (probably set from common)
            IRRN(1) = IRRN(1) + NBYTES/NBPS
            INPOS = 1
C                                       Determine length to copy.
 110     ILEN = MIN (ISIZE2+1-INPOS, 49-IOPOS)
C                                       Try it without a swap
        DO I=0, ILEN - 1
           IOBLK(IOPOS+I) = INBLK(INPOS+I)
        ENDDO
C                                       Update position counters (words)
        IOPOS = IOPOS + ILEN
        INPOS = INPOS + ILEN
C
C                                       Write to PRINTRONIX spool file.
        IF (IOPOS.LE.48) GO TO 100
C                                       Convert to PRINTRONIX char
         CALL GETCH (N48, IOBLK, IOPOS, LPBLK, IERR)
C
         IF (IOPOS.GT.128) THEN
            IOPOS = 128
         ENDIF
         WRITE (ILPLUN,1110,ERR=940) (LPBLK(I), I=1,IOPOS) , SUFFIX
         ILINE = ILINE + 1
         IOPOS = 1
        GO TO 100
C                                       Flush last buffer.
 800  CONTINUE
      IOPOS = IOPOS - 1
      IF (IOPOS.LE.0) GO TO 990
C
         CALL GETCH(IOPOS,IOBLK,IOPOS,LPBLK,IERR)
C
         WRITE (ILPLUN,1110,ERR=940)    (LPBLK(I), I=1,IOPOS) , SUFFIX
         GO TO 990
C                                       Error handling.
 920  WRITE (MSGTXT,1920) IERR
      CALL MSGWRT (N7)
      GO TO 990
 940  CONTINUE
      CALL ERRSNS (FORERR, RMSERR)
      CALL SYS$GETMSG (%VAL(RMSERR), MSGLEN, MESSAGE, %VAL(1), )
      WRITE (6, '(1X,A64)' ) MESSAGE
      WRITE (MSGTXT,1940)
      CALL MSGWRT (N7)
C                                       Issue final form feed and
C                                       close files.
 990  WRITE (ILPLUN,1020,ERR=995) FF
      NCOPY = NCOPY - 1
      IF (NCOPY.LE.0) GO TO 995
         DO 993 I = 1, NCOPY
            CLOSE (UNIT=ILPLUN)
C  Note that character variable PLOTDSN is set through its equvalence
C  with FILNAM
C            IER = QPRINT(PLOTDSN)
C            IF (IER .NE. 0) THEN
C                ENCODE (80, 1995, MSGTXT) IER
C                CALL MSGWRT (N6)
C            ENDIF
C                                       Give spooler chance to get file.
            CALL ZDELAY (DELAY3, IERR)
C                                       Try a few times to open file.
            DO 992 II = 1,10
               CALL ZDELAY (DELAY1, IERR)
               OPEN (UNIT=ILPLUN, FILE=PLTFIL, STATUS='OLD',
     *            ACCESS='SEQUENTIAL', FORM='FORMATTED',
     *            CARRIAGECONTROL='NONE', DISP='PRINT',
     *            RECORDTYPE='VARIABLE', IOSTAT=IERR)
               IF (IERR.EQ.0) GO TO 993
 992           CONTINUE
            WRITE (MSGTXT,1992)
            CALL MSGWRT (N6)
 993        CONTINUE
 995  CLOSE (UNIT=ILPLUN, DISP='PRINT/DELETE')
C  Note that character variable PLOTDSN is set through its equvalence
C  with FILNAM
C      IER = QPRINT(PLOTDSN)
C      IF (IER .NE. 0) THEN
C         ENCODE (80, 1995, MSGTXT) IER
C         CALL MSGWRT (N6)
C      ENDIF
      CALL ZCLOSE (IBMLUN,IBMIND,IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZDOPRT: UNABLE TO OPEN BIT MAP FILE. IERR=',I4)
 1010 FORMAT ('ZDOPRT: NO LOGICAL NAME "PLOTTER".')
 1012 FORMAT ('ZDOPRT: COULD NOT ASSIGN LOGICAL NAME, IERR=',Z8)
 1015 FORMAT ('ZDOPRT: ERR ',I4,' OPENING SPOOL FILE ON DISK',I3,
     *  '. TRYING NEXT DISK')
 1020 FORMAT (A2)
 1110 FORMAT (128A1,3A1)
 1920 FORMAT ('ZDOPRT: ZFI3 READ ERROR. IERR=',I4)
 1940 FORMAT ('ZDOPRT: FORTRAN WRITE ERROR TO PLOTTER')
 1992 FORMAT ('ZDOPRT: COULD NOT REOPEN FILE FOR NEXT COPY')
 1995 FORMAT ('ZDOPRT: ERROR SENDING FILE TO PRTSYMB1. IERR=',I4)
      END
        SUBROUTINE GETCH(NIN,IN,NOUT,OUT,IERR)
C-----------------------------------------------------------------------
C       This routine takes the packed input byte stream and turns it
C       into a six character stream suitable for the PRINTRONIX P600.
C     INPUTS:
C        NIN       I    number of input words
C        IN(*)     I    packed bit stream
C     OUTPUTS:
C        NOUT      I    number of output characters (should be 8/3 NIN)
C        OUT(*)    BYTE characters suitable for the P600
C        IERR      I   error return code.
C                    0 - good.
C                   >0 - an error occurred.
C   VAX VERSION: works under VMS for PRINTRONIX as a regular printer
C       831103   JHS
C-----------------------------------------------------------------------
        IMPLICIT NONE
        INTEGER   IN(*), NIN, NOUT, IERR, IOUT, II, IO, IT, J, JJ, K
        BYTE OUT(*), NULL
        DATA NULL / 'C0'X /
C
        IOUT = 0                !Clear temp for output
        K = 0                   !Bit counter for out temp
        IO = 0          !Index into OUT
C
        DO II = 1, NIN
           IT = IN(II)
           DO JJ = 1, 16
              IT = ISHFTC(IT,1,16)      !Get MSB into LSB to use
              J = IT .AND. 1            !Reduce to 1 bit of interest
              IOUT = IOUT .OR. J        !Put this bit into output
              IOUT = ISHFTC(IOUT,-1,6)
              K = K + 1                 !Done 6 bits yet?
              IF (K .GE. 6) THEN        !Then store it.
                 IO = IO + 1            !Get the next byte index
                 OUT(IO) = IOUT + NULL  !Strobe in the graphics flag bit
                 IOUT = 0               !Reset for next character
                 K = 0
              ENDIF
           ENDDO
        ENDDO
        NOUT = IO
        IERR = 0
        RETURN
        END
