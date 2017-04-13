      SUBROUTINE BBUILD (IW, IL, IP, IM, BUFFER)
C-----------------------------------------------------------------------
C! reads input lines and adds them to the text file for a batch job
C# Batch
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
C   BBUILD takes input lines via PREAD and puts them into the batch
C   file which must be open and refenced via the BWTCH common.
C   Process stops when 'ENDBATCH' is found in the data stream.  In
C   addition, if IUNIT = 3 (batch input to interactive), cards with
C   the word RUN are eliminated.
C   Inputs:
C      IW      I       logical rec # - where to put first line
C      IP      I       log rec # of line which points to first line
C      IL      I       log rec # to which last line must point
C      IM      I       initial next avail log. rec #
C   In/out:
C      BUFFER  I(256)  buffer used to write the file.  On input it
C                      contains the log rec pointed at by BWTREC.
C-----------------------------------------------------------------------
      INTEGER   IW, IL, IP, IM, BUFFER(256)
C
      CHARACTER   BATSGN*1, KARAT*1, ENDBTS(2)*8, RUNLTS(2)*4,
     *   PRGNAM*6
      INTEGER   POTERR, IWH, ICOUNT, NXT, LREC, IREC, IERR, NWPL, NLPR,
     *   INCS, LL, J, LASTLR
      INCLUDE 'INCS:DBWT.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      DATA BATSGN, KARAT /'<', '>'/
      DATA ENDBTS /'ENDBATCH','endbatch'/
      DATA RUNLTS /'RUN ','run '/
      DATA PRGNAM /'BBUILD'/
C-----------------------------------------------------------------------
C                                        init things
      POTERR = 49
      ICOUNT = 0
      IWH = IW
      NXT = IM
      IF (IM.EQ.IW) NXT = NXT + 1
      IPT = BATSGN
      NWPL = 2 + (NCHLIN+3)/4
      NLPR = 252 / NWPL
C                                        make rec. referencing first
C                                        be in core
      IF (BWTREC.EQ.IP) GO TO 25
         LREC = (BWTREC-1)/NLPR + 1
         BWTREC = IP
         IREC = (BWTREC-1)/NLPR + 1
         IF (IREC.EQ.LREC) GO TO 25
            CALL ZFIO ('READ', BWTLUN, BWTIND, IREC, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 980
C                                        read next line
 25   CALL PREAD (KARBUF)
         IF (ERRNUM.NE.0) GO TO 980
C                                        look for special strings
         LL = INDEX (KARBUF, ENDBTS(1))
         IF (LL.GT.0) GO TO 50
         LL = INDEX (KARBUF, ENDBTS(2))
         IF (LL.GT.0) GO TO 50
         IF (IUNIT.NE.3) GO TO 30
            LL = INDEX (KARBUF, RUNLTS(1))
            IF (LL.GT.0) GO TO 25
            LL = INDEX (KARBUF, RUNLTS(2))
            IF (LL.GT.0) GO TO 25
C                                        point last to this
 30      J = 5 + NWPL * MOD(BWTREC-1, NLPR)
         BUFFER(J) = IWH
         LREC = (BWTREC-1)/NLPR + 1
         LASTLR = BWTREC
C                                        on to this one
         BWTREC = IWH
         IWH = NXT
         NXT = NXT + 1
         IREC = (BWTREC-1)/NLPR + 1
         IF (IREC.EQ.LREC) GO TO 35
            CALL ZFIO ('WRIT', BWTLUN, BWTIND, LREC, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 980
            IF (IREC.LE.BWTNUM) GO TO 32
               INCS = 20/NLPR + 1
               CALL ZEXPND (BWTLUN, 1, BWTNAM, INCS, IERR)
               IF (IERR.NE.0) GO TO 980
               BWTNUM = BWTNUM + INCS
 32         CALL ZFIO ('READ', BWTLUN, BWTIND, IREC, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 980
 35      J = 5 + NWPL * MOD(BWTREC-1, NLPR)
         BUFFER(J) = IL
         BUFFER(J+1) = LASTLR
         CALL CHR2H (NCHLIN, JBUFF, 1, BUFFER(J+2))
         ICOUNT = ICOUNT + 1
         GO TO 25
C                                        finish up:
 50   IF (ICOUNT.LE.0) GO TO 70
         LASTLR = BWTREC
         LREC = (BWTREC-1)/NLPR + 1
C                                        update back link in old code
         IF (IL.LE.0) GO TO 60
            BWTREC = IL
            IREC = (BWTREC-1)/NLPR + 1
            IF (IREC.EQ.LREC) GO TO 55
               CALL ZFIO ('WRIT', BWTLUN, BWTIND, LREC, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 980
               LREC = IREC
               CALL ZFIO ('READ', BWTLUN, BWTIND, LREC, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 980
 55         J = 5 + NWPL * MOD(BWTREC-1, NLPR)
            BUFFER(J+1) = LASTLR
C                                        update full file pointers
 60      IF (LREC.EQ.1) GO TO 65
            CALL ZFIO ('WRIT', BWTLUN, BWTIND, LREC, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL ZFIO ('READ', BWTLUN, BWTIND, 1, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 980
 65      BUFFER(2) = IWH
         IF (IL.LE.0) BUFFER(3) = BWTREC
         BUFFER(4) = BWTNUM
         CALL ZFIO ('WRIT', BWTLUN, BWTIND, 1, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 980
C                                        message
 70   WRITE (MSGTXT,1070) ICOUNT
      CALL MSGWRT (2)
      GO TO 990
C                                        Errors
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 990  IPT = KARAT
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('Added',I6,' lines to batch work file')
      END
