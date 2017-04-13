      SUBROUTINE NXCHCK (DISK, CNO, BUFF2)
C-----------------------------------------------------------------------
C! Check that NX table matches number of vis, otherwise re-do table
C# FITS EXT-util UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2015
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
C   NXCHCK (re)writes the NX table correcting the visibility pointers
C   Input:
C      DISK   I   Disk number
C      CNO    I   Catalog number
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO
      REAL      BUFF2(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   BUFFER(512), LUN, IND, LUNTMP, INXRNO, NXKOLS(MAXNXC),
     *   NXNUMV(MAXNXC), IERR, IDSOUR, SUBARR, VS, VE, FREQID, RNXRET,
     *   VO, BO, IBIND, JBUFSZ, IPTRI, INIO, I, VER, ILENBU, MSGSAV
      REAL      TIME, DTIME
      LOGICAL   T, F
      CHARACTER PHNAME*48
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DRNX.INC'
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      CALL FNDEXT ('NX', CATBLK, VER)
      CALL UVPGET (IERR)
C                                       If NX table, check match of #s
      IF (VER.GT.0) THEN
         LUN = LUNTMP (1)
         CALL NDXINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *      INXRNO, NXKOLS, NXNUMV, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPENING NX TABLE TO CHECK'
            GO TO 990
            END IF
         INXRNO = BUFFER(5)
         CALL TABNDX ('READ', BUFFER, INXRNO, NXKOLS, NXNUMV, TIME,
     *      DTIME, IDSOUR, SUBARR, VS, VE, FREQID, IERR)
         CALL TABNDX ('CLOS', BUFFER, INXRNO, NXKOLS, NXNUMV, TIME,
     *      DTIME, IDSOUR, SUBARR, VSTART, VEND, FREQID, I)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING NX TABLE TO CHECK'
            GO TO 990
            END IF
         IF (VE.EQ.CATBLK(KIGCN)) THEN
            MSGTXT = 'Index table matches visibility count, assuming' //
     *         ' it is good'
            CALL MSGWRT (4)
            GO TO 999
            END IF
         END IF
C                                       have to do revised NX table
      CALL RNXGET (DISK, CNO, CATBLK)
      MSGSAV = MSGSUP
      IF (VER.GT.0) MSGSUP = 32000
      CALL RNXINI (DISK, CNO, CATBLK, RNXRET)
      MSGSUP = MSGSAV
      IF (RNXRET.GT.0) THEN
         WRITE (MSGTXT,1000) RNXRET, 'INIT INDEX TABLE (RE)WRITE'
         GO TO 990
         END IF
C                                       force file re-write
      RNXRNO = 1
      IF (VER.GT.0) THEN
         MSGTXT = 'NXCHCK: Rebuilding index table'
      ELSE
         MSGTXT = 'NXCHCK: Building new index table'
         END IF
      CALL MSGWRT (2)
      LUN = LUNTMP (0)
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISK, CNO, 1, PHNAME, IERR)
      CALL ZOPEN (LUN, IND, DISK, PHNAME, T, F, F, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN OUTPUT UV DATA SET'
         GO TO 990
         END IF
C                                       Init vis file for read.
      ILENBU = 0
      JBUFSZ = UVBFSL * 2
      CALL UVINIT ('READ', LUN, IND, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF2, BO, IBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT READ FROM OUTPUT DATA SET'
         GO TO 990
         END IF
C                                       Loop
      INIO = 1
 100  IF (INIO.GT.0) THEN
C                                       Read vis. record.
         CALL UVDISK ('READ', LUN, IND, BUFF2, INIO, IBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ OUTPUT DATA SET'
            GO TO 990
            END IF
         IPTRI = IBIND
         DO 110 I = 1,INIO
C                                       update NX table
            CALL RNXUPD (BUFF2(IPTRI), RNXRET)
            IPTRI = IPTRI + LREC
 110        CONTINUE
C                                       Read next buffer.
         GO TO 100
         END IF
C                                       Close files
      CALL ZCLOSE (LUN, IND, IERR)
C                                       close NX table
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'NXCHCK: OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
      GO TO 999
C                                       error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NXCHCK: ERROR',I4,' ON ',A)
      END
