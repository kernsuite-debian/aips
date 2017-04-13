      SUBROUTINE UVTIME (IVOL, ISLOT, CATBLK, TSTART, TSTOP, IERR)
C-----------------------------------------------------------------------
C! Find the start and stop times of a TB-sorted uv data set.
C# UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003, 2015
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
C   Input:
C      IVOL     I        Volume number on which data resides
C      ISLOT    I        Catalogue number of data
C      CATBLK   I(256)   Catalogue header
C   Output:
C      TSTART   R        Start time (days)
C      TSTOP    R        Stop time in days
C      IERR     I        Return code, 0=>OK, otherwise INDEX file
C                        exists but cannot be read.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      REAL      TSTART, TSTOP
      INTEGER   IVOL, ISLOT, CATBLK(256), IERR
      LOGICAL   NXEXST
C
      INTEGER   JERR, NXIDSO, NXSUBA, NIO, IUBIND, NXSTA, IULUN,
     *   NXEND, NREAD, BO, FRSRED, FREQID, INXRNO, FSTVIS, LSTVIS,
     *   IXLUN, NXBUFF(1024), NXKOLS(MAXNXC), NXNUMV(MAXNXC), NINDEX,
     *   UBUFSZ, IUFIND, LRECIN, CATTMP(256)
      LOGICAL   T, F, TABLE, EXIST, FITASC
      REAL      NXTIME, NXDTIM, UBUFF(UVBFSS)
      CHARACTER UFILE*48, CSORT*2
      HOLLERITH CATH(256)
      SAVE UBUFF
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATTMP, CATH)
      DATA T, F /.TRUE.,.FALSE./
      DATA BO /1/
C-----------------------------------------------------------------------
      NXEXST = F
      UBUFSZ = UVBFSS * 2
      CALL COPY (256, CATBLK, CATTMP)
C                                       Initialize output
      INXRNO = -1
      FSTVIS = 1
      LSTVIS = CATBLK(KIGCN)
      IXLUN = 30
C                                       See if NX file exists.
      CALL ISTAB ('NX', IVOL, ISLOT, 1, IXLUN, NXBUFF, TABLE, EXIST,
     *   FITASC, JERR)
      IF ((JERR.NE.0) .OR. (.NOT.TABLE) .OR. (.NOT.EXIST)) GO TO 300
C                                       Open NX table
      CALL NDXINI ('READ', NXBUFF, IVOL, ISLOT, 1, CATBLK, IXLUN,
     *   INXRNO, NXKOLS, NXNUMV, JERR)
      IF (JERR.GT.0) THEN
         IERR = 1
         WRITE (MSGTXT,1000) JERR
         GO TO 990
         END IF
C                                       Get number of scans
      NINDEX = NXBUFF(5)
C                                       Check if empty
      IF (NINDEX.LE.0) GO TO 300
C                                       Locate first selected scan.
      NXEXST = T
      INXRNO = 1
      IERR = 1
      CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV,
     *   NXTIME, NXDTIM, NXIDSO, NXSUBA, NXSTA, NXEND, FREQID,
     *   IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      IERR = 0
C                                       Return start time
      TSTART = NXTIME - 0.5 * NXDTIM
C                                       Get last included one
      INXRNO = NINDEX
      IERR = 1
      CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV,
     *   NXTIME, NXDTIM, NXIDSO, NXSUBA, NXSTA, NXEND, FREQID,
     *   IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      IERR = 0
C
      TSTOP = NXTIME + 0.5 * NXDTIM
      CALL TABIO ('CLOS', 0, INXRNO, UBUFF, NXBUFF, IERR)
      GO TO 999
C                                       No INDEX file
C                                       Check sort order
 300  CALL H2CHR (2, 1, CATH(KITYP), CSORT)
      IF (CSORT(1:1).NE.'T') THEN
         IERR = 1
         WRITE (MSGTXT,1080)
         GO TO 990
         END IF
C                                       Open uv file
      IULUN = 45
      CALL ZPHFIL ('UV', IVOL, ISLOT, 1, UFILE, IERR)
      CALL ZOPEN (IULUN, IUFIND, IVOL, UFILE, T, F, T, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1800) 'OPEN', IERR
         GO TO 990
         END IF
C                                       Init I/O to uvfile
      NREAD = 1
      LRECIN = LREC
C                                       Read first vis.
      FRSRED = FSTVIS - 1
      CALL UVINIT ('READ', IULUN, IUFIND, NREAD, FRSRED, LRECIN, 1,
     *   UBUFSZ, UBUFF, BO, IUBIND, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1810) 'INIT FIRST', IERR
         GO TO 990
         END IF
      CALL UVDISK ('READ', IULUN, IUFIND, UBUFF, NIO, IUBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1810) 'READ FIRST', IERR
         GO TO 990
         END IF
      TSTART = UBUFF(IUBIND+ILOCT)
C                                       Read last vis.
      FRSRED = LSTVIS - 1
      CALL UVINIT ('READ', IULUN, IUFIND, NREAD, FRSRED, LRECIN, 1,
     *   UBUFSZ, UBUFF, BO, IUBIND, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1810) 'INIT LAST', IERR
         GO TO 990
         END IF
      CALL UVDISK ('READ', IULUN, IUFIND, UBUFF, NIO, IUBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1810) 'READ LAST', IERR
         GO TO 990
         END IF
      TSTOP = UBUFF(IUBIND+ILOCT)
      CALL ZCLOSE (IULUN, IUFIND, JERR)
      GO TO 999
C                                       Errors
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVTIME: ERROR',I3,' OPENING INDEX TABLE')
 1020 FORMAT ('UVTIME: ERROR',I3,' READING INDEX TABLE')
 1080 FORMAT ('The data are not TB sorted. Use UVSRT to sort data')
 1800 FORMAT ('UVTIME: UNABLE TO ',A,' INPUT UV FILE, ERROR',I5)
 1810 FORMAT ('UVTIME: UNABLE TO ',A,' INPUT VISIBILITY RECORD, ERROR',
     *   I5)
      END
