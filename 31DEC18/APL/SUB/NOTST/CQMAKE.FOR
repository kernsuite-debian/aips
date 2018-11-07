      SUBROUTINE CQMAKE (IDISK, JCNO, CATUV, JBUF1, JBUF2, JLUN1, JLUN2,
     *   IRET)
C-----------------------------------------------------------------------
C! Create a CQ table using FQ, MC and catalog header information.
C# EXT-appl UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2007, 2012
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
C   Inputs:
C      IDISK    I        Disk volume number
C      JCNO     I        Catalog slot number of uv file
C      CATUV    I(256)   Catalog header block
C      JBUF1    I(512)   I/O buffer 1
C      JBUF2    I(512)   I/O buffer 2
C      JLUN1    I        LUN 1 to use for table I/O
C      JLUN2    I        LUN 2 to use for table I/O
C   Output:
C      IRET     I        Return code; 0=> ok
C   Output to common:
C      Uses DCAT.INC and DUVH.INC
C-----------------------------------------------------------------------
      INTEGER   IDISK, JCNO, CATUV(256), JLUN1, JLUN2, JBUF1(512),
     *   JBUF2(512), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCQV.INC'
      INTEGER   MAXKEY, MAXKY2
      PARAMETER (MAXKEY = 50, MAXKY2 = 2 * MAXKEY)
      LOGICAL   WEXIT
      CHARACTER LTYPE*2, KEYSTR(MAXKEY)*8, LTAPER*8, LTAPCQ(MAXIF)*8
      DOUBLE PRECISION DFOFF(MAXIF), DFRQCQ(MAXIF), DCBWCQ(MAXIF)
      REAL      FINC(MAXIF), BANDW(MAXIF), TAVGCQ(MAXIF)
      INTEGER   NOIFCQ, IFQRNO, FQKOLS(MAXFQC), FQNUMV(MAXFQC), NUMIF,
     *   ICQRNO, CQKOLS(MAXCQC), CQNUMV(MAXCQC), IFQID, JIF, IROW,
     *   IFQDCQ, ISUBCQ, NFFTCQ(MAXIF), NCHCQ(MAXIF), NSAVCQ(MAXIF),
     *   NOVSCQ(MAXIF), NZPDCQ(MAXIF), IFLTCQ(MAXIF), NBITCQ(MAXIF),
     *   IOVLCQ(MAXIF), IFSIDE(MAXIF), IKLOC(MAXKEY), KEYVAL(MAXKY2),
     *   KEYTYP(MAXKEY), NSUBA, IVER, NKEY, NCOL, NREC, DATP(128,2),
     *   IERR, MSGSAV, I, IPOINT, NFFT, NOVS, NZPD, NCHAN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Copy CATUV to CATBLK and
C                                       determine header pointers
      CALL COPY (256, CATUV, CATBLK)
      CALL UVPGET (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         IRET = 1
         GO TO 990
         END IF
C                                       Get no. of AN tables (subarrays)
      CALL FNDEXT ('AN', CATBLK, NSUBA)
C                                       Get keywords from the MC table
      LTYPE = 'MC'
      IVER = 1
      MSGSAV = MSGSUP
      MSGSUP = 32000
C                                       Initialization
      NFFT = -1
      NOVS = -1
      NZPD = -1
      LTAPER = ' '
C
      CALL TABINI ('READ', LTYPE, IDISK, JCNO, IVER, CATBLK, JLUN1,
     *   NKEY, NREC, NCOL, DATP, JBUF1, IERR)
C                                       Extract keywords
      IF (IERR.LE.0) THEN
         NKEY = MAXKEY
         CALL TABKEY ('ALL ', KEYSTR, NKEY, JBUF1, IKLOC, KEYVAL,
     *      KEYTYP, IERR)
         IF (IERR.EQ.0) THEN
            DO 50 I = 1, NKEY
               IPOINT = IKLOC(I)
               IF (IPOINT.LE.0) GO TO 50
               IF (KEYSTR(I).EQ.'FFT_SIZE') NFFT = KEYVAL(IPOINT)
               IF (KEYSTR(I).EQ.'OVERSAMP') NOVS = KEYVAL(IPOINT)
               IF (KEYSTR(I).EQ.'ZERO_PAD') NZPD = KEYVAL(IPOINT)
               IF (KEYSTR(I).EQ.'TAPER_FN')
     *            CALL H2CHR (8, 1, KEYVAL(IPOINT), LTAPER)
50             CONTINUE
            END IF
C                                       Close MC table
         CALL TABIO ('CLOS', 0, 0, JBUF1, JBUF1, IERR)
         END IF
      MSGSUP = MSGSAV
C                                       Get NCHAN from catalog hdr.
      IF (JLOCF.GT.0) THEN
         NCHAN = CATBLK(KINAX+JLOCF)
      ELSE
         NCHAN = 1
         END IF
C                                       Get NO_IF from catalog hdr.
      IF (JLOCIF.GT.0) THEN
         NOIFCQ = CATBLK(KINAX+JLOCIF)
      ELSE
         NOIFCQ = 1
         END IF
C                                       Can a CQ table be created ?
      WEXIT = ((NSUBA.LE.0).OR.(NFFT.LT.0).OR.(NOVS.LT.0).OR.
     *   (NZPD.LT.0).OR.(LTAPER.EQ.' '))
C                                       Exit if not possible
      IF (WEXIT) THEN
         WRITE (MSGTXT,1070)
         IRET = 2
         GO TO 990
         END IF
C                                       Open FQ table for READ
      IVER = 1
      CALL FQINI ('READ', JBUF1, IDISK, JCNO, IVER, CATBLK, JLUN1,
     *   IFQRNO, FQKOLS, FQNUMV, NUMIF, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('READ', 'FQINI', 'CQMAKE', IERR)
         IRET = 3
         GO TO 999
         END IF
C                                       Delete pre-existing CQ table
      CALL RMEXT (IDISK, JCNO, 'CQ', 1, CATBLK, JBUF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1080) IERR
         IRET = 4
         GO TO 990
         END IF
C                                       Open CQ table for WRIT
      IVER = 1
      CALL CQINI ('WRIT', JBUF2, IDISK, JCNO, IVER, CATBLK, JLUN2,
     *   ICQRNO, CQKOLS, CQNUMV, NOIFCQ, IERR)
      IF (IERR.NE.0) THEN
         CALL TABERR ('WRIT', 'CQINI', 'CQMAKE', IERR)
         IRET = 5
         GO TO 999
         END IF
C                                       Get no. records in FQ table
      NREC = JBUF1(5)
C                                       While NOT EOF(FQ_table) do
      DO 250 IROW = 1,NREC
         IFQRNO = IROW
         CALL TABFQ ('READ', JBUF1, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *      IFQID, DFOFF, FINC, BANDW, IFSIDE, LTAPCQ, IERR)
         IF (IERR.NE.0) THEN
            CALL TABERR ('READ', 'TABFQ', 'CQMAKE', IERR)
            IRET = 6
            GO TO 999
            END IF
C                                       Loop over all subarrays
         DO 200 I = 1,NSUBA
C                                       Fill CQ record
            IFQDCQ = IFQID
            ISUBCQ = I
            DO 150 JIF = 1,NOIFCQ
               NFFTCQ(JIF) = NFFT
               NCHCQ(JIF) = NCHAN
               IF (NCHAN.GT.0) THEN
                  NSAVCQ(JIF) = NFFT / (2.0 * NCHAN)
               ELSE
                  NSAVCQ(JIF) = -1
                  END IF
               DFRQCQ(JIF) = FREQ + DFOFF(JIF)
               DCBWCQ(JIF) = FINC(JIF)
               LTAPCQ(JIF) = LTAPER
               NOVSCQ(JIF) = NOVS
               NZPDCQ(JIF) = NZPD
               IFLTCQ(JIF) = -1
               TAVGCQ(JIF) = 0.0
               NBITCQ(JIF) = 0
               IOVLCQ(JIF) = 0
150            CONTINUE
C                                       Write CQ record
            CALL TABCQ ('WRIT', JBUF2, ICQRNO, CQKOLS, CQNUMV, NOIFCQ,
     *         IFQDCQ, ISUBCQ, NFFTCQ, NCHCQ, NSAVCQ, DFRQCQ, DCBWCQ,
     *         LTAPCQ, NOVSCQ, NZPDCQ, IFLTCQ, TAVGCQ, NBITCQ, IOVLCQ,
     *         IERR)
            IF (IERR.NE.0) THEN
               CALL TABERR ('WRIT', 'TABCQ', 'CQMAKE', IERR)
               IRET = 7
               GO TO 999
               END IF
200         CONTINUE
250      CONTINUE
C                                       Hard close FQ, CQ tables
      CALL TABIO ('CLOS', 0, IFQRNO, JBUF1, JBUF1, IRET)
      CALL TABIO ('CLOS', 0, ICQRNO, JBUF2, JBUF2, IRET)
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1000  FORMAT ('CQMAKE: ERROR',I4,' FROM UVPGET')
1070  FORMAT ('CQMAKE: NOT ENOUGH INFORMATION TO CREATE CQ TABLE')
1080  FORMAT ('CQMAKE: ERROR',I4,' DELETING OLD CQ TABLE')
      END
