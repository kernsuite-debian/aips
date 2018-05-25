      SUBROUTINE DSMEAR (IUDISK, IUCNO, CATUV, BUFF, JLUN, FRQSEL,
     *   FINC, LTAPER, NSPECA, DELBIT, NFFTSZ, NETVLB, ITYPTF, TAVGTF,
     *   WDODSM, WARN, NSIZ, NSIZ2, IRET)
C-----------------------------------------------------------------------
C! Read the VLBA delay smearing/segmentation loss parameters in CQ table
C# EXT-appl Calibration UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2006-2007, 2011
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
C   Read CQ table information for VLBA delay decorrelation corrections
C    Input:
C      IUDISK   I    Disk number of uv-data file
C      IUCNO    I    Catalog number of uv-data file
C      CATUV    I(256)   Catalog header
C      BUFF     I(512)
C      JLUN         I    Logical unit number for table I/O
C      FRQSEL       I    FQ_ID selection
C      FINC         R(*) Channel bandwidth per IF for this FQ_ID
C      NSIZ         I    Dimension of LTAPER, NSPECA, DELBIT, NFFTSZ,
C                        ITYPTF, TAVGTF.
C      NSIZ2        I    Dimension of WARN
C    Output: (all indexed on IF and subarray)
C      LTAPER       C(*) Taper function (eg. 'HANNING')
C      NSPECA       I(*) Spectral averaging factor
C      DELBIT       D(*) Time interval per bit in FFT (s)
C      NFFTSZ       I(*) FFT size
C      ITYPTF       I(*) Time filter type (0=>boxcar; 3=>32-4;
C                        4=>64-8)
C      TAVGTF       R(*) Time filter averaging time (sec)
C      WDODSM       L(*) VLBA delay decor. correction enabled ?
C            (indexed on subarray only)
C      NETVLB       I(*) Subarray correlator type (1=VLBA;
C                        else non-VLBA)
C      WARN         L(*) Print warning message about missing CQ
C                        information for this (IF,subarray)
C    Output:
C      IRET         I    Return code (0=> ok)
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IUDISK, IUCNO, CATUV(256), JLUN, FRQSEL, NSIZ, NSIZ2,
     *   IRET
      LOGICAL   WDODSM(NSIZ), WARN(NSIZ2)
      CHARACTER LTAPER(NSIZ)*8
      DOUBLE PRECISION DELBIT(NSIZ)
      REAL      FINC(MAXIF), TAVGTF(NSIZ)
      INTEGER   BUFF(512), NSPECA(NSIZ), NFFTSZ(NSIZ), ITYPTF(NSIZ),
     *   NETVLB(NSIZ2)
C
      INCLUDE 'INCS:PCQV.INC'
      LOGICAL   WVLBA, ANYVLB
      CHARACTER LTAB*2, LANAME*8, LTAPCQ(MAXIF)*8, LKEY*8
      DOUBLE PRECISION DFRQCQ(MAXIF), DCBWCQ(MAXIF), DFACT
      REAL      TAVGCQ(MAXIF)
      INTEGER   NSUBA, I, MSGSAV, IVER, NKEY, NREC, NCOL, DATP(128,2),
     *   IERR, JKLOCS, KEYVAL(2), KEYTYP, IPOINT, ICQRNO,
     *   CQKOLS(MAXCQC), CQNUMV(MAXCQC), NOIFCQ, IFQDCQ, ISUBCQ,
     *   NFFTCQ(MAXIF), NCHCQ(MAXIF), NSAVCQ(MAXIF), NOVSCQ(MAXIF),
     *   NZPDCQ(MAXIF), NBITCQ(MAXIF), IFLTCQ(MAXIF), IOVLCQ(MAXIF),
     *   INDX, JIF, JFACT, ISUB, ICORR, IFILT, ID
      INCLUDE 'INCS:DMSG.INC'
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
      ANYVLB = .FALSE.
C
      DO 40 I = 1,NSIZ
         LTAPER(I) = ' '
         TAVGTF(I) = 0.0
         NSPECA(I) = 0
         DELBIT(I) = 0.0D0
         NFFTSZ(I) = 0
         ITYPTF(I) = 0
         WDODSM(I) = .FALSE.
 40      CONTINUE
C
      DO 45 I = 1,NSIZ2
         NETVLB(I) = 0
         WARN(I) = .FALSE.
 45      CONTINUE
C                                       Determine which subarrays
C                                       contain VLBA data
      CALL FNDEXT ('AN', CATUV, NSUBA)
C                                       Suppress error messages
      MSGSAV = MSGSUP
      MSGSUP = 32000
C                                       Loop over each subarray
      DO 50 I = 1,NSUBA
         LTAB = 'AN'
         IVER = I
         LANAME = ' '
         CALL TABINI ('READ', LTAB, IUDISK, IUCNO, IVER, CATUV, JLUN,
     *      NKEY, NREC, NCOL, DATP, BUFF, IERR)
         IF (IERR.EQ.0) THEN
C                                       Extract ARRAY_NAME keyword
            NKEY = 1
            LKEY = 'ARRNAM'
            CALL TABKEY ('READ', LKEY, NKEY, BUFF, JKLOCS, KEYVAL,
     *         KEYTYP, IERR)
            IF (IERR.EQ.0) THEN
               IPOINT = JKLOCS
               IF (IPOINT.EQ.1) CALL H2CHR (8, 1, KEYVAL, LANAME)
               END IF
            END IF
C                                       Set flags for VLBA
         WVLBA = (LANAME.EQ.'VLBA')
         IF (WVLBA) THEN
            NETVLB(I) = 1
            ANYVLB = .TRUE.
            END IF
         WARN(I) = WVLBA
C                                       Close AN table
         CALL TABIO ('CLOS', 0, 0, BUFF, BUFF, IERR)
 50      CONTINUE
C                                       Read CQ table information
      IVER = 1
      CALL CQINI ('READ', BUFF, IUDISK, IUCNO, IVER, CATUV, JLUN,
     *   ICQRNO, CQKOLS, CQNUMV, NOIFCQ, IERR)
      IF (IERR.EQ.0) THEN
         NREC = BUFF(5)
         DO 200 I = 1,NREC
C                                       Read CQ table record
            CALL TABCQ ('READ', BUFF, ICQRNO, CQKOLS, CQNUMV, NOIFCQ,
     *         IFQDCQ, ISUBCQ, NFFTCQ, NCHCQ, NSAVCQ, DFRQCQ, DCBWCQ,
     *         LTAPCQ, NOVSCQ, NZPDCQ, IFLTCQ, TAVGCQ, NBITCQ,
     *         IOVLCQ, IERR)
            IF (IERR.NE.0) THEN
               GO TO 200
               END IF
C                                       Select on FQ_ID
            IF ((IFQDCQ.NE.FRQSEL) .AND. (FRQSEL.GT.0) .AND.
     *         (IFQDCQ.GT.0)) GO TO 200
C                                       Fill output arrays
            ISUB = MAX (ISUBCQ, 1)
            DO 150 JIF = 1,NOIFCQ
C                                       Extract correlation id.
               ID = MAX (IFLTCQ(JIF), 0)
               IFILT = MOD (ID, 256)
               ICORR = (ID - IFILT) / 256 + 1.1
               INDX = (ICORR - 1) * MAXIF + JIF
C                                       Taper function
               LTAPER(INDX) = LTAPCQ(JIF)
C                                       Spectral avg. function
               NSPECA(INDX) = NSAVCQ(JIF)
C                                       Scale by current channel
C                                       bandwidth in case data have
C                                       been averaged since correlation
               IF (ABS (DCBWCQ(JIF)).GT.0.0) THEN
                  JFACT = INT (ABS (FINC(JIF) / DCBWCQ(JIF)) + 0.1)
               ELSE
                  JFACT = 1
                  END IF
               NSPECA(INDX) = NSPECA(INDX) * JFACT
C                                       FFT size
               NFFTSZ(INDX) = NFFTCQ(JIF)
C                                       Time interval per bit
               IF ((ABS(DCBWCQ(JIF)).GT.0.0).AND.(NCHCQ(JIF).GT.0))THEN
                  DELBIT(INDX) = 1.0D0 / (2 * DCBWCQ(JIF) * NCHCQ(JIF))
               ELSE
                  DELBIT(INDX) = -1.0D0
                  END IF
C                                       Filter type
               ITYPTF(INDX) = IFILT
C                                       Filter averaging time
               TAVGTF(INDX) = TAVGCQ(JIF)
C                                       Can delay decorrelation
C                                       corrections be done for this
C                                       (IF,corr_id) combination ?
C                                       Are taper function and FFT
C                                       size valid ?
               CALL FXSEG (LTAPER(INDX), NFFTSZ(INDX), 0.0D0,
     *            DFACT, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1010)
                  CALL MSGWRT (8)
                  WRITE (MSGTXT,1020)
                  CALL MSGWRT (8)
                  END IF
C                                       Incorporate other conditions
               WDODSM(INDX) = ((NSPECA(INDX).GT.0).AND.
     *            (DELBIT(INDX).GT.0.0).AND.(IERR.EQ.0))
 150           CONTINUE
 200        CONTINUE
         END IF
C                                       Close CQ table
      CALL TABIO ('CLOS', 0, 0, BUFF, BUFF, IERR)
C                                       Exit
      MSGSUP = MSGSAV
 999  RETURN
C----------------------------------------------------------------------
1010  FORMAT ('FXSEG: Problem with calculating segmetation loss factor',
     *   ' using')
1020  FORMAT ('FXSEG: the CQ table.')
      END
