      SUBROUTINE TABCQ (OPCODE, BUFFER, ICQRNO, CQKOLS, CQNUMV, NOIFCQ,
     *   IFQDCQ, ISUBCQ, NFFTCQ, NCHCQ, NSAVCQ, DFRQCQ, DCBWCQ, LTAPCQ,
     *   NOVSCQ, NZPDCQ, IFLTCQ, TAVGCQ, NBITCQ, IOVLCQ, IRET)
C----------------------------------------------------------------------
C! I/O to correlator parameter frequency (CQ) tables
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2007, 2013
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
C   Does I/O to CQ tables, after setup by CQINI
C   Inputs:
C      OPCODE      C*4        Operation code:
C                             'READ' = read entry from table
C                             'WRIT' = write entry in table
C                             'CLOS' = close file, flush on write
C      BUFFER      I(512)     I/O buffer and related storage, also
C                             defines file if open. Should have been
C                             returned by CQINI.
C      ICQRNO      I          Next scan no. to read or write
C      CQKOLS      I(MAXCQC)  The column pointer array in order:
C                             FQ_ID, SUBARRAY, FFT_SIZE, NO_CHAN,
C                             SPEC_AVG, EDGE_FREQ, CHAN_BW, TAPER_FN,
C                             OVER_SAMPL, ZERO_PAD, TIME_FILT, TIME_AVG,
C                             NO_BITS, FFT_OVLP
C      CQNUMV      I(MAXCQC)  Element count in each column.
C      NOIFCQ      I          No of IFs in CQ table.
C   Input/output: (written to or read from CQ table)
C      IFQDCQ      I          Freq_id
C      ISUBCQ      I          Subarray number
C   Following fields are all indexed on IF number:
C      NFFTCQ      I(*)       FFT_size
C      NCHCQ       I(*)       No. of channels
C      NSAVCQ      I(*)       Spectral averaging factor
C      DFRQCQ      D(*)       Edge frequency (Hz)
C      DCBWCQ      D(*)       Channel bandwidth (Hz)
C      LTAPCQ      C*8(*)     Taper function
C      NOVSCQ      I(*)       Oversampling factor
C      NZPDCQ      I(*)       Zero-padding factor
C      IFLTCQ      I(*)       Filter type
C      TAVGCQ      R(*)       Time averaging interval (s)
C      NBITCQ      I(*)       Quantization (no. of bits)
C      IOVLCQ      I(*)       FFT overlap factor
C   Output:
C      IRET        I          Return error code, 0=>ok, else
C                             TABIO error
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCQV.INC'
      INTEGER NOIFCQ
      CHARACTER OPCODE*4, LTAPCQ(NOIFCQ)*8
      DOUBLE PRECISION DFRQCQ(NOIFCQ), DCBWCQ(NOIFCQ)
      REAL TAVGCQ(NOIFCQ)
      INTEGER BUFFER(512), ICQRNO, CQKOLS(MAXCQC), CQNUMV(MAXCQC),
     *   IFQDCQ, ISUBCQ, NFFTCQ(NOIFCQ), NCHCQ(NOIFCQ), NSAVCQ(NOIFCQ),
     *   NOVSCQ(NOIFCQ), NZPDCQ(NOIFCQ), IFLTCQ(NOIFCQ), NBITCQ(NOIFCQ),
     *   IOVLCQ(NOIFCQ), IRET
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      HOLLERITH HOLTMP(2)
      LOGICAL DOREAD, DOCLOS
      DOUBLE PRECISION RECD(MXSPCQ)
      REAL RECORD(MXSPCQ)
      INTEGER RECI(MXSPCQ), ITEMP(2), NUMIF, JIF
      EQUIVALENCE (RECORD, RECI, RECD), (HOLTMP, ITEMP)
C-----------------------------------------------------------------------
      NUMIF = CQNUMV(CQIFFT)
C                                       Check OPCODE
      DOREAD = (OPCODE.EQ.'READ')
      DOCLOS = (OPCODE.EQ.'CLOS')
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       If WRIT fill record
      IF (.NOT.DOREAD) THEN
C                                       FQ_ID
         RECI(CQKOLS(CQIFQD)) = IFQDCQ
C                                       Subarray
         RECI(CQKOLS(CQISUB)) = ISUBCQ
C                                       Loop over IF
         DO 100 JIF = 1, NUMIF
C                                       FFT size
            RECI(CQKOLS(CQIFFT)+JIF-1) = NFFTCQ(JIF)
C                                       No. channels
            RECI(CQKOLS(CQINCH)+JIF-1) = NCHCQ(JIF)
C                                       Spectral averaging
            RECI(CQKOLS(CQISAV)+JIF-1) = NSAVCQ(JIF)
C                                       Edge frequency
            CALL RCOPY (NWDPDP, DFRQCQ(JIF),
     *         RECD(CQKOLS(CQDFRQ)+JIF-1))
C                                       Channel bandwidth
            CALL RCOPY (NWDPDP, DCBWCQ(JIF),
     *         RECD(CQKOLS(CQDCBW)+JIF-1))
C                                       Weighting function
            CALL CHR2H (8, LTAPCQ(JIF), 1, HOLTMP)
            CALL COPY (2, ITEMP, RECI(CQKOLS(CQHTAP)+2*(JIF-1)))
C                                       Oversampling factor
            RECI(CQKOLS(CQIOVS)+JIF-1) = NOVSCQ(JIF)
C                                       Zero padding factor
            RECI(CQKOLS(CQIZPD)+JIF-1) = NZPDCQ(JIF)
C                                       Filter type
            RECI(CQKOLS(CQIFLT)+JIF-1) = IFLTCQ(JIF)
C                                       Time averaging interval
            RECORD(CQKOLS(CQRTAV)+JIF-1) = TAVGCQ(JIF)
C                                       Quantization
            RECI(CQKOLS(CQIBIT)+JIF-1) = NBITCQ(JIF)
C                                       FFT overlap factor
            RECI(CQKOLS(CQIOVL)+JIF-1) = IOVLCQ(JIF)
100         CONTINUE
         END IF
C                                       Process record
220   CALL TABIO (OPCODE, 0, ICQRNO, RECORD, BUFFER, IRET)
      ICQRNO = ICQRNO + 1
      IF (IRET.GT.0) THEN
         CALL TABERR (OPCODE, 'TABIO', 'TABCQ', IRET)
         GO TO 999
         END IF
C                                       If READ pick data from
C                                       record.
      IF (DOREAD) THEN
C                                       FQ_id
         IFQDCQ = RECI(CQKOLS(CQIFQD))
C                                       Subarray
         ISUBCQ = RECI(CQKOLS(CQISUB))
C
         DO 300 JIF = 1, NUMIF
C                                       FFT size
            NFFTCQ(JIF) = RECI(CQKOLS(CQIFFT)+JIF-1)
C                                       No. channels
            NCHCQ(JIF) = RECI(CQKOLS(CQINCH)+JIF-1)
C                                       Spectral averaging factor
            NSAVCQ(JIF) = RECI(CQKOLS(CQISAV)+JIF-1)
C                                       Edge frequency
            CALL RCOPY (NWDPDP, RECD(CQKOLS(CQDFRQ)+JIF-1),
     *         DFRQCQ(JIF))
C                                       Channel bandwidth
            CALL RCOPY (NWDPDP, RECD(CQKOLS(CQDCBW)+JIF-1),
     *         DCBWCQ(JIF))
C                                       Weighting function
            CALL COPY (2, RECI(CQKOLS(CQHTAP)+2*(JIF-1)), ITEMP)
            CALL H2CHR (8, 1, HOLTMP, LTAPCQ(JIF))
C                                       Oversampling factor
            NOVSCQ(JIF) = RECI(CQKOLS(CQIOVS)+JIF-1)
C                                       Zero padding factor
            NZPDCQ(JIF) = RECI(CQKOLS(CQIZPD)+JIF-1)
C                                       Filter type
            IFLTCQ(JIF) = RECI(CQKOLS(CQIFLT)+JIF-1)
C                                       Time averaging interval
            TAVGCQ(JIF) = RECORD(CQKOLS(CQRTAV)+JIF-1)
C                                       Quantization
            NBITCQ(JIF) = RECI(CQKOLS(CQIBIT)+JIF-1)
C                                       FFT overlap factor
            IOVLCQ(JIF) = RECI(CQKOLS(CQIOVL)+JIF-1)
300         CONTINUE
         END IF
         GO TO 999
C                                       Close
800   CALL TABIO ('CLOS', 0, ICQRNO, RECORD, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'TABCQ', IRET)
         GO TO 999
         END IF
      GO TO 999
C                                       Exit
999   RETURN
      END
