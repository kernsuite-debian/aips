      SUBROUTINE GSCRED (MXSTAR, IVOL, CNO, VERS, NSTAR, GSCIDS, RADEGS,
     *   DECDGS, MAGS, IERR)
C-----------------------------------------------------------------------
C! reads a UK (guide star table) and fills arrays of coordinates
C# Table-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   GSCRED reads the star positions in a UK table which is assumed to be
C   a Guide star catalog table. The UK file contains the center
C   position (RA-DEC (J2000) position error, magnitude,  and other info
C   for several million stars.
C   Inputs:
C      MXSTAR  I         Maximum number of stars to read from table
C      VERS    I         Desired UK file version number: 0 => high
C      IVOL    I         File disk number
C      CNO     I         File catalog number
C   Output:
C      NSTAR   I         Number of stars read from UK table
C      GSCIDS  R(MXSTAR) Guide Star ID number for each star
C      RADEGS  R(MXSTAR) Guide Star RA in DEG (J2000) for each star
C      DECDGS  R(MXSTAR) Guide Star DEC in DEG (J2000) for each star
C      MAGS    R(MXSTAR) Guide Star Magnitude for each star
C      IERR    I         Error code: 0 => okay
C                                -1 => there was no UK file
C                                +1 => logical error in UK file
C                                +2 => IO error in UK file
C                                +3 => IO error in plotting
C   Common:
C      /MAPHDR/ CATBLK input   Image header having the UK file
C-----------------------------------------------------------------------
      INTEGER   MXSTAR, VERS, IVOL, CNO, NSTAR, IERR
      REAL      GSCIDS(*), RADEGS(*), DECDGS(*), MAGS(*)
C                                       Max Numb Columns, Label length
      INTEGER MXUKCL
      PARAMETER (MXUKCL=20)
      CHARACTER KEYS(MXUKCL)*8
      HOLLERITH CATH(256)
      INTEGER   IOBUF(768), CATBLK(256), IV, I, NKEY, NCOL, NREC, NUK,
     *   TABLUN, KOLS(MXUKCL), IER, ITYPE, SCRTCH(50), IUK, GSCID, LRNO
      REAL      CATR(256), RADEG, DECDEG, POSERR, MAG
      LOGICAL   LORDER
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATH, CATR)
      DATA TABLUN /27/
      DATA LORDER /.TRUE./
C-----------------------------------------------------------------------
C                                       Is there an UK file
      IERR = -1
      CALL FNDEXT ('UK', CATBLK, I)
      IF (I.LE.0) GO TO 999
      IV = VERS
      IF ((IV.LE.0) .OR. (IV.GT.I)) IV = I
C                                       Open UK table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      CALL TABINI ('READ', 'UK', IVOL, CNO, IV, CATBLK, TABLUN, NKEY,
     *   NREC, NCOL, IOBUF(513), IOBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, IV
         GO TO 990
         END IF
C                                       Read no more than fit in array
      NUK = MIN (IOBUF(5), MXSTAR)
C                                       Find needed columns
      KEYS(1) = 'GSC_ID'
      KEYS(2) = 'RA_DEG'
      KEYS(3) = 'DEC_DEG'
      KEYS(4) = 'POS_ERR'
      KEYS(5) = 'MAG'
      NCOL    = 5
      CALL FNDCOL (NCOL, KEYS, 8, LORDER, IOBUF, KOLS, IER)
      IF (IER.GT.0) THEN
         IF (IER.LE.10) GO TO 975
            IERR = 1
            MSGTXT = '!!!  CAN NOT FIND COLUMNS IN TABLE  !!!'
            CALL MSGWRT(5)
            MSGTXT = '??? Is this a Guide Star (UK) Table ???'
            GO TO 980
         END IF
C                                       I guess we can do it now
      NSTAR = 0
C                                       For all rows in the table
      LRNO = 0
      DO 100 IUK = 1,NUK
         CALL GETCOL (IUK, KOLS(1), IOBUF(513), LRNO, IOBUF, ITYPE,
     *      GSCID, SCRTCH, IER)
         IF (IER.LT.0) GO TO 100
         IF (IER.NE.0) GO TO 975
         CALL GETCOL (IUK, KOLS(2), IOBUF(513), LRNO, IOBUF, ITYPE,
     *      RADEG, SCRTCH, IER)
         IF (IER.NE.0) GO TO 975
         CALL GETCOL (IUK, KOLS(3), IOBUF(513), LRNO, IOBUF, ITYPE,
     *      DECDEG, SCRTCH, IER)
         IF (IER.NE.0) GO TO 975
         CALL GETCOL (IUK, KOLS(4), IOBUF(513), LRNO, IOBUF, ITYPE,
     *      POSERR, SCRTCH, IER)
         IF (IER.NE.0) GO TO 975
         IF (NCOL.GT.4) THEN
            CALL GETCOL (IUK, KOLS(5), IOBUF(513), LRNO, IOBUF, ITYPE,
     *         MAG, SCRTCH, IER)
            IF (IER.NE.0) GO TO 975
             END IF
         NSTAR = NSTAR + 1
         GSCIDS(NSTAR) = GSCID
         RADEGS(NSTAR) = RADEG
         DECDGS(NSTAR) = DECDEG
         MAGS(NSTAR)   = MAG
 100     CONTINUE
      IERR = 0
      WRITE (MSGTXT,1100) NSTAR, IV
      CALL MSGWRT (2)
      GO TO 985
C                                       Error print and close
 975  WRITE (MSGTXT,1975) IER
      IERR = 2
 980  CALL MSGWRT (8)
 985  CALL TABIO ('CLOS', 0, IUK, SCRTCH, IOBUF, IER)
      GO TO 999
C                                       Error print
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I6,' OPENING UK FILE VERSION',I4)
 1100 FORMAT ('Read ',I6,' Positions from UK table version',I4)
 1975 FORMAT ('I/O ERROR',I5,' IN UK TABLE FILE')
      END
